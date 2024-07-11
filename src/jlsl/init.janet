(use judge)
(use module)
(import pat)
(use ./util)

(defmodule printer
  (defn new []
    @{:indent 0
      :indented false
      :buf (buffer/new 256)})

  (defn indent [t]
    (repeat (t :indent)
      (buffer/push-string (t :buf) "  "))
    (put t :indented true))

  (defn newline [t]
    (buffer/push-byte (t :buf) (chr "\n"))
    (put t :indented false))

  (defn prin [t & things]
    (unless (t :indented) (indent t))
    (each thing things
      (buffer/push-string (t :buf) thing)))

  (defn print [t & things]
    (prin t ;things)
    (newline t))

  (defn to-string [t]
    (string (t :buf)))

  (defn indent-body* [t f]
    (++ (t :indent))
    (f)
    (-- (t :indent)))

  (defmacro indent-body [t & body]
    ~(,indent-body* ,t (fn [] ,;body)))

  (defmacro bracket-body [t start end & body]
    ~(do
      (,print ,t ,start)
      (,indent-body* ,t (fn [] ,;body))
      (,prin ,t ,end)))
  )

(deftest "printer works"
  (def p (printer/new))
  (printer/print p "hello")
  (printer/print p "start " "{")
  (printer/indent-body p
    (printer/print p "one")
    (printer/print p "two"))
  (printer/print p "}")
  (test-stdout (print (printer/to-string p)) `
    hello
    start {
      one
      two
    }
    
  `))

(deftest "bracketing works"
  (def p (printer/new))
  (printer/print p "hello")
  (printer/prin p "start")
  (printer/bracket-body p " {" "} "
    (printer/print p "one")
    (printer/print p "two"))
  (printer/print p "hello")
  (test-stdout (print (printer/to-string p)) `
    hello
    start {
      one
      two
    } hello
    
  `))

(defmacro- wrap-when [bool before & rest]
  (def after (last rest))
  (def during (drop -1 rest))
  (with-syms [$bool]
    ~(let [,$bool ,bool]
      (when ,$bool ,before)
      ,;during
      (when ,$bool ,after))))

(defn identifier [name]
  (string/replace "-" "_" name))

(var render-statement nil)
(var render-expression nil)

(defn to-float [num]
  (def str (string num))
  (if (string/find "." str) str (string str ".0")))

(varfn render-expression [p expression &named needs-parens?]
  (default needs-parens? false)
  (pat/match expression
    [(and op (or '++ '--)) expr] (do
      (printer/prin p op)
      (render-expression p expr :needs-parens? true))
    [(and op (or '_++ '_--)) expr] (do
      (render-expression p expr :needs-parens? true)
      (printer/prin p (string/slice op 1)))
    [(and op (or '- 'not 'bnot)) expr] (do
      (def op (case op
        'not "!"
        'bnot "~"
        op))
      (printer/prin p op)
      (render-expression p expr :needs-parens? true))
    ['/ expr] (render-expression p ~(/ 1 ,expr) :needs-parens? needs-parens?)
    [(and op (or '+ '/ '- '* '%
      'and 'or 'xor
      'blshift 'brshift 'band 'bor 'bxor
      '< '> '<= '>=
      '= 'not=)) & args] (do
      (def op (case op
        'blshift "<<"
        'brshift ">>"
        'band "&"
        'bor "|"
        'bxor "^"
        'and "&&"
        'or "||"
        'xor "^^"
        'not= "!="
        '= "=="
        op))
      (wrap-when needs-parens?
        (printer/prin p "(")
        (each-last arg args
          (render-expression p arg :needs-parens? true)
          (printer/prin p " " op " "))
        (printer/prin p ")")))
    [f & args] (do
      (printer/prin p (identifier f) "(")
      (each-last arg args
        (render-expression p arg)
        (printer/prin p ", "))
      (printer/prin p ")"))
    |symbol? (printer/prin p (identifier expression))
    |keyword? (printer/prin p expression)
    |number? (printer/prin p (to-float expression))
    ))

(varfn render-statement [p statement &named newline-after-block?]
  (default newline-after-block? true)
  (var semi true)
  (pat/match statement
    ['def type name value] (do
      (printer/prin p "const " type " " (identifier name) " = ")
      (render-expression p value))
    ['var type name] (do
      (printer/prin p type " " (identifier name)))
    ['var type name value] (do
      (printer/prin p type " " (identifier name) " = ")
      (render-expression p value))
    ['set name value] (do
      (printer/prin p (identifier name) " = ")
      (render-expression p value))
    ['return value] (do
      (printer/prin p "return ")
      (render-expression p value))
    ['break] (printer/prin p "break")
    ['continue] (printer/prin p "continue")
    [(and op (or
      '+= '*= '/= '-= '%=
      'blshift= 'brshift=
      'bxor= 'band= 'bor=)) name expr] (do
      (def op (case op
        'bxor= "^="
        'band= "&="
        'bor= "|="
        'brshift= ">>="
        'blshift= "<<="
        ))
      (printer/prin p (identifier name) " " op " ")
      (render-expression p expr))
    # TODO: += etc
    # TODO: regular function calls
    ['do & statements] (do
      (printer/bracket-body p "{" "}"
        (each statement statements
          (render-statement p statement)))
      (when newline-after-block? (printer/newline p))
      (set semi false))
    ['if cond then & else] (do
      (assert (<= (length else) 1) "too many arguments to if")
      (printer/prin p "if (")
      (render-expression p cond)
      (printer/prin p ") ")
      (render-statement p then :newline-after-block? (empty? else))
      (unless (empty? else)
        (printer/prin p " else ")
        (render-statement p (first else)))
      (set semi false))
    ['case value & cases] (do
      (printer/prin p "switch (")
      (render-expression p value)
      (printer/print p ") {")
      (each case (partition 2 cases)
        (def body (match (length case)
          1 (do (printer/prin p "default: ") (first case))
          2 (let [[value body] case]
            (printer/prin p "case ")
            (render-expression p value)
            (printer/prin p ": ")
            body)
          (error "impossible")))
        (render-statement p body))
      (printer/print p "}")
      (set semi false))
    (render-expression p statement)
    )
  (when semi (printer/print p ";")))

(defn format-args [args]
  (string ;(->
    (seq [arg :in (partition 2 args)]
      (string/join arg " "))
    (intercalate ", "))))

(test (format-args [:foo 'name :bar 'other]) "foo name, bar other")

(defn render-toplevel [p toplevel]
  (pat/match toplevel
    ['defn type name args & body] (do
      (printer/prin p type " " (identifier name) "(" (format-args args) ")")
      (printer/bracket-body p " {" "}"
        (each statement body
          (render-statement p statement)))
      (printer/newline p))
    ['def type name value] (do
      (printer/prin p "const " type " " (identifier name) " = ")
      (render-expression p value)
      (printer/print p ";"))
    [(and (or 'uniform 'out) decl) type name] (printer/print p decl " " type " " (identifier name) ";")
    ['precision & rest] (printer/print p "precision " ;(intercalate rest " ") ";")
    ))

(defn render-program [program]
  (def p (printer/new))
  (var last-head nil)
  (each toplevel program
    (def new-head (first toplevel))

    (when (and last-head
        (or (= new-head 'defn)
        (not= last-head new-head)))
      (printer/newline p))

    (render-toplevel p toplevel)

    (set last-head new-head))
  (printer/to-string p)
  )

(defmacro* test-program [program &opt _]
  ~(test-stdout (print (,render-program ',program))))

(defmacro* test-statements [statements &opt _]
  ~(test-stdout (print (,render-program ['(defn :void main [] ,;statements)]))))

(deftest "if formats correctly with and without else"
  (test-statements [
    (++ i)
    (_++ i)
    (-- i)
    (_-- i)
    ] `
    void main() {
      ++i;
      i++;
      --i;
      i--;
    }
    
  `))

(deftest "if formats correctly with and without else"
  (test-statements [
    (if x (set y 10))
    (set x 10)
    (if x (do (set y 10)))
    (set x 10)
    (if x (do (set y 10)) (set y 20))
    (set x 10)
    (if x (do (set y 10)) (do (set y 20)))
    (set x 10)
    ] `
    void main() {
      if (x) y = 10.0;
      x = 10.0;
      if (x) {
        y = 10.0;
      }
      x = 10.0;
      if (x) {
        y = 10.0;
      } else y = 20.0;
      x = 10.0;
      if (x) {
        y = 10.0;
      } else {
        y = 20.0;
      }
      x = 10.0;
    }
    
  `))

(deftest "switch indentation"
  (test-statements [
    (case x
      10 (do (f) (continue) (g))
      20 (h)
      30 (do (f) (g) (break))
      (do (h)))
    ] `
    void main() {
      switch (x) {
      case 10.0: {
        f();
        continue;
        g();
      }
      case 20.0: h();
      case 30.0: {
        f();
        g();
        break;
      }
      default: {
        h();
      }
      }
    }
    
  `))

(test-program [
  (precision highp float)
  (uniform :vec3 camera-origin)
  (uniform :vec3 camera-rotation)
  (uniform float t)
  (uniform vec4 viewport)
  (out vec4 frag_color)

  (def :int MAX_STEPS 64)
  (def :float MINIMUM_HIT_DISTANCE 0.1)
  (def :float NORMAL_OFFSET 0.005)
  (def :float MAXIMUM_TRACE_DISTANCE (* 64 1024))

  (def :float PI 3.14159265359)

  (defn :mat3 rotate-x [:float angle]
    (def :float s (sin angle))
    (def :float c (cos angle))
    (return (mat3
      1 0 0
      0 c (- s)
      0 s c)))

  (defn :float s3d-ellipsoid [:vec3 p :vec3 size]
    (def :float k0 (length (/ p size)))
    (def :float k1 (length (/ p (* size size))))
    (return (/ (* k0 (- k0 1)) k1)))

  (defn :void main []
    (def :float gamma 2.2)

    (def :vec2 local_coord (- gl_FragCoord.xy viewport.xy))
    (def :vec2 resolution viewport.zw)
    (def :vec3 dir (*
      (rotate_x camera_rotation.x)
      (rotate_y camera_rotation.y)
      (rotate_z camera_rotation.z)
      (perspective 27.0 resolution local_coord)))
    (def :vec3 hit (march camera-origin dir))
    (var :vec3 color)
    (def :float depth (distance camera-origin hit))
    (var :float alpha 1)
    (if (>= depth MAXIMUM_TRACE_DISTANCE) (do
      (def :vec3 light (pow (/ (vec3 69 72 79) (vec3 255)) (vec3 gamma)))
      (def :vec3 dark (pow (/ (vec3 40 42 46) (vec3 255)) (vec3 gamma)))
      (set color (vec3 (mix dark light (/ (+ local_coord.x local_coord.y) (+ resolution.x resolution.y))))))
      (do
        (set color (nearest-color hit))))

    (if (or (>= depth MAXIMUM_TRACE_DISTANCE) (< (+ hit.x hit.z) 10.0)) (do
      (def :float r (length (- (/ local-coord resolution) (vec2 0.5))))
      (if (> r 0.45) (do
        (set color (vec3 0.6 0.8 0.6))
        (set alpha 1)))
      (if (> r 0.47) (do
        (set alpha 0)))))
    (set frag-color (vec4 (pow color (vec3 (/ gamma))) alpha)))

  ] `
  precision highp float;
  
  uniform vec3 camera_origin;
  uniform vec3 camera_rotation;
  uniform float t;
  uniform vec4 viewport;
  
  out vec4 frag_color;
  
  const int MAX_STEPS = 64.0;
  const float MINIMUM_HIT_DISTANCE = 0.1;
  const float NORMAL_OFFSET = 0.005;
  const float MAXIMUM_TRACE_DISTANCE = 64.0 * 1024.0;
  const float PI = 3.14159265359;
  
  mat3 rotate_x(float angle) {
    const float s = sin(angle);
    const float c = cos(angle);
    return mat3(1.0, 0.0, 0.0, 0.0, c, -s, 0.0, s, c);
  }
  
  float s3d_ellipsoid(vec3 p, vec3 size) {
    const float k0 = length(p / size);
    const float k1 = length(p / (size * size));
    return (k0 * (k0 - 1.0)) / k1;
  }
  
  void main() {
    const float gamma = 2.2;
    const vec2 local_coord = gl_FragCoord.xy - viewport.xy;
    const vec2 resolution = viewport.zw;
    const vec3 dir = rotate_x(camera_rotation.x) * rotate_y(camera_rotation.y) * rotate_z(camera_rotation.z) * perspective(27.0, resolution, local_coord);
    const vec3 hit = march(camera_origin, dir);
    vec3 color;
    const float depth = distance(camera_origin, hit);
    float alpha = 1.0;
    if (depth >= MAXIMUM_TRACE_DISTANCE) {
      const vec3 light = pow(vec3(69.0, 72.0, 79.0) / vec3(255.0), vec3(gamma));
      const vec3 dark = pow(vec3(40.0, 42.0, 46.0) / vec3(255.0), vec3(gamma));
      color = vec3(mix(dark, light, (local_coord.x + local_coord.y) / (resolution.x + resolution.y)));
    } else {
      color = nearest_color(hit);
    }
    if ((depth >= MAXIMUM_TRACE_DISTANCE) || ((hit.x + hit.z) < 10.0)) {
      const float r = length((local_coord / resolution) - vec2(0.5));
      if (r > 0.45) {
        color = vec3(0.6, 0.8, 0.6);
        alpha = 1.0;
      }
      if (r > 0.47) {
        alpha = 0.0;
      }
    }
    frag_color = vec4(pow(color, vec3(1.0 / gamma)), alpha);
  }
  
`)

# #version 330
# precision highp float;
#
# uniform vec3 camera_origin;
# uniform vec3 camera_orientation;
# uniform float t;
# uniform int render_type;
# uniform vec4 viewport;
#
# out vec4 frag_color;
#
# const int MAX_STEPS = 256;
# const float MINIMUM_HIT_DISTANCE = 0.1;
# const float NORMAL_OFFSET = 0.005;
# const float MAXIMUM_TRACE_DISTANCE = 64.0 * 1024.0;
#
# const float PI = 3.14159265359;
#
# struct LightIncidence {
#   vec3 direction;
#   vec3 color;
# };
#
# float nearest_distance(vec3 p);
# float s3d_box(vec3 p, vec3 size) {
# vec3 q = abs(p) - size;
#  return length(max(q, 0.0)) + min(max(q.x, max(q.y, q.z)), 0.0);
#
# }
# LightIncidence cast_light_hard_shadow(vec3 p, vec3 normal, vec3 light_position, vec3 color, float brightness) {
#   vec3 target = p + MINIMUM_HIT_DISTANCE * normal;
#   float target_distance = distance(light_position, target);
#   if (target_distance == 0.0) {
#     return LightIncidence(vec3(0.0), color * brightness);
#   }
#   vec3 to_light = (light_position - target) / target_distance;
#   if (brightness == 0.0) {
#     return LightIncidence(to_light, vec3(0.0));
#   }
#   float progress = 0.0;
#   for (int i = 0; i < MAX_STEPS; i++) {
#     float distance = nearest_distance(light_position - to_light * progress);
#     if (distance < MINIMUM_HIT_DISTANCE) {
#       if (progress + distance >= target_distance - MINIMUM_HIT_DISTANCE) {
#         return LightIncidence(to_light, brightness * color);
#       } else {
#         return LightIncidence(to_light, vec3(0.0));
#       }
#     }
#     progress += distance;
#   }
#   return LightIncidence(to_light, vec3(0.0));
# }
# LightIncidence cast_light_soft_shadow(vec3 p, vec3 normal, vec3 light_position, vec3 color, float brightness, float softness) {
#   if (softness == 0.0) {
#     cast_light_hard_shadow(p, normal, light_position, color, brightness);
#   }
#   vec3 target = p + MINIMUM_HIT_DISTANCE * normal;
#   float target_distance = distance(light_position, target);
#   if (target_distance == 0.0) {
#     return LightIncidence(vec3(0.0), color * brightness);
#   }
#   vec3 to_light = (light_position - target) / target_distance;
#   if (brightness == 0.0) {
#     return LightIncidence(to_light, vec3(0.0));
#   }
#   float in_light = 1.0;
#   float sharpness = 1.0 / (softness * softness);
#   float last_distance = 1e20;
#   float progress = 0.0;
#   for (int i = 0; i < MAX_STEPS; i++) {
#     float distance = nearest_distance(light_position - to_light * progress);
#     if (distance < MINIMUM_HIT_DISTANCE) {
#       if (progress + distance >= target_distance - MINIMUM_HIT_DISTANCE) {
#         return LightIncidence(to_light, in_light * brightness * color);
#       } else {
#         return LightIncidence(to_light, vec3(0.0));
#       }
#     }
#
#     if (distance < last_distance) {
#       float intersect_offset = distance * distance / (2.0 * last_distance);
#       float intersect_distance = sqrt(distance * distance - intersect_offset * intersect_offset);
#       in_light = min(in_light, sharpness * intersect_distance / max(0.0, target_distance - progress - intersect_offset));
#     }
#     progress += distance;
#     last_distance = distance;
#   }
#   return LightIncidence(to_light, vec3(0.0));
# }
# mat3 rotate_x(float angle) {
#   float s = sin(angle);
#   float c = cos(angle);
#   return mat3(
#     1.0, 0.0, 0.0,
#     0.0, c, -s,
#     0.0, s, c);
# }
# mat3 rotate_y(float angle) {
#   float s = sin(angle);
#   float c = cos(angle);
#   return mat3(
#     c, 0.0, s,
#     0.0, 1.0, 0.0,
#     -s, 0.0, c);
# }
# mat3 rotate_z(float angle) {
#   float s = sin(angle);
#   float c = cos(angle);
#   return mat3(
#     c, -s, 0.0,
#     s, c, 0.0,
#     0.0, 0.0, 1.0);
# }float nearest_distance(vec3 p) {
#
#   return mix(s3d_box(p, vec3(100.0, 100.0, 100.0)), (length(p) - 100.0), 0.500000);
# }
#
# vec3 calculate_normal(vec3 p) {
#   const vec3 step = vec3(NORMAL_OFFSET, 0.0, 0.0);
#
#   return normalize(vec3(
#     nearest_distance(p + step.xyy) - nearest_distance(p - step.xyy),
#     nearest_distance(p + step.yxy) - nearest_distance(p - step.yxy),
#     nearest_distance(p + step.yyx) - nearest_distance(p - step.yyx)
#   ));
# }
#
# float calculate_occlusion(vec3 p, vec3 normal) {
#   const int step_count = 10;
#   const float max_distance = 10.0;
#   const float step_size = max_distance / float(step_count);
#   float baseline = nearest_distance(p);
#   float occlusion = 0.0;
#   // TODO: this does some good to reduce the problem where a "neck" will
#   // have band of completely unoccluded space, but it introduces some
#   // terrible banding artifacts on flat surfaces.
#   // vec3 sine_noise = sin(p * 43758.5453);
#   // vec3 rand = sign(sine_noise) * fract(sine_noise);
#   // vec3 step = normalize(normal + rand) * step_size;
#   vec3 step = normal * step_size;
#   for (int i = 1; i <= step_count; i++) {
#     float expected_distance = baseline + float(i) * step_size;
#     float actual_distance = max(nearest_distance(p + float(i) * step), 0.0);
#     occlusion += actual_distance / expected_distance;
#   }
#   occlusion /= float(step_count);
#   return clamp(occlusion, 0.0, 1.0);
# }
#
# vec3 march(vec3 ray_origin, vec3 ray_direction, out int steps) {
#   float distance = 0.0;
#
#   for (steps = 0; steps < MAX_STEPS; steps++) {
#     vec3 p = ray_origin + distance * ray_direction;
#
#     float nearest = nearest_distance(p);
#
#     // TODO: this attenuation only works when we're
#     // using march to render from the camera's point
#     // of view, so we can't use the march function
#     // as-is to render reflections. I don't know if
#     // it's worth having.
#     // if (nearest < distance * MINIMUM_HIT_DISTANCE * 0.01) {
#     if (nearest < MINIMUM_HIT_DISTANCE || distance > MAXIMUM_TRACE_DISTANCE) {
#       return p + nearest * ray_direction;
#     }
#
#     distance += nearest;
#   }
#   return ray_origin + distance * ray_direction;
# }
#
# vec3 nearest_color(vec3 p) {
#   vec3 normal = calculate_normal(p);
#   vec3 P = p;
#   LightIncidence lights1[2];
#   lights1[0] = LightIncidence(vec3(0.0), (vec3(1.0, 1.0, 1.0) * 0.050000));
#   lights1[1] = cast_light_soft_shadow(P, normal, (P + vec3(1024.0, 1024.0, 512.0)), vec3(1.0, 1.0, 1.0), 1.0, 0.250000);return mix((0.500000 * (1.0 + normal)), (0.500000 * (1.0 + normal)), 0.500000);
# }
#
# const float DEG_TO_RAD = PI / 180.0;
# vec3 perspective(float fov, vec2 size, vec2 pos) {
#   vec2 xy = pos - size * 0.5;
#
#   float cot_half_fov = tan((90.0 - fov * 0.5) * DEG_TO_RAD);
#   float z = min(size.x, size.y) * 0.5 * cot_half_fov;
#
#   return normalize(vec3(xy, -z));
# }
#
# mat3 rotation_matrix(vec3 rotation) {
#   return rotate_z(rotation.z) * rotate_y(rotation.y) * rotate_x(rotation.x);
# }
#
# void main() {
#   const float gamma = 2.2;
#
#   vec2 local_coord = gl_FragCoord.xy - viewport.xy;
#   vec2 resolution = viewport.zw;
#   vec3 dir = rotation_matrix(camera_orientation) * perspective(45.0, resolution, local_coord);
#
#   const vec3 fog_color = vec3(0.15);
#
#   int steps;
#   vec3 hit = march(camera_origin, dir, steps);
#
#   vec3 color;
#   switch (render_type) {
#     case 0: {
#       float depth = distance(camera_origin, hit);
#       if (depth >= MAXIMUM_TRACE_DISTANCE) {
#         const vec3 light = pow(vec3(69.0, 72.0, 79.0) / vec3(255.0), vec3(gamma));
#         const vec3 dark = pow(vec3(40.0, 42.0, 46.0) / vec3(255.0), vec3(gamma));
#         color = vec3(mix(dark, light, (local_coord.x + local_coord.y) / (resolution.x + resolution.y)));
#       } else {
#         color = nearest_color(hit);
#       }
#       break;
#     }
#     case 1: {
#       // convergence debugging
#       if (steps == MAX_STEPS) {
#         color = vec3(1.0, 0.0, 1.0);
#       } else {
#         color = vec3(float(steps) / float(MAX_STEPS));
#       }
#       break;
#     }
#     case 2: {
#       // overshoot debugging
#       float distance = nearest_distance(hit);
#       float overshoot = max(-distance, 0.0) / MINIMUM_HIT_DISTANCE;
#       float undershoot = max(distance, 0.0) / MINIMUM_HIT_DISTANCE;
#       color = vec3(overshoot, 1.0 - undershoot - overshoot, 1.0 - step(1.0, undershoot));
#       break;
#     }
#   }
#
#   frag_color = vec4(pow(color, vec3(1.0 / gamma)), 1.0);
# }
