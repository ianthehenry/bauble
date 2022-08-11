(def- unit-vec3 [1 1 1])
(def- zero-vec3 [0 0 0])
(def- identity-matrix-3 [1 0 0 0 1 0 0 0 1])

(defn- float [n]
  (if (int? n) (string n ".0") (string n)))

(defn- vec3 [[x y z]]
  (string/format `vec3(%s, %s, %s)` (float x) (float y) (float z)))

(defn- vec2 [[x y]]
  (string/format `vec2(%s, %s)` (float x) (float y)))

(defn- other-axes [axis]
  (case axis
    :x [:y :z]
    :y [:x :z]
    :z [:x :y]
    (error "unknown axis")))

(defn- string-of-axis [axis]
  (case axis
    :x "x"
    :y "y"
    :z "z"
    (error "unknown axis")))

(defn- string-of-axes [args]
  (def result (buffer/new (length args)))
  (each axis args
    (buffer/push-string result (string-of-axis axis)))
  result)

(defn- split-signed-axis [arg]
  (case arg
    :-x [-1 :x] :x [1 :x] :+x [1 :x]
    :-y [-1 :y] :y [1 :y] :+y [1 :y]
    :-z [-1 :z] :z [1 :z] :+z [1 :z]
    (error "unknown signed axis")))

(defn- mat3 [m]
  (string/format "mat3(%s, %s, %s, %s, %s, %s, %s, %s, %s)"
    (float (m 0)) (float (m 1)) (float (m 2))
    (float (m 3)) (float (m 4)) (float (m 5))
    (float (m 6)) (float (m 7)) (float (m 8))))

(defmacro- defconstructor [name proto args & body]
  (let [$proto (gensym)]
    ~(def ,name
      (let [,$proto ,proto]
        (fn ,args (table/setproto (do ,;body) ,$proto))))))

(defmacro- defconstructor- [name proto args & body]
  (let [$proto (gensym)]
    ~(def- ,name
      (let [,$proto ,proto]
        (fn ,args (table/setproto (do ,;body) ,$proto))))))

(defmacro- defcompiler [name self compile-body args & body]
  ~(defconstructor ,name
    @{:compile (fn [,self comp-state coord] ,compile-body)}
    ,args
    ,;body))

(defcompiler translate
  {:offset offset :expr expr} (:compile expr comp-state (string/format "(%s - %s)" coord (vec3 offset)))
  [offset expr] @{:offset offset :expr expr})

(defcompiler offset
  {:amount amount :expr expr}
  (string/format "(%s - %s)" (:compile expr comp-state coord) (float amount))
  [amount expr] @{:amount amount :expr expr})

(defcompiler onion
  {:thickness thickness :expr expr}
  (string/format "(abs(%s) - %s)" (:compile expr comp-state coord) (float thickness))
  [thickness expr] @{:thickness thickness :expr expr})

(defcompiler scale
  {:amount amount :expr expr}
  (string/format "(%s * %s)"
    (:compile expr comp-state (string/format "(%s / %s)" coord (float amount)))
    (float amount))
  [amount expr] @{:amount amount :expr expr})

(defcompiler box
  {:size size :center center}
  (:function comp-state "float" :box "s3d_box"
    [coord (vec3 center) (vec3 size)]
    ["vec3 p" "vec3 center" "vec3 size"] `
    vec3 q = abs(p - center) - size;
    return length(max(q, 0.0)) + min(max(q.x, max(q.y, q.z)), 0.0);
    `)
  [size &opt center]
  (default center zero-vec3)
  @{:size (if (number? size) [size size size] size)
    :center center})

(defcompiler cylinder
  {:radius radius :height height :axis axis}
  (:function comp-state "float" [:cylinder axis] "s3d_cylinder"
    [coord (float radius) (float height)]
    ["vec3 p" "float radius" "float height"]
    (string/format `
    vec2 d = abs(vec2(length(p.%s), p.%s)) - vec2(radius, height);
    return min(max(d.x, d.y), 0.0) + length(max(d, 0.0));
    ` (string-of-axes (other-axes axis)) (string-of-axis axis)))
  [axis radius height]
  @{:axis axis :radius radius :height height})

(defcompiler cone
  {:axis axis :radius radius :height height :upside-down upside-down}
  (:function comp-state "float" [:cone axis] "s3d_cone"
    [coord (float radius) (float height)]
    ["vec3 p" "float radius" "float height"] 
    (string/format `
    vec2 q = vec2(radius, height);
    vec2 w = vec2(length(p.%s), %sp.%s);
    vec2 a = w - q * clamp(dot(w, q) / dot(q, q), 0.0, 1.0);
    vec2 b = w - q * vec2(clamp(w.x / q.x, 0.0, 1.0), 1.0);
    float k = sign(q.y);
    float d = min(dot(a, a), dot(b, b));
    float s = max(k * (w.x * q.y - w.y * q.x), k * (w.y - q.y));
    return sqrt(d) * sign(s);
    `
    (string-of-axes (other-axes axis))
    (if upside-down "" "height - ")
    (string-of-axis axis)))
  [signed-axis radius height]
  (let [[sign axis] (split-signed-axis signed-axis)]
    @{:axis axis
      :radius radius
      :height (* sign (math/abs height))
      :upside-down (neg? height)}))

(defcompiler sphere
  {:radius radius :center center}
  (:function comp-state "float" :sphere "s3d_sphere"
    [coord (vec3 center) (float radius)]
    ["vec3 p" "vec3 center" "float radius"] `
    return length(p - center) - radius;
    `)
  [&opt radius center]
  (default radius 1)
  (default center zero-vec3)
  @{:radius radius :center center})

(defcompiler half-space
  {:axis axis :sign sign} (string (if (neg? sign) "" "-") coord "." (string-of-axes [axis]))
  [signed-axis]
  (let [[sign axis] (split-signed-axis signed-axis)]
    @{:axis axis :sign sign}))

(defcompiler line
  {:radius radius :start start :end end}
  (:function comp-state "float" :line "s3d_line"
    [coord (vec3 start) (vec3 end) (float radius)]
    ["vec3 p" "vec3 a" "vec3 b" "float r"] `
    vec3 pa = p - a, ba = b - a;
    float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0);
    return length(pa - ba * h) - r;
    `)
  [start end &opt radius]
  (default radius 0)
  @{:start start :end end :radius radius})

(defn- rotate-x-matrix [angle]
  (let [c (math/cos angle)
        s (math/sin angle)]
    [1 0 0
     0 c (- s)
     0 s c]))

(defn- rotate-y-matrix [angle]
  (let [c (math/cos angle)
        s (math/sin angle)]
    [c 0 s
     0 1 0
     (- s) 0 c]))

(defn- rotate-z-matrix [angle]
  (let [c (math/cos angle)
        s (math/sin angle)]
    [c (- s) 0
     s c 0
     0 0 1]))

(defconstructor- transform
  @{:compile (fn [{:matrix matrix :expr expr} comp-state coord]
    (:compile expr comp-state (string/format "(%s * %s)" coord (mat3 matrix))))}
  [matrix expr] @{:matrix matrix :expr expr})

# our matrices are just flat tuples. we could do this in C but whatever.
(defn- matrix-multiply-3 [a b]
  (def [a11 a12 a13 a21 a22 a23 a31 a32 a33] a)
  (def [b11 b12 b13 b21 b22 b23 b31 b32 b33] b)
  [
  (+ (* a11 b11) (* a12 b21) (* a13 b31))
  (+ (* a11 b12) (* a12 b22) (* a13 b32))
  (+ (* a11 b13) (* a12 b23) (* a13 b33))
  (+ (* a21 b11) (* a22 b21) (* a23 b31))
  (+ (* a21 b12) (* a22 b22) (* a23 b32))
  (+ (* a21 b13) (* a22 b23) (* a23 b33))
  (+ (* a31 b11) (* a32 b21) (* a33 b31))
  (+ (* a31 b12) (* a32 b22) (* a33 b32))
  (+ (* a31 b13) (* a32 b23) (* a33 b33))
  ])

(defn rotate [& args]
  (var axis nil)
  (var matrix nil)
  (var expr nil)

  (defn assert-number [x]
    (if (number? x)
      x
      (error "rotation must be a number")))

  (defn incorporate (new-matrix)
    (if (nil? matrix)
      (set matrix new-matrix)
      (set matrix (matrix-multiply-3 matrix new-matrix)))
    (set axis nil))

  (defn axis? [arg]
    (case arg
      :x true
      :y true
      :z true
      false))

  (each arg args
    (case axis
      :x (incorporate (rotate-x-matrix (assert-number arg)))
      :y (incorporate (rotate-y-matrix (assert-number arg)))
      :z (incorporate (rotate-z-matrix (assert-number arg)))
      (if (axis? arg)
        (set axis arg)
        (do
          (unless (nil? expr)
            (error "multiple expressions"))
          (set expr arg)))))

  (unless (nil? axis) (error "no angle for rotation axis"))
  (when (nil? matrix) (set matrix identity-matrix-3))
  (when (nil? expr) (error "nothing to rotate"))

  (transform matrix expr))

(defn- fold-exprs [base-name &named preamble fn-first fn-rest postamble return]
  (default preamble (fn [_] ""))
  (default postamble (fn [_] ""))
  (fn [self comp-state coord]
    (def exprs (self :exprs))
    (if (= 1 (length exprs))
      (:compile (first exprs) comp-state coord)
      (:sdf-3d comp-state self base-name coord (fn [coord]
        (string/join [
          (preamble self)
          (fn-first (:compile (first exprs) comp-state coord))
          ;(->> exprs (drop 1) (map |(:compile $ comp-state coord)) (map fn-rest))
          (postamble self)
          (string/format "return %s;" return)
        ] "\n"))))))

(defn- get-axes-and-expr [args]
  (var x false)
  (var y false)
  (var z false)
  (var expr nil)

  (each arg args
    (match arg
      :x (if x (error "duplicate axis") (set x true))
      :y (if y (error "duplicate axis") (set y true))
      :z (if z (error "duplicate axis") (set z true))
      (if expr (error "multiple expressions") (set expr arg))))

  (when (not (or x y z)) (error "no axis"))

  (def axes (buffer/new 3))
  (when x (buffer/push-string axes "x"))
  (when y (buffer/push-string axes "y"))
  (when z (buffer/push-string axes "z"))

  (when (nil? expr) (error "no expression"))

  [axes expr])

(defcompiler mirror
  self
  (let [{:expr expr :axes axes} self]
    (if (= 3 (length axes))
      (:compile expr comp-state (string/format "abs(%s)" coord))
      (:sdf-3d comp-state self "mirror" coord (fn [coord]
        (string/format "%s.%s = abs(%s.%s); return %s;" coord axes coord axes (:compile expr comp-state coord))))))
  [& args]
  (def [axes expr] (get-axes-and-expr args))
  @{:axes axes :expr expr})

(defn- transpose-other-axes [axis]
  (case axis
    :x "xzy"
    :y "zyx"
    :z "yxz"
    (error "unknown axis")))

(defn- negate-other-axes [axis]
  (case axis
    :x [1 -1 -1]
    :y [-1 1 -1]
    :z [-1 -1 1]
    (error "unknown axis")))

(defcompiler flip
  {:expr expr :axes axes :signs signs}
  (if (nil? signs)
    (:compile expr comp-state (string coord "." axes))
    (:compile expr comp-state (string/format "(%s.%s * %s)" coord axes (vec3 signs))))
  [signed-axis expr]
  (let [[sign axis] (split-signed-axis signed-axis)]
    @{:axes (transpose-other-axes axis)
      :expr expr
      :signs (if (neg? sign) (negate-other-axes axis) nil)}))

# this is a "full" symmetry that mirrors across every axis and rotation.
# you could have more flexible symmetry that only rotates across a single axis,
# does not do the abs(), etc. also... i'm not really sure how this works. this
# operation seems slightly insane.
(defcompiler symmetry
  {:expr expr}
  (:compile expr comp-state (string/format "sort3(abs(%s))" coord))
  [expr]
  @{:expr expr})

(defcompiler reflect
  self
  (:sdf-3d comp-state self "sym" coord (fn [coord]
    (def {:expr expr :axes axes} self)
    (string/format "%s.%s = -%s.%s; return %s;" coord axes coord axes (:compile expr comp-state coord))))
  [& args]
  (def [axes expr] (get-axes-and-expr args))
  @{:axes axes :expr expr})

(defconstructor union
  @{:compile (fold-exprs "union"
      :fn-first |(string/format "float d = %s;" $)
      :fn-rest |(string/format "d = min(d, %s);" $)
      :return "d")}
  [& exprs] @{:exprs exprs})

(defconstructor intersect
  @{:compile (fold-exprs "intersect"
      :fn-first |(string/format "float d = %s;" $)
      :fn-rest |(string/format "d = max(d, %s);" $)
      :return "d")}
  [& exprs] @{:exprs exprs})

(defconstructor subtract
  @{:compile (fold-exprs "subtract"
      :fn-first |(string/format "float d = %s;" $)
      :fn-rest |(string/format "d = max(d, -%s);" $)
      :return "d")}
  [& exprs] @{:exprs exprs})

(defconstructor smooth-union
  @{:compile (fold-exprs "smooth_union"
      :preamble |(string/format "float b, h = 0.0, k = %s;" (float ($ :size)))
      :fn-first |(string/format "float a = %s;" $)
      :fn-rest |(string/format "b = %s; h=clamp(0.5 + 0.5 * (b - a) / k, 0.0, 1.0); a = mix(b, a, h) - k * h * (1.0 - h);" $)
      :return "a")}
  [size & exprs] @{:size size :exprs exprs})

(defconstructor smooth-subtract
  @{:compile (fold-exprs "smooth_subtract"
      :preamble |(string/format "float b, h = 0.0, k = %s;" (float ($ :size)))
      :fn-first |(string/format "float a = %s;" $)
      :fn-rest |(string/format "b = %s; h=clamp(0.5 - 0.5 * (a + b) / k, 0.0, 1.0); a = mix(a, -b, h) + k * h * (1.0 - h);" $)
      :return "a")}
  [size & exprs] @{:size size :exprs exprs})

(defconstructor smooth-intersect
  @{:compile (fold-exprs "smooth_intersect"
      :preamble |(string/format "float b, h = 0.0, k = %s;" (float ($ :size)))
      :fn-first |(string/format "float a = %s;" $)
      :fn-rest |(string/format "b = %s; h=clamp(0.5 - 0.5 * (b - a) / k, 0.0, 1.0); a = mix(b, a, h) + k * h * (1.0 - h);" $)
      :return "a")}
  [size & exprs] @{:size size :exprs exprs})

# TODO: this has the somewhat fatal problem that the named arguments have to come at
# the *end* of the argument list. I need to make my own function thingy that's like
# aware of types and stuff, and lets me supply named arguments in arbitrary order.
(defcompiler tile
  self
  (let [{:offset offset :expr expr :limit limit} self
        offset (vec3 offset)]
    (if (nil? limit)
      (:compile expr comp-state
        (string/format "(mod(%s+0.5*%s,%s)-0.5*%s)" coord offset offset offset))
      (:function comp-state "float" self "tile" [coord offset (vec3 limit)]
        ["vec3 p" "vec3 offset" "vec3 limit"]
        (string
          "vec3 q = p - offset * clamp(round(p / offset), -limit, limit);"
          "return " (:compile expr comp-state "q") ";"))))
  [offset expr &named limit] @{:offset offset :expr expr :limit limit})

(defcompiler morph
  self
  (let [{:weight weight :expr1 expr1 :expr2 expr2} self]
    (:sdf-3d comp-state self "morph" coord (fn [coord]
      (string/format
        "float a = %s; float b = %s; return mix(a, b, %s);"
        (:compile expr1 comp-state coord)
        (:compile expr2 comp-state coord)
        (float weight)))))
  [weight expr1 expr2] @{:weight weight :expr1 expr1 :expr2 expr2})

(defconstructor flat-color
  @{:compile (fn [{:shape shape} comp-state coord]
      (:compile shape comp-state coord))
    :surface (fn [{:color color} comp-state coord]
      (vec3 color))}
  [color shape] @{:color color :shape shape})

(defconstructor blinn-phong
  @{:compile (fn [{:shape shape} comp-state coord]
      (:compile shape comp-state coord))
    :surface (fn [{:color color :shininess shininess :glossiness glossiness} comp-state coord]
      (:function comp-state "vec3" :blinn-phong "blinn_phong"
        [coord "camera" (vec3 color) (float shininess) (float glossiness)]
        ["vec3 p" "vec3 camera" "vec3 color" "float shininess" "float glossiness"]
        `
        vec3 normal = calculate_normal(p);
        vec3 light = vec3(256.0, 256.0, 0.0);
        vec3 light_color = vec3(1.0);

        vec3 light_dir = normalize(light - p);
        vec3 view_dir = normalize(camera - p);
        vec3 halfway_dir = normalize(light_dir + view_dir);

        float specular_strength = shininess * pow(max(dot(normal, halfway_dir), 0.0), pow(glossiness, 2.0));
        float diffuse = max(0.0, dot(normal, normalize(light - p)));
        float ambient = 0.2;

        if (diffuse + specular_strength > 0.0) {
          float light_brightness = cast_light(p + 2.0 * MINIMUM_HIT_DISTANCE * normal, light, 1024.0);
          vec3 specular_color = light_color * light_brightness * specular_strength;
          return color * (light_brightness * diffuse + ambient) + specular_color;
        } else {
          return color * ambient;
        }
        `))}
  [color shininess glossiness shape]
  @{:color color
    :shape shape
    :shininess shininess
    :glossiness glossiness})

(def- TAU (* 2 math/pi))

(defn tau [x] (* TAU x))

(defn tau/ [x] (/ TAU x))

(defn axis-vec [axis scale]
  (case axis
    :x [scale 0 0]
    :y [0 scale 0]
    :z [0 0 scale]
    (error "unknown axis")))

# TODO: this is a terrible name for this
(defmacro reflex [combine expr & fs]
  (let [$expr (gensym)
        combine (if (tuple? combine) combine [combine])
        transformed (map (fn [f] (if (tuple? f) [;f $expr] [f $expr])) fs)]
    ~(let [,$expr ,expr]
      (,;combine ,$expr ,;transformed))))
