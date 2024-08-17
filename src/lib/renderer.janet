(use judge)
(use ./util)
(import ../jlsl)
(import ../glsl)
(use ./dynvars)

(use ../jlsl/prelude)

(defmacro jlsl/do [& ast]
  (jlsl/expr/of-ast ['do ;ast]))

(test-macro

  (jlsl/do "default-2d-color"
    (def line-every 10)
    (def shadow-thickness 0.5)
    (def boundary-thickness 2)

    (var gradient-color (+ 0.5 (* 0.5 gradient)))

    (var inside (step (sign d) 0))
    (var isoline (smoothstep (- 1 shadow-thickness) 1 (/ (mod (abs d) line-every) line-every)))
    (var boundary-line (- 1 (smoothstep 0 (* 0.5 boundary-thickness) (abs d))))

    (mix
      (pow (vec3 gradient-color inside) (vec3 (mix 1 2 isoline)))
      (vec3 1)
      boundary-line))
  (@do-expr [(upscope (def <1> (@expr/literal (quote (<2> primitive (<3> float))) 10)) (def <4> (@expr/type <1>)) (def line-every (@new "line-every" <4>)) (@statement/declaration true line-every <1>)) (upscope (def <5> (@expr/literal (quote (<2> primitive (<3> float))) 0.5)) (def <6> (@expr/type <5>)) (def shadow-thickness (@new "shadow-thickness" <6>)) (@statement/declaration true shadow-thickness <5>)) (upscope (def <7> (@expr/literal (quote (<2> primitive (<3> float))) 2)) (def <8> (@expr/type <7>)) (def boundary-thickness (@new "boundary-thickness" <8>)) (@statement/declaration true boundary-thickness <7>)) (upscope (def <9> (@call + @[(@expr/literal (quote (<2> primitive (<3> float))) 0.5) (@call * @[(@expr/literal (quote (<2> primitive (<3> float))) 0.5) (@coerce-expr gradient)])])) (def <10> (@expr/type <9>)) (def gradient-color (@new "gradient-color" <10>)) (@statement/declaration false gradient-color <9>)) (upscope (def <11> (@call step @[(@call sign @[(@coerce-expr d)]) (@expr/literal (quote (<2> primitive (<3> float))) 0)])) (def <12> (@expr/type <11>)) (def inside (@new "inside" <12>)) (@statement/declaration false inside <11>)) (upscope (def <13> (@call smoothstep @[(@call - @[(@expr/literal (quote (<2> primitive (<3> float))) 1) (@coerce-expr shadow-thickness)]) (@expr/literal (quote (<2> primitive (<3> float))) 1) (@call / @[(@call mod @[(@call abs @[(@coerce-expr d)]) (@coerce-expr line-every)]) (@coerce-expr line-every)])])) (def <14> (@expr/type <13>)) (def isoline (@new "isoline" <14>)) (@statement/declaration false isoline <13>)) (upscope (def <15> (@call - @[(@expr/literal (quote (<2> primitive (<3> float))) 1) (@call smoothstep @[(@expr/literal (quote (<2> primitive (<3> float))) 0) (@call * @[(@expr/literal (quote (<2> primitive (<3> float))) 0.5) (@coerce-expr boundary-thickness)]) (@call abs @[(@coerce-expr d)])])])) (def <16> (@expr/type <15>)) (def boundary-line (@new "boundary-line" <16>)) (@statement/declaration false boundary-line <15>)) (@statement/expr (@call mix @[(@call pow @[(@call vec3 @[(@coerce-expr gradient-color) (@coerce-expr inside)]) (@call vec3 @[(@call mix @[(@expr/literal (quote (<2> primitive (<3> float))) 1) (@expr/literal (quote (<2> primitive (<3> float))) 2) (@coerce-expr isoline)])])]) (@call vec3 @[(@expr/literal (quote (<2> primitive (<3> float))) 1)]) (@coerce-expr boundary-line)]))] "default-2d-color"))

(def default-2d-color-expression
  (jlsl/do "default-2d-color"
    (def line-every 10)
    (def shadow-thickness 0.5)
    (def boundary-thickness 2)

    (var gradient-color (+ 0.5 (* 0.5 gradient)))

    (var inside (step (sign d) 0))
    (var isoline (smoothstep (- 1 shadow-thickness) 1 (/ (mod (abs d) line-every) line-every)))
    (var boundary-line (- 1 (smoothstep 0 (* 0.5 boundary-thickness) (abs d))))

    (mix
      (pow (vec3 gradient-color inside) (vec3 (mix 1 2 isoline)))
      (vec3 1)
      boundary-line)))

(jlsl/jlsl/defn :float circle [:float r]
  (- (length q) r))

(defn get-2d-program [subject]
  (def NORMAL_OFFSET 0.005)

  (jlsl/program/new
    (precision highp float)
    # just to be ignored...
    (uniform :vec3 camera-origin)
    (uniform :vec3 camera-orientation)
    (uniform :float t)
    (uniform :vec4 viewport)
    (out :vec4 frag-color)

    (in :vec4 gl_FragCoord)

    (defn :float nearest-distance []
      (return ,(@or (subject :distance) 0)))

    (defn :vec2 calculate-gradient []
      (def step (vec2 NORMAL_OFFSET 0))
      (return (normalize [
        (-
          (with [q (+ q (. step xy))] (nearest-distance))
          (with [q (- q (. step xy))] (nearest-distance)))

        (-
          (with [q (+ q (. step yx))] (nearest-distance))
          (with [q (- q (. step yx))] (nearest-distance)))
        ])))

    (defn :void main []
      (def gamma 2.2)

      (var resolution (. viewport zw))

      (var local-frag-coord (- (. gl_FragCoord xy) (. viewport xy)))
      (var relative-position (/ (- local-frag-coord (* resolution 0.5)) resolution))

      # TODO: 256 should vary by zoom amount
      (var local-coord (* relative-position 256))

      # TODO: with expression?
      (var color [0 0 0])
      (with [q local-coord
             Q q
             d (nearest-distance)
             gradient (calculate-gradient)]
        (set color ,(if-let [color-expression (subject :color)]
          # TODO: if expressions
          (jlsl/do "coloring"
            (var result [0 0 0])
            (if (< d 0)
              (set result ,color-expression))
            result)
          default-2d-color-expression)))

      (set frag-color (vec4 (pow color (vec3 (/ gamma))) 1)))
    ))

(defn render-2d [subject]
  (get-2d-program subject))

(defn render-3d [subject]
  (error "oh no"))

(defn render [env glsl-version]
  (def subject (get-var env 'subject))
  (unless subject
    (error "nothing to render"))

  # so our subject is either 2D or 3D
  (def glsl
    (jlsl/render/program
      (if true #(two-d? subject)
        (render-2d subject)
        (render-3d subject))))

  # TOOD: this is supposed to return whether or not t is a free variable,
  # which i don't really have a way to see...
  [false (glsl/render-program glsl glsl-version)]
  )

(test-stdout (print (glsl/render-program (jlsl/render/program (get-2d-program
  {:distance (circle 30)
    })))) `
  precision highp float;
  
  in vec4 gl_FragCoord;
  
  out vec4 frag_color;
  
  uniform vec3 camera_origin;
  uniform vec3 camera_orientation;
  uniform float t;
  uniform vec4 viewport;
  
  float circle(float r, vec2 q) {
    length(q) - r;
  }
  
  float nearest_distance(vec2 q) {
    return circle(30.0, q) || 0.0;
  }
  
  float with_outer(vec2 q, vec2 step) {
    {
      vec2 q1 = q + step.xy;
      return nearest_distance(q1);
    }
  }
  
  float with_outer1(vec2 q, vec2 step) {
    {
      vec2 q1 = q - step.xy;
      return nearest_distance(q1);
    }
  }
  
  float with_outer2(vec2 q, vec2 step) {
    {
      vec2 q1 = q + step.yx;
      return nearest_distance(q1);
    }
  }
  
  float with_outer3(vec2 q, vec2 step) {
    {
      vec2 q1 = q - step.yx;
      return nearest_distance(q1);
    }
  }
  
  vec2 calculate_gradient(vec2 q) {
    const vec2 step = vec2(0.005, 0.0);
    return normalize(vec2(with_outer(q, step) - with_outer1(q, step), with_outer2(q, step) - with_outer3(q, step)));
  }
  
  vec3 default_2d_color(vec2 gradient, float d) {
    const float line_every = 10.0;
    const float shadow_thickness = 0.5;
    const float boundary_thickness = 2.0;
    vec2 gradient_color = 0.5 + (0.5 * gradient);
    float inside = step(sign(d), 0.0);
    float isoline = smoothstep(1.0 - shadow_thickness, 1.0, mod(abs(d), line_every) / line_every);
    float boundary_line = 1.0 - smoothstep(0.0, 0.5 * boundary_thickness, abs(d));
    return mix(pow(vec3(gradient_color, inside), vec3(mix(1.0, 2.0, isoline))), vec3(1.0), boundary_line);
  }
  
  void main() {
    const float gamma = 2.2;
    vec2 resolution = viewport.zw;
    vec2 local_frag_coord = gl_FragCoord.xy - viewport.xy;
    vec2 relative_position = (local_frag_coord - (resolution * 0.5)) / resolution;
    vec2 local_coord = relative_position * 256.0;
    vec3 color = vec3(0.0, 0.0, 0.0);
    {
      vec2 q = local_coord;
      vec2 Q = q;
      float d = nearest_distance(q);
      vec2 gradient = calculate_gradient(q);
      color = default_2d_color(gradient, d);
    }
    frag_color = vec4(pow(color, vec3(1.0 / gamma)), 1.0);
  }
  
`)
