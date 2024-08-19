(use judge)
(use ./util)
(import ../jlsl)
(import ../glsl)
(import ./field-set)
(use ./dynvars)

(use ../jlsl/prelude)

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

    (defn :vec3 sample [:vec2 frag-coord]
      (var resolution (. viewport zw))
      (var local-frag-coord (- frag-coord (. viewport xy)))
      (var relative-position (/ (- local-frag-coord (* resolution 0.5)) resolution))
      # TODO: 256 should vary by zoom amount
      (var local-coord (* relative-position 256))

      (with [q local-coord
             Q q
             d (nearest-distance)
             gradient (calculate-gradient)]
        (return ,(if-let [color-expression (subject :color)]
          # TODO: if expressions
          (jlsl/do "coloring"
            (if (< d 0)
              ,color-expression
              [0 0 0]))
          default-2d-color-expression))))

    (defn :void main []
      (def gamma 2.2)
      (var color [0 0 0])
      (def aa-samples :3)
      (def aa-sample-width (/ (float (+ :1 aa-samples))))
      (def pixel-origin [0.5 0.5])

      (for (var y :1) (<= y aa-samples) (++ y)
        (for (var x :1) (<= x aa-samples) (++ x)
          (var sample-offset (- (* aa-sample-width [(float x) (float y)]) pixel-origin))
          (+= color (sample (+ (. gl-frag-coord xy) sample-offset)))
          ))
      (/= color (float (* aa-samples aa-samples)))

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
      (case (field-set/type subject)
        jlsl/type/vec2 (render-2d subject)
        jlsl/type/vec2 (render-3d subject)
        (errorf "whoa whoa whoa, what the heck is %q" subject))))

  # TOOD: this is supposed to return whether or not t is a free variable,
  # which i don't really have a way to see...
  [false (glsl/render-program glsl glsl-version)]
  )

(test-stdout (print (glsl/render-program (jlsl/render/program (get-2d-program
  {:distance (circle 30)
    })))) `
  precision highp float;
  
  out vec4 frag_color;
  
  uniform vec3 camera_origin;
  uniform vec3 camera_orientation;
  uniform float t;
  uniform vec4 viewport;
  
  float circle(float r, vec2 q) {
    length(q) - r;
  }
  
  float nearest_distance(vec2 q) {
    return circle(30.0, q);
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
  
  vec3 sample(vec2 frag_coord, vec4 viewport) {
    vec2 resolution = viewport.zw;
    vec2 local_frag_coord = frag_coord - viewport.xy;
    vec2 relative_position = (local_frag_coord - (resolution * 0.5)) / resolution;
    vec2 local_coord = relative_position * 256.0;
    {
      vec2 q = local_coord;
      vec2 Q = q;
      float d = nearest_distance(q);
      vec2 gradient = calculate_gradient(q);
      return default_2d_color(gradient, d);
    }
  }
  
  void main() {
    const float gamma = 2.2;
    vec3 color = vec3(0.0, 0.0, 0.0);
    const int aa_samples = 3;
    const float aa_sample_width = 1.0 / float(1 + aa_samples);
    const vec2 pixel_origin = vec2(0.5, 0.5);
    for (int y = 1; y <= aa_samples; ++y) {
      for (int x = 1; x <= aa_samples; ++x) {
        vec2 sample_offset = (aa_sample_width * vec2(float(x), float(y))) - pixel_origin;
        color += sample(gl_FragCoord.xy + sample_offset, viewport);
      }
    }
    color /= float(aa_samples * aa_samples);
    frag_color = vec4(pow(color, vec3(1.0 / gamma)), 1.0);
  }
  
`)
