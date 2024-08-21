(use judge)
(use ./util)
(import ../../jlsl)
(use ./util)
(use ../environment)

(def default-2d-color-expression (sugar
  (jlsl/do "default-2d-color"
    (def line-every 10)
    (def shadow-thickness 0.5)
    (def boundary-thickness 2)

    (var gradient-color (+ 0.5 (* 0.5 gradient)))

    (var inside (step (sign d) 0))
    (var isoline (smoothstep (1 - shadow-thickness) 1 (mod (abs d) line-every / line-every)))
    (var boundary-line (1 - (smoothstep 0 (0.5 * boundary-thickness) (abs d))))

    (mix
      (pow (vec3 gradient-color inside) (vec3 (mix 1 2 isoline)))
      (vec3 1)
      boundary-line))))

(defn render [subject]
  (def NORMAL_OFFSET 0.005)

  (program/new
    (precision highp float)
    # just to be ignored...
    (uniform :vec3 camera-origin)
    (uniform :vec3 camera-orientation)
    (uniform ,t)
    (uniform :vec4 viewport)
    (out :vec4 frag-color)

    (defn :float nearest-distance []
      (return ,(@or (subject :distance) 0)))

    (defn :vec2 calculate-gradient []
      (def step (vec2 NORMAL_OFFSET 0))
      (return (normalize [
        (with [q (q + step.xy)] (nearest-distance) - with [q (q - step.xy)] (nearest-distance))
        (with [q (q + step.yx)] (nearest-distance) - with [q (q - step.yx)] (nearest-distance))
        ])))

    (defn :vec3 sample [:vec2 frag-coord]
      (var resolution viewport.zw)
      (var local-frag-coord (frag-coord - viewport.xy))
      (var relative-position (local-frag-coord - (resolution * 0.5) / resolution))
      # TODO: 256 should vary by zoom amount
      (var local-coord (relative-position * 256))

      (with [q local-coord
             Q q
             d (nearest-distance)
             gradient (calculate-gradient)]
        (return ,(if-let [color-expression (subject :color)]
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
          (var sample-offset (aa-sample-width * [(float x) (float y)] - pixel-origin))
          (+= color (sample (gl-frag-coord.xy + sample-offset)))
          ))
      (/= color (float (aa-samples * aa-samples)))

      (set frag-color [(pow color (vec3 (/ gamma))) 1]))))