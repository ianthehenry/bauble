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

    (var gradient-color (remap+ gradient))

    (var inside (step d 0))
    (var isoline (smoothstep (1 - shadow-thickness) 1 (abs d / line-every | fract)))
    (var boundary-line (1 - (smoothstep 0 (boundary-thickness * 0.5) (abs d))))

    (mix
      (pow [gradient-color inside] (mix 1 2 isoline) | clamp 0 1)
      (vec3 1)
      boundary-line)
    )))

(defn render [distance-field color-field]
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
      (return ,(@or distance-field 0)))

    (defn :vec2 calculate-gradient []
      (def step (vec2 NORMAL_OFFSET 0))
      (return (normalize [
        (with [q (q + step.xy)] (nearest-distance) - with [q (q - step.xy)] (nearest-distance))
        (with [q (q + step.yx)] (nearest-distance) - with [q (q - step.yx)] (nearest-distance))
        ])))

    (defn :vec3 sample [:vec2 frag-coord]
      (var resolution viewport.zw)
      (var relative-position (frag-coord - (resolution * 0.5) / resolution))
      # TODO: should vary by zoom amount
      # 384 is a better approximation of a 45° fov at the default zoom level
      (var local-coord (relative-position * 256))

      (with [q local-coord
             Q q
             d (nearest-distance)
             gradient (calculate-gradient)]
        (return ,(if color-field
          (jlsl/do "coloring"
            (if (< d 0)
              ,color-field
              [0 0 0]))
          default-2d-color-expression))))

    (defn :void main []
      (def gamma 2.2)
      (var color [0 0 0])
      (def aa-samples :3)
      (def aa-sample-width (/ (float (+ :1 aa-samples))))
      (def pixel-origin [0.5 0.5])
      (var local-frag-coord (gl-frag-coord.xy - viewport.xy))

      (var rotation (rotation-matrix 0.2))
      (for (var y :1) (<= y aa-samples) (++ y)
        (for (var x :1) (<= x aa-samples) (++ x)
          (var sample-offset (aa-sample-width * [(float x) (float y)] - pixel-origin))
          (set sample-offset (* rotation sample-offset))
          (set sample-offset (sample-offset + pixel-origin | fract - pixel-origin))
          (+= color (sample (local-frag-coord + sample-offset)))
          ))
      (/= color (float (aa-samples * aa-samples)))

      (set frag-color [(pow color (/ gamma)) 1]))))
