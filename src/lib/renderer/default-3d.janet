(use judge)
(use ./util)
(import ../../jlsl)
(use ./util)
(use ../environment)

(def default-3d-color-expression (sugar
  (jlsl/do
    (normal * 0.5 + 0.5))))

(defn render [env distance-field color-field]
  (def MAX_STEPS :256)
  (def MINIMUM_HIT_DISTANCE 0.1)
  (def NORMAL_OFFSET 0.005)
  (def MAXIMUM_TRACE_DISTANCE (* 64 1024))
  (def PI 3.14159265359)
  (def DEG_TO_RAD (/ PI 180))
  (def nearest-distance (get-env env 'nearest-distance))

  (program/new
    (precision highp float)
    (uniform ,camera-origin)
    (uniform :vec3 camera-orientation)
    (uniform :int render-type)
    (uniform ,t)
    (uniform :vec4 viewport)
    (out :vec4 frag-color)

    # TODO: we need a better default 3D surface
    (implement :float nearest-distance []
      (return distance-field))
      #(return ,(@or (subject :distance) (jlsl/do (length p - 100)))))

    # returns the depth that it's able to march
    (defn :float march [:vec3 ray-origin :vec3 ray-direction [out :int] steps]
      (var depth 0)

      (for (set steps :0) (< steps MAX_STEPS) (++ steps)
        (with [P (+ ray-origin (* depth ray-direction)) p P]
          (var nearest (nearest-distance))

          # we could dynamically adjust the minimum_hit_distance when the ray
          # is farther away from the camera... it could speed up scenes with background
          # scenery for little cost, but might cause issues with reflections
          (if (or (and (>= nearest 0) (< nearest MINIMUM_HIT_DISTANCE)) (> depth MAXIMUM_TRACE_DISTANCE))
            (return depth))

          (+= depth nearest)))
      (return depth))

    (defn :vec3 perspective [:float fov :vec2 size :vec2 pos]
      (var xy (pos - (size * 0.5)))
      (var cot-half-fov (tan (radians (90 - (fov * 0.5)))))
      (var z (* -0.5 (min size.x size.y) cot-half-fov))
      (return (normalize [xy z])))

    (defn :vec3 calculate-normal []
      (def s [1 -1])
      (return (normalize (+
        (s.xyy * with [p (s.xyy * NORMAL_OFFSET + p)] (nearest-distance))
        (s.yyx * with [p (s.yyx * NORMAL_OFFSET + p)] (nearest-distance))
        (s.yxy * with [p (s.yxy * NORMAL_OFFSET + p)] (nearest-distance))
        (s.xxx * with [p (s.xxx * NORMAL_OFFSET + p)] (nearest-distance))
        ))))

    (defn :vec3 nearest-color []
      (return ,(@or color-field default-3d-color-expression)))

    (defn :vec3 sample [:vec2 frag-coord]
      (var resolution viewport.zw)

      # the "camera orientation" vector is really "what transformation do we make to
      # the vector [0 0 1]" to arrive at to the camera origin. but we want to point to
      # the actual origin, so we basically want to reverse this transformation.
      (var dir (*
        (rotation-z (- camera-orientation.z))
        (rotation-y (- camera-orientation.y))
        (rotation-x (- camera-orientation.x))
        (perspective 45.0 resolution frag-coord)
        ))

      (var steps :0)

      (var depth (march camera-origin dir steps))
      (var hit (+ camera-origin (* dir depth)))

      (case render-type
        # default color field
        :0 (do
          (if (>= depth MAXIMUM_TRACE_DISTANCE)
            (do
              (def light (pow ([69 72 79] / 255) (vec3 2.2)))
              (def dark (pow ([40 42 46] / 255) (vec3 2.2)))
              (return (vec3 (mix dark light
                ( frag-coord.x + frag-coord.y
                / (resolution.x + resolution.y ))))))
            (with [P hit
                   p P
                   d (nearest-distance)
                   normal (calculate-normal)]
              (return (nearest-color)))))
        # convergence debug view
        :1
          (return (if (= steps MAX_STEPS)
            [1 0 1]
            (float steps / float MAX_STEPS | vec3)))
        # overshoot debug view
        :2 (with [P hit p P d (nearest-distance)]
          (var overshoot (max (- d) 0 / MINIMUM_HIT_DISTANCE))
          (var undershoot (max d 0 / MINIMUM_HIT_DISTANCE))
          (return [overshoot (- 1 undershoot overshoot) (1 - (step 1 undershoot))]))))

    (defn :void main []
      (def gamma 2.2)
      (var color [0 0 0])
      # too expenive to enable by default
      (def aa-samples :1)
      (def aa-sample-width (/ (float (+ :1 aa-samples))))
      (def pixel-origin [0.5 0.5])
      (var local-frag-coord (gl-frag-coord.xy - viewport.xy))

      (var rotation (rotation-matrix 0.2))
      (for (var y :1) (<= y aa-samples) (++ y)
        (for (var x :1) (<= x aa-samples) (++ x)
          (var sample-offset (aa-sample-width * [(float x) (float y)] - pixel-origin))
          (set sample-offset (* rotation sample-offset))
          (set sample-offset (sample-offset + pixel-origin | fract - pixel-origin))
          (+= color (sample (local-frag-coord + sample-offset) | clamp 0 1))
          ))
      (/= color (float (aa-samples * aa-samples)))

      (set frag-color [(pow color (/ gamma)) 1]))))
