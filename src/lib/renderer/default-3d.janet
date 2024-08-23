(use judge)
(use ./util)
(import ../../jlsl)
(use ./util)
(use ../environment)

(def default-3d-color-expression (sugar
  (jlsl/do
    (normal * 0.5 + 0.5))))

(defn render [subject]
  (def MAX_STEPS :256)
  (def MINIMUM_HIT_DISTANCE 0.1)
  (def NORMAL_OFFSET 0.005)
  (def MAXIMUM_TRACE_DISTANCE (* 64 1024))
  (def PI 3.14159265359)
  (def DEG_TO_RAD (/ PI 180))

  (program/new
    (precision highp float)
    (uniform :vec3 camera-origin)
    (uniform :vec3 camera-orientation)
    (uniform :int render-type)
    (uniform ,t)
    (uniform :vec4 viewport)
    (out :vec4 frag-color)

    # TODO: we need a better default 3D surface
    (defn :float nearest-distance []
      (return ,(subject :distance))
      )
      #(return ,(@or (subject :distance) (jlsl/do (length p - 100)))))

    (defn :vec3 march [:vec3 ray-origin :vec3 ray-direction [out :int] steps]
      (var distance 0)

      (for (set steps :0) (< steps MAX_STEPS) (++ steps)
        (with [P (+ ray-origin (* distance ray-direction)) p P]
          (var nearest (nearest-distance))

          # we could dynamically adjust the minimum_hit_distance when the ray
          # is farther away from the camera... it could speed up scenes with background
          # scenery for little cost, but might cause issues with reflections
          (if (or (< nearest MINIMUM_HIT_DISTANCE) (> distance MAXIMUM_TRACE_DISTANCE))
            (return (+ p (* nearest ray-direction))))

          (+= distance nearest)))
      (return (+ ray-origin (* distance ray-direction))))

    (defn :vec3 perspective [:float fov :vec2 size :vec2 pos]
      (var xy (pos - (size * 0.5)))
      (var cot-half-fov (tan (radians (90 - (fov * 0.5)))))
      (var z (* -0.5 (min size.x size.y) cot-half-fov))
      (return (normalize [xy z])))

    (defn :vec3 calculate-normal []
      (def step (vec2 NORMAL_OFFSET 0))
      (return (normalize
        [(with [p (p + step.xyy)] (nearest-distance) - with [p (p - step.xyy)] (nearest-distance))
         (with [p (p + step.yxy)] (nearest-distance) - with [p (p - step.yxy)] (nearest-distance))
         (with [p (p + step.yyx)] (nearest-distance) - with [p (p - step.yyx)] (nearest-distance))])))

    (defn :vec3 nearest-color []
      (return ,(@or (subject :color) default-3d-color-expression)))

    (defn :void main []
      (def gamma 2.2)

      (var local-coord (gl-frag-coord.xy - viewport.xy))
      (var resolution viewport.zw)

      # the "camera orientation" vector is really "what transformation do we make to
      # the vector [0 0 1]" to arrive at to the camera origin. but we want to point to
      # the actual origin, so we basically want to reverse this transformation.
      (var dir (*
        (rotate-z (- camera-orientation.z))
        (rotate-y (- camera-orientation.y))
        (rotate-x (- camera-orientation.x))
        (perspective 45.0 resolution local-coord)
        ))

      (var steps :0)

      (var hit (march camera-origin dir steps))

      # TODO: should be unassigned
      (var color [0 0 0])

      (case render-type
        # default color field
        :0 (do
          (var depth (distance camera-origin hit))
          #(var depth (length (hit - camera-origin)))
          (if (>= depth MAXIMUM_TRACE_DISTANCE)
            (do
              (def light (pow ([69 72 79] / 255) (vec3 gamma)))
              (def dark (pow ([40 42 46] / 255) (vec3 gamma)))
              (set color (vec3 (mix dark light
                ( local-coord.x + local-coord.y
                / (resolution.x + resolution.y ))))))
            (with [p hit
                   P p
                   d (nearest-distance)
                   normal (calculate-normal)]
              (set color (nearest-color))))
          (break))
        # convergence debug view
        :1 (do
          (if (= steps MAX_STEPS)
            (set color [1 0 1])
            (set color (float steps / float MAX_STEPS | vec3)))
          (break))
        # overshoot debug view
        :2 (with [p hit P p d (nearest-distance)]
          (var overshoot (max (- d) 0 / MINIMUM_HIT_DISTANCE))
          (var undershoot (max d 0 / MINIMUM_HIT_DISTANCE))
          (set color [overshoot (- 1 undershoot overshoot) (1 - (step 1 undershoot))])
          (break)))

      (set frag-color [(pow color (/ gamma)) 1]))))
