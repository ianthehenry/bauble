(use ../environment)
(import ../../jlsl)
(use ./util)

(jlsl/jlsl/defdyn camera-origin :vec3 "")
(jlsl/jlsl/defdyn camera-orientation :vec3 "")
(jlsl/jlsl/defdyn render-type :int "")

# TODO: these should probably be somewhere else
(def- MAX_STEPS :256)
(def- MINIMUM_HIT_DISTANCE 0.1)
(def- MAXIMUM_TRACE_DISTANCE (* 64 1024))

(defmacro jlsl/fn [t name & rest]
  ~(as-macro ,sugar
    (as-macro ,jlsl/jlsl/fn ,t ,(string name) ,;rest)))

(defn make-sample-2d [nearest-distance <color>]
  (jlsl/fn :vec3 sample []
    # TODO: should vary by zoom amount
    # 384 is a better approximation of a 45° fov at the default zoom level
    (with [q (frag-coord * 256)
           Q q
           dist (nearest-distance)
           gradient (calculate-gradient (nearest-distance))]

      (case render-type
        :0 ,(if <color>
          (jlsl/statement
            (if (<= dist 0)
              (return ,<color>)
              (return ,default-background-color)))
          (jlsl/statement
            (return (gradient-color))))
        (return (gradient-color))))))

(defn- make-march [nearest-distance]
  (jlsl/fn :float march [[out :int] steps]
    (var ray-depth 0)

    (for (set steps :0) (< steps MAX_STEPS) (++ steps)
      (with [depth ray-depth
             P (+ ray-origin (* ray-depth ray-dir))
             p P]
        (var dist (nearest-distance))
        # we could dynamically adjust the minimum_hit_distance when the ray
        # is farther away from the camera... it could speed up scenes with background
        # scenery for little cost, but might cause issues with reflections
        (if (or (and (>= dist 0) (< dist MINIMUM_HIT_DISTANCE)) (> ray-depth MAXIMUM_TRACE_DISTANCE))
          (return ray-depth))

        (+= ray-depth dist)))
    (return ray-depth)))

# TODO: okay, so, theoretically we have nearest-distance in the current environment already
(defn make-sample-3d [nearest-distance <color>]
  (def march (make-march nearest-distance))
  (jlsl/fn :vec3 sample []
    # the "camera orientation" vector is really "what transformation do we make to
    # the vector [0 0 1]" to arrive at to the camera origin. but we want to point to
    # the actual origin, so we basically want to reverse this transformation.
    (var dir (*
      (rotation-z (- camera-orientation.z))
      (rotation-y (- camera-orientation.y))
      (rotation-x (- camera-orientation.x))
      (perspective 45.0 resolution Frag-Coord)
      ))

    (var steps :0)

    (with [ray-origin camera-origin
           ray-dir dir
           depth (march steps)
           P (+ ray-origin (* ray-dir depth))
           p P
           dist (nearest-distance)
           normal (calculate-normal (nearest-distance))]
      (case render-type
        :0 (if (>= depth MAXIMUM_TRACE_DISTANCE)
            (return default-background-color)
            (return ,(@or <color> (normal-color))))
        # ignore color field
        :1 (if (>= depth MAXIMUM_TRACE_DISTANCE)
            (return default-background-color)
            (return (normal-color)))
        # convergence debug view
        :2
          (return (if (= steps MAX_STEPS)
            [1 0 1]
            (float steps / float MAX_STEPS | vec3)))
        # overshoot debug view
        :3 (do
          (var overshoot (max (- dist) 0 / MINIMUM_HIT_DISTANCE))
          (var undershoot (max dist 0 / MINIMUM_HIT_DISTANCE))
          (return [overshoot (- 1 undershoot overshoot) (1 - (step 1 undershoot))]))))))
