(use ../environment)
(import ../../jlsl)
(use ./util)
(use judge)

# TODO: this should not be a uniform
(jlsl/jlsl/defdyn render-type :int "")

(jlsl/jlsl/defdyn camera-type :int "")
(jlsl/jlsl/defdyn free-camera-target :vec3 "")
(jlsl/jlsl/defdyn free-camera-orbit :vec2 "")
(jlsl/jlsl/defdyn free-camera-zoom :float "")

(jlsl/jlsl/defdyn origin-2d :vec2 "")

# TODO: these should probably be somewhere else
(def- MAX_STEPS 256:u)
(def- MINIMUM_HIT_DISTANCE 0.1)
(def- MAXIMUM_HIT_DISTANCE 10)
(def- MAXIMUM_TRACE_DISTANCE (* 64 1024))

(def- base-zoom-distance 512)
(def- free-camera-fov 45)

# think of base zoom distance as the apothem of a triangle.
# we want to know the length of the opposite edge of this triangle.
#
#      -----
#  \   |   /
#   \  |  /
#    \ | /
#     \|/
#
# tan(45 / 2) = (x / base-zoom-distance)

(def- ortho-base-zoom-distance (sugar (* 2 base-zoom-distance (math/tan (free-camera-fov / 360 * tau / 2)))))
(test ortho-base-zoom-distance 424.15468787004937)

(defmacro jlsl/fn [t name & rest]
  ~(as-macro ,sugar
    (as-macro ,jlsl/jlsl/fn ,t ,(string name) ,;rest)))

(defn make-sample-2d [nearest-distance <background-color> <default-color> <color-field>]
  (jlsl/fn :vec4 sample []
    (with [Q (frag-coord * ortho-base-zoom-distance * free-camera-zoom + origin-2d)
           q Q
           dist (nearest-distance)
           gradient (calculate-gradient (nearest-distance))]

      (case render-type
        0:s ,(if <color-field>
          (jlsl/statement
            (if (<= dist 0)
              (return [<color-field> 1])
              (return <background-color>)))
          (jlsl/statement
            (return [<default-color> 1])))
        (return [<default-color> 1])))))

(defn- make-march [nearest-distance]
  (jlsl/fn :float march [[out :uint] steps]
    (var ray-depth 0)

    (for (set steps 0:u) (< steps MAX_STEPS) (++ steps)
      (with [depth ray-depth
             P (+ ray.origin (* ray-depth ray.direction))
             p P]
        (var dist (nearest-distance))
        # we could dynamically adjust the minimum_hit_distance when the ray
        # is farther away from the camera... it could speed up scenes with background
        # scenery for little cost, but might cause issues with reflections
        (if (or (and (>= dist 0) (< dist MINIMUM_HIT_DISTANCE)) (> ray-depth MAXIMUM_TRACE_DISTANCE))
          (return ray-depth))
        (var rate (if (> dist 0) 0.95 1.05))
        (+= ray-depth (* dist rate))
        (if (< ray-depth 0)
          (return 0))))
    (return ray-depth)))

# TODO: okay, so, theoretically we have nearest-distance in the current environment already
(defn make-sample-3d [nearest-distance <camera> <background-color> <default-color> <color-field>]
  (def march (make-march nearest-distance))
  (def ortho-distance 1024)

  (jlsl/fn :vec4 sample []
    (var ray* (Ray [0 0 0] [0 0 1]))
    (var ortho [ortho-distance (* frag-coord ortho-base-zoom-distance free-camera-zoom)])
    (case camera-type
      0:s ,(if <camera>
        (jlsl/statement (set ray* (camera/ray <camera>)) (break))
        (jlsl/statement
          # just fall through
          ))
      1:s (do
        (var camera-rotation-matrix
          (* (rotation-y (* tau free-camera-orbit.x))
             (rotation-x (* tau free-camera-orbit.y))))
        (set ray* (Ray
          (camera-rotation-matrix * [0 0 (base-zoom-distance * free-camera-zoom)] + free-camera-target)
          (camera-rotation-matrix * (perspective-vector free-camera-fov * [1 1 -1]))))
        (break))
      2:s (do # XZ
        (set ray* (Ray (ortho.yxz + free-camera-target) [0 -1 0]))
        (break))
      3:s (do # XY
        (set ray* (Ray (ortho.yzx + free-camera-target) [0 0 -1]))
        (break))
      4:s (do # ZY
        (set ray* (Ray (ortho.xzy + free-camera-target) [-1 0 0]))
        (break)))

    (var steps 0:u)

    (with [ray ray*
           depth (march steps)
           P (+ ray.origin (* ray.direction depth))
           p P
           dist (nearest-distance)
           normal (calculate-normal (nearest-distance))]
      (case render-type
        0:s (if (>= dist MAXIMUM_HIT_DISTANCE)
            (return <background-color>)
            (return [,(@or <color-field> <default-color>) 1]))
        # ignore color field
        1:s (if (>= dist MAXIMUM_HIT_DISTANCE)
            (return <background-color>)
            (return [<default-color> 1]))
        # convergence debug view
        2:s
          (return (if (= steps MAX_STEPS)
            [1 0 1 1]
            [(float steps / float MAX_STEPS | vec3) 1]))
        # overshoot debug view
        3:s (do
          (var overshoot (max (- dist) 0 / MINIMUM_HIT_DISTANCE))
          (var undershoot (max dist 0 / MINIMUM_HIT_DISTANCE))
          (return [overshoot (- 1 undershoot overshoot) (1 - (step 1 undershoot)) 1]))))))
