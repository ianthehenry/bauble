(use ./import)
(use ./axis-helpers)

(defshape/3d sphere [:float radius]
  "Returns a 3D shape."
  (return (length p - radius)))

(defshape/3d box [:vec3 size]
  ```
  Returns a 3D shape, a box with corners at `(- size)` and `size`. `size` will be coerced to a `vec3`.
  
  Think of `size` like the "radius" of the box: a box with `size.x = 50` will be `100` units wide.
  ```
  (var d (abs p - size))
  (return (max d 0 | length + min (max d) 0)))

(defshape/3d box-frame [:vec3 size :float thickness]
  ```
  Returns a 3D shape, the outline of a box.
  ```
  (var p (abs p - size))
  (var q (abs (p + thickness) - thickness))
  (return (min (min
    ((max [p.x q.y q.z] 0 | length) + (min (max p.x (max q.y q.z)) 0))
    ((max [q.x p.y q.z] 0 | length) + (min (max q.x (max p.y q.z)) 0)))
    ((max [q.x q.y p.z] 0 | length) + (min (max q.x (max q.y p.z)) 0)))))

(defn torus
  ```
  Returns a 3D shape, a torus around the provided `axis`.
  ```
  [axis radius thickness]
  (def [this other] (split-axis axis))
  (def radius (jlsl/coerce-expr radius))
  (def thickness (jlsl/coerce-expr thickness))
  (field-set/distance-3d (sugar
    (jlsl/do "torus"
      (length [(length other - radius) this] - thickness)))))
