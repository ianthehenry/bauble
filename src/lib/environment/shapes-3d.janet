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

(deforiented torus [:float radius :float thickness]
  ```
  Returns a 3D shape, a torus around the provided `axis`.
  ```
  (return (length [(length other-axes - radius) this-axis] - thickness)))

(deforiented cone [:float radius :float height]
  ```
  TODOC
  ```
  (var q [radius (- height)])
  (var w [(length other-axes) (this-axis - height)])
  (var a (w - (q * (dot w q / dot q | clamp 0 1))))
  (var b (w - (q * [(w.x / q.x | clamp 0 1) 1])))
  (var k (sign q.y))
  (var d (min (dot a) (dot b)))
  (var s (max (k * (w.x * q.y - (w.y * q.x))) (k * (w.y - q.y))))
  (return (sqrt d * sign s)))
