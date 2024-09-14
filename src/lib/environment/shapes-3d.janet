(use ./import)
(use ./axis-helpers)

(def r3
  ```A 2D shape with zero distance everywhere.```
  (shape/3d (jlsl/coerce-expr 0)))

(defshape/3d- sphere [:float radius] ""
  (return (length p - radius)))

(defshape/3d- ellipsoid [:vec3 size] ""
  (var k0 (length (p / size)))
  (var k1 (length (p / (size * size))))
  (return (k0 * (k0 - 1) / k1)))

(defn ball
  ````
  Returns a 3D shape, which is either a sphere or an ellipsoid, depending on the type of `size`.

  ```example
  (ball 100)
  ```

  ```example
  (ball [50 80 120])
  ```

  Ellipsoids do not have correct distance fields. Their distance field is only a bound, and
  it has strange isosurfaces that can make it combine with other shapes oddly:

  ```example
  (ball [30 50 80] | slice y)
  ```
  ````
  [size]
  (def size (jlsl/coerce-expr size))
  (case (jlsl/expr/type size)
    jlsl/type/float (sphere size)
    jlsl/type/vec3 (ellipsoid size)
    (error "unknown overload: ball expects a float or vec3")))

(defshape/3d box [:vec3 !size]
  ```
  Returns a 3D shape, a box with corners at `(- size)` and `size`. `size` will be coerced to a `vec3`.
  
  Think of `size` like the "radius" of the box: a box with `size.x = 50` will be `100` units wide.
  ```
  (var d (abs p - size))
  (return (max d 0 | length + min (max d) 0)))

(defshape/3d box-frame [:vec3 !size :float !thickness]
  ````
  Returns a 3D shape, the outline of a box.

  ```example
  (union
    (box-frame 100 5 :r (osc t 3 5))
    (box-frame [(osc t 4 30 100) (osc t 5 30 100) (oss t 6 30 100)] 1))
  ```
  ````
  (var p (abs p - size))
  (var q (abs (p + thickness) - thickness))
  (return (min (min
    ((max [p.x q.y q.z] 0 | length) + (min (max p.x (max q.y q.z)) 0))
    ((max [q.x p.y q.z] 0 | length) + (min (max q.x (max p.y q.z)) 0)))
    ((max [q.x q.y p.z] 0 | length) + (min (max q.x (max q.y p.z)) 0)))))

(deforiented torus [:float radius :float thickness]
  ````
  Returns a 3D shape, a torus around the provided `axis`.

  ```example
  (torus z 100 10)
  ```
  ````
  (return (length [(length other-axes - radius) this-axis] - thickness)))

# TODO: pretty complicated to round correctly
(deforiented cone [:float radius :float height]
  ````
  Returns a 3D shape. The `height` is the extent in only a single direction.

  ```example
  (cone y 50 (sin t * 150))
  ```
  ````
  (var q [radius (- height)])
  (var w [(length other-axes) (this-axis - height)])
  (var a (w - (q * (dot w q / dot q | clamp 0 1))))
  (var b (w - (q * [(w.x / q.x | clamp 0 1) 1])))
  (var k (sign q.y))
  (var d (min (dot a) (dot b)))
  (var s (max (k * (w.x * q.y - (w.y * q.x))) (k * (w.y - q.y))))
  (return (sqrt d * sign s)))

# TODO: subtracting from the radius does *something*, but it's not what
# i would expect
(defshape/3d octahedron [:float radius]
  ````
  Returns a 3D shape.

  ```example
  (octahedron 100 | rotate x t)
  ```
  ````
  (var p (abs p))
  (var m (p.x + p.y + p.z - radius))

  # TODO: should be uninitialized
  (var q [0 0 0])
  (if (< (3 * p.x) m) (set q p.xyz)
    (if (< (3 * p.y) m) (set q p.yzx)
      (if (< (3 * p.z) m) (set q p.zxy)
        (return (m * (sqrt 3 / 3))))))

  (var k (q.z - q.y + radius * 0.5 | clamp 0 radius))
  (return (length [q.x (q.y - radius + k) (q.z - k)])))
