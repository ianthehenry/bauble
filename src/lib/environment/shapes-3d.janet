(use ./import)
(use ./axis-helpers)

(def r3
  ```A 2D shape with zero distance everywhere.```
  (shape/3d (jlsl/coerce-expr 0)))

(defshape/3d sphere [:float radius]
  ````
  Returns a 3D shape. This is an alias for the float overload of `ball`.
  ````
  (return (length p - radius)))

(defshape/3d ellipsoid [:vec3 size]
  ````
  Returns a 3D shape. This is an alias for the `vec3` overload of `ball`.
  ````
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

(defshape/3d cube [:float !size]
  ```
  This is an alias for the `float` overload of `box`.
  ```
  (var d (abs p - size))
  (return (max d 0 | length + min (max d) 0)))

(defshape/3d- box [:vec3 !size] ""
  (var d (abs p - size))
  (return (max d 0 | length + min (max d) 0)))

(def- box- box)

(defnamed box [size :?r:round]
  ````
  Returns a 3D shape, a box with corners at `(- size)` and `size`. `size` will be coerced to a `vec3`.
  
  Think of `size` like the "radius" of the box: a box with `size.x = 50` will be `100` units wide.

  ```example
  (box 100 :r (osc t 3 0 10))
  ```

  ```example
  (box [100 (osc t 3 50 100) (oss t 4 50 100)])
  ```
  ````
  (def size (jlsl/coerce-expr size))
  (case (jlsl/expr/type size)
    jlsl/type/float (cube size :r round)
    jlsl/type/vec3 (box- size :r round)
    (error "unknown overload: box expects a float or vec3")))

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
  (torus z 100 (osc t 3 10 50))
  ```
  ````
  (return (length [(length other-axes - radius) this-axis] - thickness)))

(deforiented- uneven-capsule [:float r1 :float r2 :float height] ""
  (var q [(length other-axes) (this-axis * sign height)])
  (set height (abs height))
  (var b (r1 - r2 / height))
  (var a (b * b | 1 - _ | sqrt))
  (var k [(- b) a | dot q])
  (if (< k 0) (return (length q - r1)))
  (if (> k (a * height)) (return (q - [0 height] | length - r2)))
  (return [a b | dot q - r1]))

(deforiented- capsule [:float radius :float height] ""
  (var p [this-axis other-axes])
  (*= p.x (sign height))
  (-= p.x (clamp p.x 0 (abs height)))
  (return (length p - radius)))
(def- capsule- capsule)

(defnamed capsule [axis length radius ?top-radius]
  ````
  There are two types of `capsule`s: symmetric capsules, which look
  like pills, or axis-aligned lines:

  ```example
  (capsule y 100 25)
  ```

  And asymmetric capsules, which have a different radius at the
  top and bottom:

  ```example
  (capsule y 100 25 10)
  ```
  ````
  (if top-radius
    (uneven-capsule axis radius top-radius length)
    (capsule- axis radius length)))

(defshape/3d- line-3d [:vec3 a :vec3 b :float r] ""
  (var pa (p - a))
  (var ba (b - a))
  (var h (dot pa ba / dot ba | clamp 0 1))
  (return (pa - (ba * h) | length - r)))

(defshape/3d- uneven-line-3d [:vec3 a :vec3 b :float r1 :float r2] ""
  (var ba (b - a))
  (var l2 (dot ba))
  (var rr (r1 - r2))
  (var a2 (l2 - (rr * rr)))
  (var il2 (/ l2))

  (var pa (p - a))
  (var y (dot pa ba))
  (var z (y - l2))
  (var x2 (pa * l2 - (ba * y) | dot))
  (var y2 (* y y l2))
  (var z2 (* z z l2))

  (var k (* (sign rr) rr rr x2))
  (if (> (* (sign z) a2 z2) k) (return (x2 + z2 | sqrt * il2 - r2)))
  (if (< (* (sign y) a2 y2) k) (return (x2 + y2 | sqrt * il2 - r1)))
  (return (* x2 a2 il2 | sqrt + (y * rr) * il2 - r1)))

(defshape/2d- line-2d [:vec2 start :vec2 end :float width] ""
  (var q-start (q - start))
  (var end-start (end - start))
  (var h (clamp (dot q-start end-start / dot end-start) 0 1))
  (return (length (q-start - (end-start * h)) - (width * 0.5))))

(defshape/2d- uneven-line-2d [:vec2 start :vec2 end :float start-width :float end-width] ""
  (var q-start (q - start))
  (var end-start (end - start))
  (var h (clamp (dot q-start end-start / dot end-start) 0 1))
  (return (length (q-start - (end-start * h)) - (mix start-width end-width h * 0.5))))

(defnamed line [from to from-radius ?to-radius]
  ````
  Returns a line between two points.

  ```example
  (line
    [-100 (sin t * 100) (cos t * 100)]
    [100 (cos t * 100) (sin t * 100)]
    10
  | union (box-frame 100 1))
  ```

  You can supply two radii to taper the line over its length:

  ```example
  (line
    [-100 (sin t * 100) (cos t * 100)]
    [100 (cos t * 100) (sin t * 100)]
    (oss t 3 50) (osc t 5 50)
  | union (box-frame 100 1))
  ```

  You can also give 2D points for a line in 2D:

  ```example
  (line
    [-100 (cos t * 100)]
    [100 (sin t * 100)]
    (osc t 3 50) (osc t 5 50))
  ```
  ````
  (def from (jlsl/coerce-expr from))
  (def to (typecheck to (jlsl/expr/type from)))
  (def three-d (case (jlsl/expr/type from)
    jlsl/type/vec2 false
    jlsl/type/vec3 true
    (error "line needs 2D or 3D points")))
  (cond
    (@and to-radius three-d) (uneven-line-3d from to from-radius to-radius)
    three-d (line-3d from to from-radius)
    to-radius (uneven-line-2d from to from-radius to-radius)
    (line-2d from to from-radius)))

(deforiented- round-cone [:float radius :float height :float round] ""
  (var offset (height / radius * round))
  (-= radius round)
  (-= height offset)
  (var q [radius (- height)])
  (var w [(length other-axes) (this-axis - height - (sign height * round))])
  (var a (w - (q * (dot w q / dot q | clamp 0 1))))
  (var b (w - (q * [(w.x / q.x | clamp 0 1) 1])))
  (var k (sign q.y))
  (var d (min (dot a) (dot b)))
  (var s (max (k * (w.x * q.y - (w.y * q.x))) (k * (w.y - q.y))))
  (return (sqrt d * sign s - round)))

(deforiented- cone [:float radius :float height] ""
  (var q [radius (- height)])
  (var w [(length other-axes) (this-axis - height)])
  (var a (w - (q * (dot w q / dot q | clamp 0 1))))
  (var b (w - (q * [(w.x / q.x | clamp 0 1) 1])))
  (var k (sign q.y))
  (var d (min (dot a) (dot b)))
  (var s (max (k * (w.x * q.y - (w.y * q.x))) (k * (w.y - q.y))))
  (return (sqrt d * sign s)))
(def- cone- cone)

(defnamed cone [axis radius height :?r:round]
  ````
  Returns a 3D shape. The `height` is the extent in only a single direction.

  ```example
  (cone y 50 (sin t * 150) :r (osc t 2 10))
  ```

  If you supply a rounding factor, the cone will be offset such that
  it always rests exactly on the zero plane normal to your axis. Is
  that what you'd expect? I went back on forth on this. I think it's more
  intuitive but if you have thoughts I'd like to hear them.
  ````
  (if round
    (round-cone axis radius height round)
    (cone- axis radius height)))

(deforiented cylinder [:float !radius :float !height]
  ````
  Returns a 3D shape, a cylinder oriented along the given `axis`.

  ```example
  (cylinder y 50 100)
  ```

  The second argument is twice the length of the cylinder. Like many shapes,
  you can round it with `:r`.

  ```example
  (cylinder z 100 50 :r (osc t 3 0 20))
  ```
  ````
  (var d ((abs [(length other-axes) this-axis]) - [radius height]))
  (return (min (max d) 0 + length (max d 0))))

(defshape/3d octahedron [:float !radius]
  ````
  Returns a 3D shape.

  ```example
  (octahedron 100 :r (sin+ t * 20) | rotate x t y t z t)
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

(defnamed plane [normal ?offset]
  ````
  Returns a 3D shape that represents the infinite extrusion
  of a plane in a single direction. `normal` should be a
  normalized vector.

  Because of its infinite size and featurelessness, this shape
  is difficult to visualize on its own. You'll probably want to
  use it in combination with boolean operations:

  ```example
  (ball 100 | intersect (plane x (sin t * 50)))
  ```

  `(plane x)` is the shape that faces the `+x` direction and extends
  infinitely in the `-x` direction, and a positive offset will move it
  in the `+x` direction.
  ````
  (def normal (typecheck normal jlsl/type/vec3))
  (def offset (typecheck? offset jlsl/type/float))
  (sugar (shape/3d (if offset
    (dot p normal - offset)
    (dot p normal)))))

(sugar (defnamed ground [?offset]
  ````
  Returns a 3D plane that only exists while the camera is above it.

  This is useful for quickly debugging shadows while still being able
  to see the underside of your scene, although note that taking the plane
  away will affect ambient occlusion, so you're not *really* seeing the
  underside.
  ````
  (def offset (typecheck (@or offset 0) jlsl/type/float))
  (shape/3d (gl/if (< ray.origin.y offset) 1e6 (dot p y - offset)))))
