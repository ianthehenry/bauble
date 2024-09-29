(use ./import)
(use ./transforms)

(def r2
  ```A 2D shape with zero distance everywhere.```
  (shape/2d (jlsl/coerce-expr 0)))

(defshape/2d circle [:float radius]
  ````
  Returns a 2D shape.

  ```example
  (circle 100)
  ```

  A circle is the most primitive shape, and it's very versatile. With no radius it's a
  point in space, which can be useful for procedural patterns:

  ```example
  (circle 0
  | move (hash2 $i * 50)
  | tile: $i [50 50] :oversample true :sample-from -1 :sample-to 1)
  ```

  (See also `worley` for an optimized version of that voronoi pattern.)

  By varying the radius dynamically, you can produce other interesting shapes:

  ```example
  (circle (ss q.x -100 150 100 150)
  | color green)
  ```

  ```example
  (circle (sin+ (abs q.y - abs q.x / 10 + (t * 5)) * 20 + 100)
  | color sky)
  ```

  And by projecting it into 3D, you can produce shapes like a disc:

  ```example
  (circle 100 | extrude y | expand 25)
  ```

  A torus:

  ```example
  (circle 100 | shell | extrude y | expand 25)
  ```

  A tube:

  ```example
  (circle 100 | shell | extrude z 100 | expand 10)
  ```

  Among other shapes.
  ````
  (return (length q - radius)))

(defshape/2d- symmetric-rect [:vec2 !size] ""
  (var d (abs q - size))
  (return (max d 0 | length + min (max d) 0)))

(defshape/2d- asymmetric-rect [:vec2 size :vec4 radii] ""
  (var r (if (< q.x 0) radii.xw radii.yz))
  (var r (if (> q.y 0) r.x r.y))
  (var q (abs q - size + r))
  (return (min (max q) 0 + length (max q 0) - r)))

(defnamed rect [size :?r:radius]
  ````
  Returns a 2D shape, a rectangle with corners at `(- size)` and `size`. `size` will be coerced to a `vec2`.

  Think of `size` like the "radius" of the rect: a rect with `size.x = 50` will be `100` units wide.

  `radii` can be a single radius or a `vec4` of `[top-left` `top-right` `bottom-right` `bottom-left]`.

  ```example
  (union
    (rect 50 | move [-100 100])
    (rect 50 :r 10 | move [100 100])
    (rect 50 :r [0 10 20 30] | move [-100 -100])
    (rect 50 :r [0 30 0 30] | move [100 -100]))
  ```
  ````
  (if radius
    (let [radius (jlsl/coerce-expr radius)]
      (case (jlsl/expr/type radius)
        jlsl/type/float (symmetric-rect size :r radius)
        jlsl/type/vec4 (asymmetric-rect size radius)
        (error "type mismatch: expected float or vec3")))
    (symmetric-rect size)))

(defhelper- :float ndot [:vec2 a :vec2 b]
  (return ((* a.x b.x) - (* a.y b.y))))

(defshape/2d rhombus [:vec2 !size]
  ````
  Returns a 2D shape. It rhombs with a kite.

  ```example
  (rhombus [100 (osc t 3 50 150)])
  ```
  ````
  (var q (abs q))
  (var h (size - (2 * q) | ndot size / (dot size size) | clamp -1 1))
  (var d (q - (0.5 * size * [(1 - h) (1 + h)]) | length))
  (return (d * (q.x * size.y + (q.y * size.x) - (size.x * size.y) | sign))))

# TODO: rounding this is kinda complex because because skew
# needs to change by some complex factor
(defshape/2d parallelogram [:vec2 size :float skew]
  ````
  Returns a 2D shape. `size.x` is the width of the top and bottom edges, and `size.y` 
  is the height of the parellogram.

  ```example
  (parallelogram [80 100] (sin t * 100))
  ```
  
  `skew` is how far the pallorelogram leans in the `x` direction, so the total
  width of the prellogram is `(size.x + skew) * 2`. A `skew` of `0` gives the
  same shape as `rect`.
  ````
  (var e [skew size.y])
  (var q (if (< q.y 0) (- q) q))
  (var w (q - e))
  (-= w.x (clamp w.x (- size.x) size.x))
  (var d [(dot w w) (- w.y)])
  (var s (q.x * e.y - (q.y * e.x)))
  (set q (if (< s 0) (- q) q))
  (var v (q - [size.x 0]))
  (-= v (e * (dot v e / dot e e | clamp -1 1)))
  (set d (min d [(dot v v) (size.x * size.y - abs s)]))
  (return (sqrt d.x * sign d.y * -1)))

(defshape/2d quad-circle [:float radius]
  ````
  Returns a 2D shape, an approximation of a circle made out of quadratic bezier curves.

  ```example
  (quad-circle 100)
  ```

  It's like a circle, but quaddier.
  ````
  (var q (abs q / radius))
  (if (> q.y q.x)
    (set q q.yx))
  (var a (q.x - q.y))
  (var b (q.x + q.y))
  (var c (2 * b - 1 / 3))
  (var h (a * a + (c * c * c)))
  # TODO: t should be uninitialized
  (var t 0)
  (if (>= h 0) (do
    (set h (sqrt h))
    (set t (sign (h - a) * pow (abs (h - a)) (1 / 3) - pow (h + a) (1 / 3))))
  (do
    (var z (sqrt (- c)))
    (var v (acos (a / (c * z)) / 3))
    (set t ((- z) * (cos v + (sin v * sqrt 3))))))
  (*= t 0.5)
  (var w ([(- t) t] + 0.75 - (t * t) - q))
  (return (radius * length w * sign (a * a * 0.5 + b - 1.5))))

# TODO: this should maybe just be a "line cap" option?
(defshape/2d oriented-rect [:vec2 start :vec2 end :float width]
  ```
  TODOC
  ```
  (var l (length (end - start)))
  (var d (end - start / l))
  (var q (q - (start + end * 0.5)))
  (set q (mat2 d.x (- d.y) d.y d.x * q))
  (set q (abs q - ([l width] * 0.5)))
  (return (length (max q 0) + min (max q) 0)))

(defshape/2d trapezoid [:float !bottom-width :float !top-width :float !height]
  ````
  Returns a 2D shape.

  ```example
  (trapezoid (osc t 3 50 100) (oss t 2 100 50) 100)
  ```
  ````
  (var k1 [top-width height])
  (var k2 [(top-width - bottom-width) (2 * height)])
  (var q [(abs q.x) q.y])
  (var ca [(q.x - min q.x (if (< q.y 0) bottom-width top-width)) (abs q.y - height)])
  (var cb (q - k1 + (k2 * (dot (k1 - q) k2 / dot k2 | clamp 0 1))))
  (var s (if (and (< cb.x 0) (< ca.y 0)) -1 1))
  (return (s * sqrt (min (dot ca) (dot cb)))))

(defshape/2d- equilateral-triangle [:float radius] ""
  (def k (sqrt 3))
  (var q [((abs q.x) - radius) (q.y + (radius / k))])
  (if (> (q.x + (k * q.y)) 0)
    (set q ([(q.x - (k * q.y)) (- k * q.x - q.y)] * 0.5)))
  (-= q.x (clamp q.x (-2 * radius) 0))
  (return (* -1 (length q) (sign q.y))))

# TODO: the asymmetric height makes it difficult to round this
(defshape/2d- isosceles-triangle [:vec2 size] ""
  (var q [(abs q.x) (size.y - q.y)])
  (var a (q - (size * ((dot q size) / (dot size) | clamp 0 1))))
  (var b (q - (size * [(q.x / size.x | clamp 0 1) 1])))
  (var k (sign size.y))
  (var d (min (dot a) (dot b)))
  (var s (max (q.x * size.y - (q.y * size.x) * k) (q.y - size.y * k)))
  (return (sqrt d * sign s)))

(defshape/2d- triangle-points-2d [:vec2 a :vec2 b :vec2 c] ""
  (var e0 (b - a))
  (var e1 (c - b))
  (var e2 (a - c))
  (var v0 (q - a))
  (var v1 (q - b))
  (var v2 (q - c))
  (var pq0 (v0 - (e0 * ((dot v0 e0) / (dot e0 e0) | clamp 0 1))))
  (var pq1 (v1 - (e1 * ((dot v1 e1) / (dot e1 e1) | clamp 0 1))))
  (var pq2 (v2 - (e2 * ((dot v2 e2) / (dot e2 e2) | clamp 0 1))))
  (var s (sign (e0.x * e2.y - (e0.y * e2.x))))
  (var d (min (min
    [(dot pq0) (v0.x * e0.y - (v0.y * e0.x) * s)]
    [(dot pq1) (v1.x * e1.y - (v1.y * e1.x) * s)])
    [(dot pq2) (v2.x * e2.y - (v2.y * e2.x) * s)]))
  (return (* -1 (sqrt d.x) (sign d.y))))

(defshape/3d- triangle-points-3d [:vec3 a :vec3 b :vec3 c] ""
  (var ba (b - a))
  (var pa (p - a))
  (var cb (c - b))
  (var pb (p - b))
  (var ac (a - c))
  (var pc (p - c))
  (var nor (cross ba ac))

  (return (sqrt
    (if (+ (cross ba nor | dot pa | sign)
           (cross cb nor | dot pb | sign)
           (cross ac nor | dot pc | sign)
        | < 2)
     (min (min
       (dot (ba * (dot ba pa / dot ba | clamp 0 1) - pa))
       (dot (cb * (dot cb pb / dot cb | clamp 0 1) - pb)))
       (dot (ac * (dot ac pc / dot ac | clamp 0 1) - pc)))
     (dot nor pa * dot nor pa / dot nor)))))

(defnamed triangle [&args]
  ````
  Usually returns a 2D shape, with various overloads:

  ```example
  (triangle 100)
  ```

  ```example
  (triangle [50 100])
  ```

  ```example
  (triangle [-50 100] [100 10] [-10 -100])
  ```

  But it can also return a 3D shape:

  ```example
  (triangle 
    [(osc t 4 -100 100) -100 (oss t 5 -100 100)]
    [100 (osc t 6 -100 100) 100]
    [-100 (oss t 7 -100 100) (osc t 8 -100 100)]
  | union (box-frame 100 1))
  ```
  ````
  (case (@length args)
    1 (let [size (jlsl/coerce-expr (in args 0))]
        (case (jlsl/expr/type size)
          jlsl/type/float (equilateral-triangle size)
          jlsl/type/vec2 (isosceles-triangle size)
          (error "unknown triangle overload: expected float or vec2")))
    3 (let [[a b c] args]
      (def a (jlsl/coerce-expr a))
      (def b (typecheck b (jlsl/expr/type a)))
      (def c (typecheck c (jlsl/expr/type a)))
      (case (jlsl/expr/type a)
        jlsl/type/vec2 (triangle-points-2d a b c)
        jlsl/type/vec3 (triangle-points-3d a b c)
        (error "whatrya tryna pull")))
    (error "wrong number of arguments")))

(defshape/2d capsule-2d [:float bottom-radius :float top-radius :float height]
  ````
  ```example
  (capsule-2d 50 (osc t 3 20 60) (oss t 8 30 100))
  ```
  ````
  (var q [(abs q.x) q.y])
  (var b (bottom-radius - top-radius / height))
  (var a (sqrt (1 - (b * b))))
  (var k (dot q [(- b) a]))
  (if (< k 0)
    (return (length q - bottom-radius)))
  (if (> k (a * height))
    (return (length (q - [0 height]) - top-radius)))
  (return (dot q [a b] - bottom-radius)))

(defshape/2d pentagon [:float !radius]
  ````
  ```example
  (pentagon 100 :r (osc t 3 20))
  ```
  ````
  (def angle (math/pi / 5))
  (def k [(cos angle) (sin angle) (tan angle)])
  (var q [(abs q.x) (- q.y)])
  (-= q (2 * (min (dot [(- k.x) k.y] q) 0) * [(- k.x) k.y]))
  (-= q (2 * (min (dot [k.x k.y] q) 0) * [k.x k.y]))
  (-= q [(clamp q.x (* -1 radius k.z) (radius * k.z)) radius])
  (return (length q * sign q.y)))

(defshape/2d hexagon [:float !radius]
  ````
  ```example
  (hexagon 100 :r (osc t 3 20))
  ```
  ````
  (def angle (math/pi / 6))
  (def k [(- (cos angle)) (sin angle) (tan angle)])
  (var q (abs q))
  (-= q (2 * (min (dot k.xy q) 0) * k.xy))
  (-= q [(clamp q.x ((- k.z) * radius) (k.z * radius)) radius])
  (return (length q * sign q.y)))

(defshape/2d octagon [:float !radius]
  ````
  ```example
  (octagon 100 :r (osc t 3 20))
  ```
  ````
  (def angle (math/pi / 8))
  (def k [(- (cos angle)) (sin angle) (tan angle)])
  (var q (abs q))
  (-= q (2 * (min (dot [k.x k.y] q) 0) * [k.x k.y]))
  (-= q (2 * (min (dot [(- k.x) k.y] q) 0) * [(- k.x) k.y]))
  (-= q [(clamp q.x (- k.z * radius) (k.z * radius)) radius])
  (return (length q * sign q.y)))

(defshape/2d hexagram [:float !radius]
  ````
  ```example
  (hexagram 100 :r (osc t 3 20))
  ```
  ````
  (def angle (math/pi / 6))
  (def k [(- (sin angle)) (cos angle) (tan angle) (sqrt 3)])
  (var q (abs q))
  (var radius (0.5 * radius))
  (-= q (2 * (min (dot k.xy q) 0) * k.xy))
  (-= q (2 * (min (dot k.yx q) 0) * k.yx))
  (-= q [(clamp q.x (k.z * radius) (k.w * radius)) radius])
  (return (length q * sign q.y)))

(defshape/2d star [:float !outer-radius :float !inner-radius]
  ````
  ```example
  (star 100 70 :r (osc t 3 20))
  ```
  ````
  (def angle (math/pi / 5))
  (def k1 [(cos angle) (- (sin angle))])
  (def k2 [(- k1.x) k1.y])
  (var rf (inner-radius / outer-radius))
  (var q [(abs q.x) q.y])
  (-= q (2 * max (dot k1 q) 0 * k1))
  (-= q (2 * max (dot k2 q) 0 * k2))
  (set q.x (abs q.x))
  (-= q.y outer-radius)
  (var ba (rf * [(- k1.y) k1.x] - [0 1]))
  (var h (dot q ba / (dot ba) | clamp 0 outer-radius))
  (return (length (q - (ba * h)) * sign (q.y * ba.x - (q.x * ba.y)))))

# TODO: kinda tricky to round because it's centered at the origin
(defshape/2d pie [:float radius :float angle]
  ````
  Returns a 2D shape, something like a pie slice or a pacman depending on `angle`.

  ```example
  (pie 100 (osc t 5 tau))
  ```
  ````
  (var angle (angle * 0.5))
  (var c [(sin angle) (cos angle)])
  (var q [(abs q.x) q.y])
  (var l (length q - radius))
  (var m (length (q - (c * (dot q c | clamp 0 radius)))))
  (return (max l (m * sign (c.y * q.x - (c.x * q.y))))))

# TODO: in order to round this, we have to subtract from
# radius but add to bottom
(defshape/2d cut-disk [:float radius :float bottom]
  ````
  Returns a 2D shape.

  ```example
  (cut-disk 100 (sin t * 80))
  ```
  ````
  (var w (sqrt (radius * radius - (bottom * bottom))))
  (var q [(abs q.x) q.y])
  (var s (max (bottom - radius * q.x * q.x + (w * w * (bottom + radius - (2 * q.y)))) (bottom * q.x - (w * q.y))))
  (if (< s 0) (return (length q - radius)))
  (if (< q.x w) (return (bottom - q.y)))
  (return (length (q - [w bottom]))))

(defshape/2d arc [:float radius :float angle :float thickness]
  ````
  ```example
  (arc 100 (osc t 5 tau) (osc t 3 5 20))
  ```
  ````
  (var angle (angle * 0.5))
  (var sc [(sin angle) (cos angle)])
  (var q [(abs q.x) q.y])
  (return (if (> (sc.y * q.x) (sc.x * q.y))
    (length (q - (sc * radius)))
    (abs (length q - radius)) - (thickness * 0.5))))

(defshape/2d ring [:float radius :float angle :float thickness]
  ````
  ```example
  (ring 100 (osc t 5 tau) (osc t 3 5 20))
  ```
  ````
  (var q (rotate [(abs q.x) q.y] (angle * 0.5)))
  (return (max
    (abs (length q - radius) - (thickness * 0.5))
    (length [q.x (max 0 (abs (radius - q.y) - (thickness * 0.5)))] * sign q.x))))
