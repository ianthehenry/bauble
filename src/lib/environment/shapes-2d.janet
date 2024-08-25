(use ./import)

(defshape/2d circle [:float radius]
  "Returns a 2D shape."
  (return (length q - radius)))

(defshape/2d rect [:vec2 size]
  ```
  Returns a 2D shape, a rectangle with corners at `(- size)` and `size`. `size` will be coerced to a `vec2`.

  Think of `size` like the "radius" of the rect: a rect with `size.x = 50` will be `100` units wide.
  ```
  (var d (abs q - size))
  (return (max d 0 | length + min (max d) 0)))

(defhelper- :float ndot [:vec2 a :vec2 b]
  (return ((* a.x b.x) - (* a.y b.y))))

(defshape/2d rhombus [:vec2 size]
  "Returns a 2D shape. It rhombs with a kite."
  (var q (abs q))
  (var h (size - (2 * q) | ndot size / (dot size size) | clamp -1 1))
  (var d (q - (0.5 * size * [(1 - h) (1 + h)]) | length))
  (return (d * (q.x * size.y + (q.y * size.x) - (size.x * size.y) | sign))))

(defshape/2d parallelogram [:vec2 size :float skew]
  ```
  Returns a 2D shape. `size.x` is the width of the top and bottom edges, and `size.y` is the height of the parellogram.
  
  `skew` is how far the pallorelogram leans in the `x` direction, so the total width of the prellogram is `(size.x + skew) * 2`.
  A `skew` of `0` gives the same shape as `rect`."
  ```
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
  ```
  Returns a 2D shape, an approximation of a circle out of quadratic bezier curves.

  It's like a circle, but quaddier.
  ```
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
