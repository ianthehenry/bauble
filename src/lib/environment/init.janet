(use ./import)

(import ./prelude :prefix "" :export true)
(import ./rotate :prefix "" :export true)
(import ./noise :prefix "" :export true)
(import ../dynvars :prefix "" :export true)
(import ../../jlsl/prelude :prefix "" :export true)

(defmacro view
  "A shorthand for `(set subject _)` that fits nicely into pipe notation, e.g. `(sphere 50 | view)`."
  [subject]
  ~(set subject ,subject))

(var subject
  "A variable that determines what Bauble will render.\n\nYou can set this variable explicitly to change your focus, or use the `view` macro to change your focus. If you don't set a subject, Bauble will render the last shape in your script."
  nil)

(defshape/2d circle [radius]
  "Returns a 2D shape."
  (- (length q) radius))

(defshape/3d sphere [radius]
  "Returns a 3D shape."
  (- (length p) radius))

(defshape/3d box [size]
  ```
  Returns a 3D shape, a box with corners at `(- size)` and `size`. `size` will be coerced to a `vec3`.
  
  Think of `size` like the "radius" of the box: a box with `size.x = 50` will be `100` units wide.
  ```
  (var d (abs p - vec3 ,size))
  # TODO: i would like to add an overload such that this is just (max d)
  (max d 0 | length + (min (max d.x (max d.y d.z)) 0)))

(defshape/2d rect [size]
  ```
  Returns a 2D shape, a rectangle with corners at `(- size)` and `size`. `size` will be coerced to a `vec2`.

  Think of `size` like the "radius" of the rect: a rect with `size.x = 50` will be `100` units wide.
  ```
  (var d (abs q - vec2 ,size))
  # TODO: i would like to add an overload such that this is just (max d)
  (max d 0 | length + (min (max d.x d.y) 0)))

# This is a minor convenience that lets us use the 3D vector
# x/y/-x/-y vectors as arguments to move
# TODO: should we just truncate the vector instead? That's... easier and maybe better?
(defn- coerce-axis-vector [type vector]
  (typecheck
    (jlsl/coerce-expr
      (if (= type jlsl/type/vec2)
        (case vector
          x [1 0]
          y [0 1]
          -x [-1 0]
          -y [0 -1]
          vector)
        vector))
    type))

# TODO: maybe this should be, like, a vector sum, but you peek forward
# each time to see if the next argument is a scalar? would that be better?
# it's a backwards-compatible change...
(defn- sum-scaled-vectors [dimension args]
  (reduce2 + (seq [[direction scale] :in (partition 2 args)]
    (* (coerce-axis-vector dimension direction) (jlsl/coerce-expr (@or scale 1))))))

(defn move
  ````
  Translate a shape. Usually you'd use this with a vector offset:

  ```
  (move (box 50) [0 100 0])
  ```

  But you can also provide a vector and a scalar:

  ```
  (move (box 50) y 100)
  ```

  Which is the same as `(move (box 50) (y * 100))`.

  If you provide multiple vector-scalar pairs, their sum is the final offset:

  ```
  (move (box 50) x 100 y 100 -z 20)
  ```

  That is the same as `(move (box 50) (+ (x * 100) (y * 100) (-z * 100)))`.
  ````
  [shape & args]
  (def offset (sum-scaled-vectors (field-set/type shape) args))
  (if (= (field-set/type shape) jlsl/type/vec2)
    (transform shape "move" q (- q offset))
    (transform shape "move" p (- p offset))))

(deftransform color [shape color]
  "Set the color field of a shape."
  (typecheck color jlsl/type/vec3)
  (field-set/with shape :color color))

(defn- sharp-union [& shapes]
  (def type (get-unique field-set/type shapes (fn [types]
    (if (empty? types)
      (error "empty union expression")
      (error "cannot union 2D and 3D shapes")))))
  (def distances (seq [{:distance d} :in shapes :when d] d))
  (def colors (seq [{:color c :distance d} :in shapes :when c] [c (@or d (jlsl/coerce-expr 0))]))

  (field-set/new type
    :distance (case (@length distances)
      0 nil
      1 (distances 0)
      (jlsl/do "union-distance"
        (var nearest ,(first distances))
        ,;(seq [expr :in (drop 1 distances)]
          (jlsl/statement (set nearest (min nearest ,expr))))
        nearest))

    :color (case (@length colors)
      0 nil
      1 ((colors 0) 0)
      (jlsl/iife "union-color"
        ,;(seq [[c d] :in (reverse (drop 1 colors))]
          (jlsl/statement
            (if (< ,d 0)
              (return ,c))))
        ,((colors 0) 0)
        ))
    ))

# TODO: we should probably have a way to do this
(defn- symmetric-color-union [r shapes]
  (def colors (seq [{:color c :distance d} :in shapes :when c] [c (@or d (jlsl/coerce-expr 0))]))
  (case (@length colors)
    0 nil
    1 ((colors 0) 0)
    (jlsl/do "smooth-union-color"
      (var color [0 0 0])
      (var nearest 1000000)
      ,;(seq [[color-expr dist-expr] :in colors]
        (jlsl/statement
          (var dist ,dist-expr)
          (var contribution (remap+ (clamp (/ (- nearest dist) r) -1 1)))
          (set nearest (- (mix nearest dist contribution) (* r contribution (- 1 contribution))))
          (if (> contribution 0) (set color (mix color ,color-expr contribution)))
        ))
      color)))

(defn- asymmetric-color-union [r shapes]
  (def colors (seq [{:color c :distance d} :in shapes :when c] [c (@or d (jlsl/coerce-expr 0))]))
  (case (@length colors)
    0 nil
    1 ((colors 0) 0)
    (jlsl/do "smooth-union-color"
      (var color [0 0 0])
      (var nearest 1000000)
      ,;(seq [[color-expr dist-expr] :in colors]
        (jlsl/statement
          (var dist ,dist-expr)
          # TODO: wait, what, do we need h here, what is happening
          (var contribution (- 1 (remap+ (clamp (/ (min dist (- dist nearest)) r) -1 1))))
          (var h (remap+ (clamp (/ (- nearest dist) r) -1 1)))
          (set h contribution)
          (set nearest (- (mix nearest dist h) (* r h (- 1 h))))
          (if (> contribution 0) (set color (mix color ,color-expr contribution)))
        ))
      color)))

(defn smooth-union [r & shapes]
  (def type (get-unique field-set/type shapes (fn [types]
    (if (empty? types)
      (error "empty union expression")
      (error "cannot union 2D and 3D shapes")))))
  (def r (typecheck (jlsl/coerce-expr r) jlsl/type/float))

  (def distances (seq [{:distance d} :in shapes :when d] d))

  (field-set/new type
    :distance (case (@length distances)
      0 nil
      1 (distances 0)
      (jlsl/do "smooth-union-distance"
        (var nearest ,(first distances))
        ,;(seq [expr :in (drop 1 distances)]
          (jlsl/statement
            (var dist ,expr)
            (var h (remap+ (clamp (/ (- nearest dist) r) -1 1)))
            (set nearest (- (mix nearest dist h) (* r h (- 1 h))))))
        nearest))

    # TODO
    #:color (symmetric-color-union r shapes)
    :color (asymmetric-color-union r shapes)
    ))

# TODO: this should have an optional radius argument
(defn union
  "Join 'em up. Do it to it."
  [& shapes]
  (sharp-union ;shapes))

(defhelper- :float ndot [:vec2 a :vec2 b]
  (return ((* a.x b.x) - (* a.y b.y))))

(defshape/2d rhombus [size]
  "Returns a 2D shape. It rhombs with a kite."
  (var q (abs q))
  (var h (size - (2 * q) | ndot size / (dot size size) | clamp -1 1))
  (var d (q - (0.5 * size * [(1 - h) (1 + h)]) | length))
  (d * (q.x * size.y + (q.y * size.x) - (size.x * size.y) | sign)))

(defshape/2d parallelogram [size skew]
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
  (sqrt d.x * sign d.y * -1))

(defshape/2d quad-circle [radius]
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
  (radius * length w * sign (a * a * 0.5 + b - 1.5)))

(defhelper :float sum [:vec2 v]
  "Add the components of a vector."
  (return (+ v.x v.y)))

(overload :float sum [:vec3 v] (return (+ v.x v.y v.z)))
(overload :float sum [:vec4 v] (return (+ v.x v.y v.z v.w)))

(defhelper :float product [:vec2 v]
  "Multiply the components of a vector."
  (return (* v.x v.y)))

(overload :float product [:vec3 v] (return (* v.x v.y v.z)))
(overload :float product [:vec4 v] (return (* v.x v.y v.z v.w)))
