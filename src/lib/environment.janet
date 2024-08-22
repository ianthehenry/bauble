(defmacro view
  "A shorthand for `(set subject _)` that fits nicely into pipe notation, e.g. `(sphere 50 | view)`."
  [subject]
  ~(set subject ,subject))

(var subject
  "A variable that determines what Bauble will render.\n\nYou can set this variable explicitly to change your focus, or use the `view` macro to change your focus. If you don't set a subject, Bauble will render the last expression in your script that it knows how to render."
  nil)

(def x "`[1 0 0]`" [1 0 0])
(def y "`[0 1 0]`" [0 1 0])
(def z "`[0 0 1]`" [0 0 1])
(def -x "`[-1 0 0]`" [-1 0 0])
(def -y "`[0 -1 0]`" [0 -1 0])
(def -z "`[0 0 -1]`" [0 0 -1])
(def +x "`[1 0 0]`" [1 0 0])
(def +y "`[0 1 0]`" [0 1 0])
(def +z "`[0 0 1]`" [0 0 1])

(import ../jlsl)
(import ./dynvars :prefix "" :export true)
(use ./util)
(import ./syntax)
(import ./field-set)
(import ../jlsl/prelude :prefix "" :export true)

(defn- typecheck [expr expected]
  (def actual (jlsl/expr/type expr))
  (assertf (= actual expected)
    "type mismatch: expected %q, got %q"
    (jlsl/show-type expected)
    (jlsl/show-type actual))
  expr)

# TODO: should really be private
(defmacro- defhelper- [return-type name bindings & body]
  ~(jlsl/jlsl/defn- ,return-type ,name ,bindings
    ,;(syntax/expand body)))
(defmacro- defhelper [return-type name bindings & body]
  ~(jlsl/jlsl/defn ,return-type ,name ,bindings
    ,;(syntax/expand body)))

(defmacro- defshape/2d [name bindings docstring & body]
  (assert (string? docstring))
  ~(defn ,name ,docstring ,bindings
    ,;(seq [param :in bindings]
      ~(def ,param (,jlsl/coerce-expr ,param)))
    (field-set/distance-2d (jlsl/do
      ,;(syntax/expand body)))))

(defmacro- defshape/3d [name bindings docstring & body]
  (assert (string? docstring))
  ~(defn ,name ,docstring ,bindings
    ,;(seq [param :in bindings]
      ~(def ,param (,jlsl/coerce-expr ,param)))
    (field-set/distance-3d (jlsl/do
      ,;(syntax/expand body)))))

(defmacro- deftransform [name bindings docstring & body]
  (assert (string? docstring))
  ~(defn ,name ,docstring ,bindings
    ,;(seq [param :in (drop 1 bindings)]
      ~(def ,param (,jlsl/coerce-expr ,param)))
    ,;(syntax/expand body)))

(defmacro .
  "Behaves like `.` in GLSL, for accessing components of a vector or struct, e.g. `(. foo xy)`.\n\nBauble's dot syntax, `foo.xy`, expands to call this macro."
  [expr field]
  [jlsl/expr/dot [jlsl/coerce-expr expr] ['quote field]])

(defshape/2d circle [r]
  "it a circle"
  (- (length q) r))

(defshape/3d sphere [r]
  "it a sphere"
  (- (length p) r))

(defshape/3d box [size]
  "it a box"
  (var d (abs p - vec3 ,size))
  # TODO: i would like to add an overload such that this is just (max d)
  (max d 0 | length + (min (max d.x (max d.y d.z)) 0)))

(defshape/2d rect [size]
  "it a box"
  (var d (abs q - vec2 ,size))
  # TODO: i would like to add an overload such that this is just (max d)
  (max d 0 | length + (min (max d.x d.y) 0)))

(deftransform move [shape offset]
  "translate"
  (typecheck offset (field-set/type shape))
  (if (= (field-set/type shape) jlsl/type/vec2)
    (field-set/map shape (fn [expr]
      (jlsl/with "move" [q (- q offset)] ,expr)))
    (field-set/map shape (fn [expr]
      (jlsl/with "move" [p (- p offset)] ,expr)))))

(deftransform color [field-set color-expression]
  "color"
  (typecheck color-expression jlsl/type/vec3)
  (field-set/with field-set :color color-expression))

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

# TODO: this should be called remap+ but JLSL doesn't
# sanitize properly
# remap -1 to +1 into 0 to 1
(defhelper :float remap-plus [:float x]
  "(remap-plus x)\n\nRemap a number in the range `[-1 1]` into the range `[0 1]`."
  (return (+ 0.5 (* 0.5 x))))

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
          (var contribution (remap-plus (clamp (/ (- nearest dist) r) -1 1)))
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
          (var contribution (- 1 (remap-plus (clamp (/ (min dist (- dist nearest)) r) -1 1))))
          (var h (remap-plus (clamp (/ (- nearest dist) r) -1 1)))
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
            (var h (remap-plus (clamp (/ (- nearest dist) r) -1 1)))
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
  "it rhomb"
  (var q (abs q))
  (var h (size - (2 * q) | ndot size / (dot size size) | clamp -1 1))
  (var d (q - (0.5 * size * [(1 - h) (1 + h)]) | length))
  (d * (q.x * size.y + (q.y * size.x) - (size.x * size.y) | sign)))

(defshape/2d parallelogram [width height skew]
  "it a parallelogram"
  (var e [skew height])
  (var q (if (< q.y 0) (- q) q))
  (var w (q - e))
  (-= w.x (clamp w.x (- width) width))
  (var d [(dot w w) (- w.y)])
  (var s (q.x * e.y - (q.y * e.x)))
  (set q (if (< s 0) (- q) q))
  (var v (q - [width 0]))
  (-= v (e * (dot v e / dot e e | clamp -1 1)))
  (set d (min d [(dot v v) (width * height - abs s)]))
  (sqrt d.x * sign d.y * -1))

(defshape/2d quad-circle [radius]
  "it's like a circle but quaddier."
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
    (set t (sign (h - a) * pow (abs (h - a)) (1 / 3) - pow (h + a) (1 / 3)))
    )
  (do
    (var z (sqrt (- c)))
    (var v (acos (a / (c * z)) / 3))
    (set t ((- z) * (cos v + (sin v * sqrt 3))))))
  (*= t 0.5)
  (var w ([(- t) t] + 0.75 - (t * t) - q))
  (radius * length w * sign (a * a * 0.5 + b - 1.5)))

(defhelper :mat3 rotate-x [:float angle]
  "A rotation matrix about the X axis."
  (var s (sin angle))
  (var c (cos angle))
  (return (mat3
    1 0 0
    0 c s
    0 (- s) c)))

(defhelper :mat3 rotate-y [:float angle]
  "A rotation matrix about the Y axis."
  (var s (sin angle))
  (var c (cos angle))
  (return (mat3
    c 0 (- s)
    0 1 0
    s 0 c)))

(defhelper :mat3 rotate-z [:float angle]
  "A rotation matrix about the Z axis."
  (var s (sin angle))
  (var c (cos angle))
  (return (mat3
    c s 0
    (- s) c 0
    0 0 1)))

(defhelper :mat3 cross-matrix [:vec3 vec]
  "Returns the matrix such that `(* (cross-matrix vec1) vec2)` = `(cross vec1 vec2)`."
  (return (mat3
    0 vec.z (- vec.y)
    (- vec.z) 0 vec.x
    vec.y (- vec.x) 0)))

(defhelper- :mat3 rotate-around [:vec3 axis :float angle]
  (return (+
    (cos angle * mat3 1 0 0 0 1 0 0 0 1)
    (sin angle * cross-matrix axis)
    (1 - cos angle * outer-product axis axis))))

(defmacro- transform [name variable new-position]
  (with-syms [$expr]
    ~(field-set/map shape (fn [,$expr]
      (jlsl/with ,name [,variable ,new-position] (,'unquote ,$expr))))))

(defn- coerce-expr-or-axis [value]
  (case value
    x value
    y value
    z value
    (jlsl/coerce-expr value)))

(defn- floaty? [x] (or (number? x) (and (jlsl/expr? x) (= (jlsl/expr/type x) jlsl/type/float))))

(defhelper- :mat2 rotate-2d [:float angle]
  (var s (sin angle))
  (var c (cos angle))
  (return (mat2 c s (- s) c)))

(defn- rotation-matrix-3d [multiplier args]
  (reduce2 * (seq [[axis angle] :in (partition 2 args)]
    (assert angle "angle required")
    (def angle (* multiplier angle))
    (case axis
      x (rotate-x angle)
      y (rotate-y angle)
      z (rotate-z angle)
      (rotate-around axis angle)))))

(defn- rotation-matrix-2d [multiplier args]
  (rotate-2d (reduce2 + (map |(* multiplier (jlsl/coerce-expr $)) args))))

(defn rotation-matrix [args]
  "Return a rotation matrix. Takes the same arguments as `rotate`, minus the initial thing to rotate."
  (if (floaty? (first args))
    (rotation-matrix-2d 1 args)
    (rotation-matrix-3d 1 args)))

(defn- rotate-shape [shape args]
  (case (field-set/type shape)
    jlsl/type/vec2 (transform "rotate" q (* ,(rotation-matrix-2d -1 args) q))
    jlsl/type/vec3 (transform "rotate" p (* ,(rotation-matrix-3d -1 args) p))))

(defn- rotate-vector [vector args]
  (def v (jlsl/coerce-expr vector))
  (case (jlsl/expr/type v)
    jlsl/type/vec2 (* (rotation-matrix-2d 1 args) v)
    jlsl/type/vec3 (* (rotation-matrix-3d 1 args) v)
    (errorf "I don't know how to rotate %q" (jlsl/expr/to-sexp v))))

(defn rotate
  ````
  Rotate a shape or a vector. Positive angles are counter-clockwise rotations.

  In 3D, the arguments should be pairs of `axis angle`. For example:

  ```
  (rotate (box 50) x 0.1 y 0.2)
  ```

  All `axis` arguments must be unit vectors. There are built-in axis variables `x`/`+y`/`-z`
  for the cardinal directions, and these produce optimized rotation matrices. But you can
  rotate around an arbitrary axis:

  ```
  (rotate (box 50) (normalize [1 1 1]) t)
  ```

  The order of the arguments is significant, as rotations are not commutative.

  In 2D, the arguments should just be angles; no axis is allowed.
  ````
  [target & args]
  (assert (> (@length args) 0) "not enough arguments")
  (if (field-set/is? target)
    (rotate-shape target args)
    (rotate-vector target args)))
