(use ./util)
(import ../../jlsl)
(import ../shape)

(defmacro .
  ```
  Behaves like `.` in GLSL, for accessing components of a vector or struct, e.g. `(. foo xy)`.

  Bauble's dot syntax, `foo.xy`, expands to call this macro. The second argument to `.` will be
  quasiquoted, so you can dynamically select a dynamic field with `(. foo ,axis)`.
  ```
  [expr field]
  [jlsl/expr/dot [jlsl/coerce-expr expr] ['quasiquote field]])

(def x "`[1 0 0]`" [1 0 0])
(def y "`[0 1 0]`" [0 1 0])
(def z "`[0 0 1]`" [0 0 1])
(def -x "`[-1 0 0]`" [-1 0 0])
(def -y "`[0 -1 0]`" [0 -1 0])
(def -z "`[0 0 -1]`" [0 0 -1])
(def +x "`[1 0 0]`" [1 0 0])
(def +y "`[0 1 0]`" [0 1 0])
(def +z "`[0 0 1]`" [0 0 1])

(import ../../jlsl/prelude :prefix "" :export true)
(import ./gl :prefix "" :export true)
(import ./types :prefix "" :export true)
(import ./dynvars :prefix "" :export true)

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

(def pi "I think it's around three.\n\nNote that there are also values like `pi/4` and `pi/6*5` and related helpers all the way up to `pi/12`. They don't show up in autocomplete because they're annoying, but they're there." math/pi)
(def tau "Bigger than six, but smaller than seven.\n\nNote that there are also values like `tau/4` and `tau/6*5` and related helpers all the way up to `tau/12`.  They don't show up in autocomplete because they're annoying, but they're there." (* 2 math/pi))
(loop [i :range-to [2 12]]
  (put (curenv) (symbol "pi/" i) @{:value (/ pi i)})
  (put (curenv) (symbol "tau/" i) @{:value (/ tau i)})
  (put (curenv) (symbol "-pi/" i) @{:value (- (/ pi i))})
  (put (curenv) (symbol "-tau/" i) @{:value (- (/ tau i))}))
(loop [i :in [3 4 6 8 12] j :range [2 i]]
  (put (curenv) (symbol "pi/" i "*" j) @{:value (* (/ pi i) j)})
  (put (curenv) (symbol "tau/" i "*" j) @{:value (* (/ tau i) j)})
  (put (curenv) (symbol "-pi/" i "*" j) @{:value (* -1 (/ pi i) j)})
  (put (curenv) (symbol "-tau/" i "*" j) @{:value (* -1 (/ tau i) j)}))

(defn remap+
  "Linearly transform a number in the range `[-1 1]` to `[0 1]`."
  [x]
  (+ 0.5 (* 0.5 x)))

(defn remap-
  "Linearly transform a number in the range `[-1 1]` to `[0 -1]`."
  [x]
  (- (remap+ x)))

(defhelper :float atan2 [:float y :float x]
  ```
  Returns a value in the range `[-pi, pi]` representing the angle
  between the (2D) `+x` axis and the point `[x y]`.

  This is an alternative to the built-in `atan`'s two argument
  overload that is defined when `x = 0`. You can also invoke this
  with a single `vec2` whose coordinates will act as `x` and `y`.

  See `atan2+` for an angle in the range `[0, tau)`.
  ```
  # The built-in atan is undefined when x = 0, and while
  # I would assume this means "it might be positive
  # or negative pi," I don't know if I can count on that.
  (return (if (= x 0) (0.5 * pi * sign y) (atan y x))))
(overload :float atan2 [:vec2 p]
  (return (atan2 p.y p.x)))

(defhelper :float atan2+ [:float y :float x]
  ```
  Like `atan2`, but returns a value in the range `[0, tau)` instead of
  `[-pi, pi]`.
  ```
  (var signed-angle (atan2 y x))
  (if (< signed-angle 0)
    (return (tau + signed-angle))
    (return signed-angle)))
(overload :float atan2+ [:vec2 p]
  (return (atan2+ p.y p.x)))

(defhelper :float quantize [:float value :float count]
  ```
  Rounds a value to the nearest multiple of `count`.
  ```
  (return (value * count | round / count)))

(defn map-distance
  ```
  Apply a function `f` to the shape's distance field. `f` should take and return a
  `:float` expression.

  The returned shape has the same dimensions as the input.

  This differs from `shape/map-distance` in that the expression is wrapped in `gl/let`,
  so you can refer to it multiple times.
  ```
  [shape f]
  (shape/map-distance shape (fn [expr]
    (jlsl/do "map-distance"
      (var dist expr)
      ,(typecheck (f dist) jlsl/type/float)))))

(defn map-color
  ```
  Apply a function `f` to the shape's color field. `f` should take and return a
  `vec3` expression.

  The returned shape has the same dimensions as the input.

  This differs from `shape/map-color` in that the expression is wrapped in `gl/let`,
  so you can refer to it multiple times.
  ```
  [shape f]
  (shape/map-color shape (fn [expr]
    (jlsl/do "map-color"
      (var color expr)
      ,(typecheck (f color) jlsl/type/vec3)))))

(defhelper :mat3 cross-matrix [:vec3 vec]
  "Returns the matrix such that `(* (cross-matrix vec1) vec2)` = `(cross vec1 vec2)`."
  (return (mat3
    0 vec.z (- vec.y)
    (- vec.z) 0 vec.x
    vec.y (- vec.x) 0)))

(put (curenv) 'shape? (table/setproto @{:private false} (dyn 'shape/shape?)))
