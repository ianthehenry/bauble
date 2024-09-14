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
(import ./dynvars :prefix "" :export true)

(defn remap+
  "Linearly transform a number in the range `[-1 1]` to `[0 1]`."
  [x]
  (+ 0.5 (* 0.5 x)))

(defn remap-
  "Linearly transform a number in the range `[-1 1]` to `[0 -1]`."
  [x]
  (- (remap+ x)))

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
      (f dist)))))

(defn map-color
  ```
  Apply a function `f` to the shape's color field. `f` should take and return a
  `:vec3` expression.

  The returned shape has the same dimensions as the input.

  This differs from `shape/map-color` in that the expression is wrapped in `gl/let`,
  so you can refer to it multiple times.
  ```
  [shape f]
  (shape/map-color shape (fn [expr]
    (jlsl/do "map-color"
      (var dist expr)
      (f dist)))))
