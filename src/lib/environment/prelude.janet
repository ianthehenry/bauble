(use ./util)
(import ../../jlsl)

(defmacro .
  "Behaves like `.` in GLSL, for accessing components of a vector or struct, e.g. `(. foo xy)`.\n\nBauble's dot syntax, `foo.xy`, expands to call this macro."
  [expr field]
  [jlsl/expr/dot [jlsl/coerce-expr expr] ['quote field]])

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

(defn remap+
  "Remap a number in the range `[-1 1]` into the range `[0 1]`."
  [x]
  (+ 0.5 (* 0.5 x)))