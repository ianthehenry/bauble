(import ./dsl :prefix "" :export true)
(import ./render :prefix "" :export true)
(import ./type :prefix "" :export true)
(import ./types :prefix "" :export true)
(import ./expr :prefix "" :export true)
(def show-type type/to-glsl)

(defn show
  "Return a debug representation of the argument that's suitable for printing."
  [x]
  (cond
    (expr? x) (expr/to-sexp x)
    (type? x) (type/to-glsl x)
    (variable? x) (symbol (variable/name x))
    x))

(defmacro iife [& ast] (expr/of-ast ['iife ;ast]))
(defmacro do [& ast] (expr/of-ast ['do ;ast]))
(defmacro with [& ast] (expr/of-ast ['with ;ast]))
(defmacro statement [& ast] (statement/of-ast ['upscope ;ast]))
