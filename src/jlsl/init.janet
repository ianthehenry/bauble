(import ./dsl :prefix "" :export true)
(import ./render :prefix "" :export true)
(import ./type :prefix "" :export true)
(import ./types :prefix "" :export true)
(import ./expr :prefix "" :export true)
(def show-type type/to-glsl)

(defmacro do [& ast] (expr/of-ast ['do ;ast]))
(defmacro with [& ast] (expr/of-ast ['with ;ast]))
