(use judge)
(import ../../jlsl)
(import ../syntax)

(defn get-unique [f ind on-error]
  (def distincts (distinct (map f ind)))
  (case (length distincts)
    1 (in distincts 0)
    (on-error distincts)))

(defmacro assertf [x & args]
  ~(as-macro ,assert ,x (,string/format ,;args)))

(defn typecheck [expr expected]
  (def actual (jlsl/expr/type expr))
  (assertf (= actual expected)
    "type mismatch: expected %q, got %q"
    (jlsl/show-type expected)
    (jlsl/show-type actual))
  expr)

(defmacro defhelper- [return-type name bindings & body]
  ~(jlsl/jlsl/defn- ,return-type ,name ,bindings
    ,;(syntax/expand body)))
(defmacro defhelper [return-type name bindings & body]
  ~(jlsl/jlsl/defn ,return-type ,name ,bindings
    ,;(syntax/expand body)))

(defmacro defshape/2d [name bindings docstring & body]
  (assert (string? docstring))
  ~(defn ,name ,docstring ,bindings
    ,;(seq [param :in bindings]
      ~(def ,param (,jlsl/coerce-expr ,param)))
    (field-set/distance-2d (jlsl/do
      ,;(syntax/expand body)))))

(defmacro defshape/3d [name bindings docstring & body]
  (assert (string? docstring))
  ~(defn ,name ,docstring ,bindings
    ,;(seq [param :in bindings]
      ~(def ,param (,jlsl/coerce-expr ,param)))
    (field-set/distance-3d (jlsl/do
      ,;(syntax/expand body)))))

(defmacro deftransform [name bindings docstring & body]
  (assert (string? docstring))
  ~(defn ,name ,docstring ,bindings
    ,;(seq [param :in (drop 1 bindings)]
      ~(def ,param (,jlsl/coerce-expr ,param)))
    ,;(syntax/expand body)))

(defmacro transform [shape name variable new-position]
  (with-syms [$expr]
    ~(field-set/map ,shape (fn [,$expr]
      (jlsl/with ,name [,variable ,new-position] (,'unquote ,$expr))))))
