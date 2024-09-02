(use judge)
(import ../../jlsl)
(import ../syntax)
(import ../shape)
(import ../util :prefix "" :export true)

(defn typecheck [expr expected]
  (def actual (jlsl/expr/type expr))
  (assertf (= actual expected)
    "type mismatch: expected %q, got %q"
    (jlsl/show-type expected)
    (jlsl/show-type actual))
  expr)

(defmacro defhelper- [return-type name bindings & body]
  ~(as-macro ,jlsl/jlsl/defn- ,return-type ,name ,bindings
    ,;(syntax/expand body)))
(defmacro defhelper [return-type name bindings & body]
  ~(as-macro ,jlsl/jlsl/defn ,return-type ,name ,bindings
    ,;(syntax/expand body)))
(defmacro overload [return-type name bindings & body]
  ~(as-macro ,jlsl/jlsl/overload ,return-type ,name ,bindings
    ,;(syntax/expand body)))

(defn coerce-expr-to-type [desired-type constructor-function expr]
  (def expr (jlsl/coerce-expr expr))
  (if (= (jlsl/expr/type expr) desired-type)
    expr
    (constructor-function expr)))

(defmacro make-shaper [name constructor defn-macro]
  ~(defmacro ,name [name bindings docstring & body]
    (assert (string? docstring))
    (def gl/name (symbol "sdf-" name))
    ~(upscope
      # TODO: maybe this should not include the do wrap...
      (as-macro ,defhelper- :float ,gl/name ,bindings
        ,;(syntax/expand body))
      (,',defn-macro ,name ,docstring ,(tuple/brackets ;(seq [[_ name] :in (partition 2 bindings)] name))
        (,,constructor (,gl/name
          ,;(seq [[type name] :in (partition 2 bindings)]
            # obviously this means you can only use primitive types
            (def constructor-function-name (symbol type))
            (def <jlsl-type> (jlsl/type/of-ast type))
            ~(,coerce-expr-to-type ,<jlsl-type> ,constructor-function-name ,name)
)))))))

(make-shaper defshape/2d shape/distance-2d defn)
(make-shaper defshape/3d shape/distance-3d defn)
(make-shaper defshape/2d- shape/distance-2d defn-)
(make-shaper defshape/3d- shape/distance-3d defn-)

(test-macro (defshape/2d circle [:float radius]
  "Returns a 2D shape."
  (return (length q - radius)))
  (upscope
    (as-macro @defhelper- :float sdf-circle [:float radius] (return (- (length q) radius)))
    (defn circle
      "Returns a 2D shape."
      [radius]
      (@distance-2d (sdf-circle (@coerce-expr-to-type (@type/primitive (quote (<1> float))) float radius))))))

(defmacro deftransform [name bindings docstring & body]
  (assert (string? docstring))
  ~(defn ,name ,docstring ,bindings
    (as-macro ,assertf (,shape/is? ,(first bindings)) "first argument to %s should be a shape, got %q" ,(string name) ,(first bindings))
    ,;(seq [param :in (drop 1 bindings)]
      ~(def ,param (,jlsl/coerce-expr ,param)))
    ,;(syntax/expand body)))

(defmacro transform [shape name variable new-position]
  (with-syms [$expr]
    ~(shape/map ,shape (fn [,$expr]
      (jlsl/with ,name [,variable ,new-position] (,'unquote ,$expr))))))

(defmacro sugar [expr] (syntax/expand expr))

# like defn, but docstring is required, it appears in the right place, and
# it doesn't implicitly include the arguments
(defmacro deffn [name args docstring & body]
  (assert (string? docstring) "docstring required")
  ~(def ,name ,docstring (fn ,name ,args ,;body)))
