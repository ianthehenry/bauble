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

# Okay, so this is a global thing. I admit that. I will not deny it.
# It would be smarter to have this be like a dynamic variable, and then to
# require the environment with the dynamic variable in place correctly,
# and use that. But. This is way easier and we can change it if it ever
# becomes a problem.
#
# The point of this is that there are some parts of the user environment that we
# can evaluate eagerly -- they're functions that don't change from one invocation
# of the user's script to the next, so we can just evaluate them up front.
#
# But there are other functions that we need to re-create from scratch every time.
# These are things like the `nearest-distance` definition: it's a forward-declared
# GLSL function, and we can only implement it once. And therefore anything that
# references that function -- like our shadow-casting code, or our raymarcher --
# also needs to be re-evaluated every time. So they are declared as "thunks", and
# gathered up into this array, and then evaluated every time we build a new user
# environment.
(def *thunks* @[])
(defmacro thunk [expr]
  ~(,array/push ',*thunks* ,expr))
