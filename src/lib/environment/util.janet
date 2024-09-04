(use judge)
(import ../../jlsl)
(import ../syntax)
(import ../shape)
(import ../util :prefix "" :export true)

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

(defmacro make-shaper [name constructor def-macro]
  ~(defmacro ,name [name bindings docstring & body]
    (assert (string? docstring))
    (def gl/name (symbol "sdf-" name))
    (def params (seq [[type name] :in (partition 2 bindings)]
      [(symbol (string/triml name "!")) type (string/has-prefix? "!" name)]))
    (def add-round-param? (truthy? (some 2 params)))
    (def <adjusted-distance> ~(- (,gl/name
      ,;(seq [[name type subtract-roundness?] :in params]
        (def constructor-function-name (symbol type))
        (def <jlsl-type> (jlsl/type/of-ast type))
        ~(,coerce-expr-to-type ,<jlsl-type> ,constructor-function-name
          ,(if subtract-roundness? ~(- ,name r) name)))) r))

    (def <unadjusted-distance> ~(,gl/name
      ,;(seq [[name type _] :in params]
        # obviously this means you can only use primitive typ
        (def constructor-function-name (symbol type))
        (def <jlsl-type> (jlsl/type/of-ast type))
        ~(,coerce-expr-to-type ,<jlsl-type> ,constructor-function-name ,name))))

    (def bindings [;(map 0 params) ;(if add-round-param? [:?r:round] [])])

    ~(upscope
      (as-macro ,defhelper- :float ,gl/name [,;(catseq [[name type _] :in params] [type name])] ,;(syntax/expand body))
      (,',def-macro ,name ,bindings ,docstring
        (,,constructor
          ,(if add-round-param?
            ~(if (nil? round)
              ,<unadjusted-distance>
              (as-macro ,jlsl/let [r round]
                ,['unquote <adjusted-distance>]))
            <unadjusted-distance>))))))

(make-shaper defshape/2d shape/2d defnamed)
(make-shaper defshape/3d shape/3d defnamed)
(make-shaper defshape/2d- shape/2d defnamed-)
(make-shaper defshape/3d- shape/3d defnamed-)

(test-macro (defshape/2d circle [:float radius]
  "Returns a 2D shape."
  (return (length q - radius)))
  (upscope
    (as-macro @defhelper- :float sdf-circle [:float radius] (return (- (length q) radius)))
    (defnamed circle (radius) "Returns a 2D shape." (@shape/2d (sdf-circle (@coerce-expr-to-type (@type/primitive (quote (<1> float))) float radius))))))

(test-macro (defshape/2d rect [:vec2 !size]
  ```
  Returns a 2D shape.
  ```
  (var d (abs q - size))
  (return (max d 0 | length + min (max d) 0)))
  (upscope
    (as-macro @defhelper- :float sdf-rect [:vec2 size] (var d (- (abs q) size)) (return (+ (length (max d 0)) (min (max d) 0))))
    (defnamed rect (size :?r:round) "Returns a 2D shape." (@shape/2d (if (nil? round) (sdf-rect (@coerce-expr-to-type (quote (<1> vec (<2> float) 2)) vec2 size)) (as-macro @let [r round] (unquote (- (sdf-rect (@coerce-expr-to-type (quote (<1> vec (<2> float) 2)) vec2 (- size r))) r))))))))

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
