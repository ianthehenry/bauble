(import ../../jlsl)
(use ./util)
(import ../shape)
(import ../expression-hoister)
(use ../../jlsl/prelude)

(defn- make-let-macro [new-vars? bindings body]
  (def [field bindings body] (if (keyword? bindings)
    [bindings (first body) (drop 1 body)]
    [nil bindings body]))
  (def bindings (seq [[name <value>] :in (partition 2 bindings)] [name <value> (gensym)]))
  (def <with-bindings> (map (fn [[name _ $value]] (tuple/brackets name $value)) bindings))
  (def with-name (if new-vars? "let" "with"))
  (with-syms [$subject $field] ~(do
    ,;(catseq [[name <value> $value] :in bindings]
      [~(def ,$value (,jlsl/coerce-expr ,<value>))
       ;(if new-vars?
        [~(def ,name (,jlsl/variable/new ,(string name) (,jlsl/expr/type ,$value)))]
        [])])
    (def ,$subject (do ,;body))
    ,;(if field
      [~(as-macro ,assertf (,shape/is? ,$subject) "%q is not a shape" ,$subject)
       ~(,shape/map-field ,$subject ,field (fn [,$field]
         (,jlsl/with-expr ,<with-bindings> [] ,$field ,with-name)))]
      [~(if (,shape/is? ,$subject)
        (,shape/map ,$subject (fn [,$field]
          (,jlsl/with-expr ,<with-bindings> [] ,$field ,with-name)))
        (,jlsl/with-expr ,<with-bindings> [] (,jlsl/coerce-expr ,$subject) ,with-name))])
    )))

(defmacro gl/let
  ````
  Like `let`, but creates GLSL bindings instead of a Janet bindings. You can use this
  to reference an expression multiple times while only evaluating it once in the resulting
  shader.

  For example:

  ```
  (let [s (sin t)]
    (+ s s))
  ```

  Produces GLSL code like this:

  ```
  sin(t) + sin(t)
  ```

  Because `s` refers to the GLSL *expression* `(sin t)`.

  Meanwhile:

  ```
  (gl/let [s (sin t)]
    (+ s s))
  ```

  Produces GLSL code like this:

  ```
  float let(float s) {
    return s + s;
  }

  let(sin(t))
  ```

  Or something equivalent. Note that the variable is hoisted into an immediately-invoked function
  because it's the only way to introduce a new identifier in a GLSL expression context.

  You can also use Bauble's underscore notation to fit this into a pipeline:

  ```
  (s + s | gl/let [s (sin t)] _)
  ```

  If the body of the `gl/let` returns a shape, the bound variable will be available in all of its
  fields. If you want to refer to variables or expressions that are only available in color fields,
  pass a keyword as the first argument:

  ```
  (gl/let :color [banding (sin depth)]
    (sphere 100 | blinn-phong [1 banding 0]))
  ```
  ````
  [bindings & body]
  (make-let-macro true bindings body))

(defmacro gl/with
  ````
  Like `gl/let`, but instead of creating a new binding, it alters the value of an existing
  variable. You can use this to give new values to dynamic variables. For example:

  ```
  # implement your own `move`
  (gl/with [p (- p [0 50 0])] (sphere 50))
  ```

  You can also use Bauble's underscore notation to fit this into a pipeline:

  ```
  (sphere 50 | gl/with [p (- p [0 50 0])] _)
  ```

  You can -- if you really want -- use this to alter `P` or `Q` to not refer to the point in
  global space, or use it to pretend that `ray-dir` is actually a different angle.

  The variables you change in `gl/with` will, by default, apply to all of the fields of a shape.
  You can pass a keyword as the first argument to only change a particular field. This allows you
  to refer to variables that only exist in color expressions:

  ```
  (gl/with :color [normal (normal + (perlin p * 0.1))]
    (sphere 100 | blinn-phong [1 0 0] | move [-50 0 0]))
  ```
  ````
  [bindings & body]
  (make-let-macro false bindings body))

(defmacro gl/do
  ````
  Execute a series of GLSL statements and return the final expression.

  ```
  (gl/do "optional-label"
    (var c [1 0 1])
    (for (var i 0:u) (< i 10:u) (++ i)
      (+= c.g 0.01))
    c)
  ```

  The body of this macro is not regular Janet code, but a special DSL
  that is not really documented anywhere, making it pretty hard to use.
  ````
  [& body]
  (call jlsl/do ;body))

(defmacro gl/iife
  ````
  Like `gl/do`, except that you can explicitly return early.

  ```
  (gl/iife "optional-label"
    (var c [1 0 1])
    (if (< normal.y 0)
      (return c))
    (for (var i 0:u) (< i 10:u) (++ i)
      (+= c.g 0.01))
    c)
  ```

  ````
  [& body]
  (call jlsl/iife ;body))

(defmacro gl/defn
  ````
  Defines a GLSL function. You must explicitly annotate the return type
  and the type of all arguments. The body of the function uses the GLSL
  DSL, i.e. it is not normal Janet code.

  ```
  (gl/defn :vec3 hsv [:float hue :float saturation :float value]
    (var c (hue * 6 + [0 4 2] | mod 6 - 3 | abs))
    (return (value * (mix (vec3 1) (c - 1 | clamp 0 1) saturation))))
  ```
  ````
  [return-type name params & body]
  (call jlsl/jlsl/defn return-type name params ;body))

(defmacro gl/overload
  ````
  Overloads a previously defined function with an additional signature.

  Note that the argument type must uniquely determine the return type of
  a GLSL function, so you can't make an overload that only varies in
  its return type.

  ```
  (gl/overload :float min [:float a :float b :float c]
    (return (min (min a b) c)))
  ```

  You can overload any function, including built-in functions.
  ````
  [return-type name params & body]
  (call jlsl/jlsl/overload return-type name params ;body))

(defmacro gl/def
  ````
  You can use `gl/def` to create new top-level GLSL variables which will only
  be evaluated once (per distance and color field evaluation). This is useful in
  order to re-use an expensive value in multiple places, when that value only
  depends on values that are available at the beginning of shading.

  ```
  (gl/def signal (perlin+ (p / 20)))
  (shape/3d (signal * 0.5)
  | intersect (sphere 50)
  | color [signal (pow signal 2) 0])
  ```

  This is shorthand for `(def foo (hoist expression "foo"))`.

  Note that since the signal is evaluated at the top-level, `p` will always be the
  same as `P`. Consider this example:

  ```
  (gl/def signal (perlin+ (p / 20)))
  (shape/3d (signal * 0.5)
  | intersect (sphere 50)
  | color [signal (pow signal 2) 0]
  | move x (sin t * 100)
  )
  ```

  Change the `gl/def` to a regular `def` to see some of the impliciations of hoisting
  a computation.
  ````
  [name expression]
  ~(def ,name (,expression-hoister/hoist ,(string name) ,expression)))

(defmacro gl/hoist
  ````
  Return a hoisted version of the expression See the documentation for `gl/def`
  for an explanation of this.
  ````
  [expression &opt name]
  ~(,expression-hoister/hoist ,(string (or name "hoist")) ,expression))

(defn gl/if
  ````
  A GLSL ternary conditional expression.

  ```
  (sphere 50
  | color (gl/if (< normal.y 0) [1 0 0] [1 1 0]))
  ```
  ````
  [condition then else]
  (jlsl/if-expr (typecheck condition jlsl/type/bool) (jlsl/coerce-expr then) (jlsl/coerce-expr else)))
