(use ./util)
(use ./uniforms-private)
(import ../../jlsl)

(defn uniform
  ```
  Create a uniform with an initial value.

  A uniform is like an input to a shader, and you can use uniforms to
  create dynamic shaders that you can control from outside of Bauble.
  If you use Bauble's "Export to HTML Embed" function, you can put
  your Baubles on a web page you control, and then set its uniforms
  from JavaScript based on whatever inputs you want.

  These correspond to literal GLSL uniforms, so you don't have to use
  Bauble's JavaScript player at all -- you can export a GLSL shader with
  custom uniforms and set them by hand, if you want to.

  You probably want to give your uniforms names, so that you can set them, and
  `(defuniform)` is a convenient wrapper for doing this. But you can also create
  anonymous uniforms. It's kind of a weird thing to do, but you can edit the
  initial value of an anonymous uniform without needing to recompile the
  shader every time it changes, which can be helpful for refining values in
  complex scenes that take a long time to compile. One day Bauble might
  automatically create anonymous uniforms whenever you use mouse editing,
  but it can't do that yet. Also sometimes it recompiles the shader anyway because
  the shader output is not fully deterministic; my bad; one day this will work right.
  ```
  [initial-value &opt name]
  (default name (string/format "_u%d" (length (dyn *uniforms*))))
  (assertf (not (jlsl/expr? initial-value)) "uniforms cannot be expressions, only literal values")
  (def type (jlsl/expr/type (jlsl/coerce-expr initial-value)))
  (def variable (jlsl/variable/new name type))
  (put (dyn *uniforms*) variable initial-value)
  variable)

(defmacro defuniform
  ```
  Short for `(def name (uniform initial-value "name"))`.
  ```
  [name initial-value]
  ~(def ,name (,uniform ,initial-value ,(string name))))
