(use ../../jlsl)

# TODO: docstrings for these

(jlsl/defstruct Ray
  :vec3 origin
  :vec3 direction)

(jlsl/defstruct Light
  :vec3 color
  :vec3 direction
  :float brightness)

(jlsl/defstruct PerspectiveCamera
  :vec3 position
  :vec3 direction
  :vec3 up
  :float fov)
(jlsl/defstruct OrthographicCamera
  :vec3 position
  :vec3 direction
  :vec3 up
  :float scale)

(defn ray?
  ```
  Returns `true` if `value` is a GLSL expression with type `Ray`.
  ```
  [value]
  (if-let [expr (try-coerce-expr value)]
    (= (type/coerce Ray) (expr/type expr))
    false))

(defn camera?
  ```
  Returns `true` if `value` is a GLSL expression with type `PerspectiveCamera` or `OrthographicCamera`.
  ```
  [value]
  (if-let [expr (try-coerce-expr value)]
    (case (expr/type expr)
      (type/coerce PerspectiveCamera) true
      (type/coerce OrthographicCamera) true
      false)
    false))

(defn light?
  ```
  Returns `true` if `value` is a GLSL expression with type `Light`.
  ```
  [value]
  (if-let [expr (try-coerce-expr value)]
    (= (type/coerce Light) (expr/type expr))
    false))
