(use judge)
(use ../util)
(import ../../jlsl)
(import ../../glsl)
(import ../field-set)
(import ../dynvars)
(import ./default-2d)
(import ./default-3d)

# TODO: probably add some way to hoist top-level variables
(defn get-hoisted-field [shape field-name]
  (def field (field-set/get-field shape field-name))
  (unless field (break))
  (def hoisted-vars (field-set/get-hoisted-vars shape field-name))
  (if hoisted-vars
    (jlsl/with-expr (pairs hoisted-vars) [] field "hoist")
    field))

(defn render-2d [shape]
  (default-2d/render
    (get-hoisted-field shape :distance)
    (get-hoisted-field shape :color)))

(defn render-3d [shape]
  (default-3d/render
    (get-hoisted-field shape :distance)
    (get-hoisted-field shape :color)))

(defn render [env glsl-version]
  (def subject (get-var env 'subject))
  (unless subject (error "nothing to render"))

  # so our subject is either 2D or 3D
  (def program
    (case (field-set/type subject)
      jlsl/type/vec2 (render-2d subject)
      jlsl/type/vec3 (render-3d subject)
      (errorf "whoa whoa whoa, what the heck is %q" subject)))

  (def glsl (jlsl/render/program program))

  # TODO: we should probably just have a helper for this;
  # I don't like that this knows the representation of
  # program
  [(truthy? (some |(= dynvars/t (jlsl/param/var $))
    (jlsl/function/implicit-params (in program :main))))
   (glsl/render-program glsl glsl-version)])
