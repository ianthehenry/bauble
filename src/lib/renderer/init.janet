(use judge)
(use ../util)
(import ../../jlsl)
(import ../../glsl)
(import ../shape)
(import ../dynvars)
(import ./default-2d)
(import ./default-3d)
(import ../../ordered)

# TODO: probably add some way to hoist top-level variables
(defn get-hoisted-field [shape field-name]
  (def field (shape/get-field shape field-name))
  (unless field (break))
  (def hoisted-vars (shape/get-hoisted-vars shape field-name))
  (if hoisted-vars
    (jlsl/with-expr (pairs hoisted-vars) [] field "hoist")
    field))

# TODO: maybe it would be better to have a function that derived a new subject
# out of the old one, and just passed that along

(defn render-2d [env shape]
  (default-2d/render env
    (get-hoisted-field shape :distance)
    (get-hoisted-field shape :color)))

(defn render-3d [env shape]
  (default-3d/render env
    (get-hoisted-field shape :distance)
    (get-hoisted-field shape :color)))

(defn render [env glsl-version]
  (def subject (get-var env 'subject))
  (unless subject (error "nothing to render"))

  # so our subject is either 2D or 3D
  (def program
    (case (shape/type subject)
      jlsl/type/vec2 (render-2d env subject)
      jlsl/type/vec3 (render-3d env subject)
      (errorf "whoa whoa whoa, what the heck is %q" subject)))

  (def glsl (jlsl/render/program program))

  # TODO: we should probably just have a helper for this;
  # I don't like that this knows the representation of
  # program
  [(truthy? (some |(= dynvars/t (jlsl/param/var $))
    (jlsl/function/implicit-params (in program :main))))
   (glsl/render-program glsl glsl-version)])
