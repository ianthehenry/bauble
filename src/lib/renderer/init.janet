(use judge)
(use ../util)
(import ../../jlsl)
(import ../../glsl)
(import ../field-set)
(import ../dynvars)
(import ./default-2d)
(import ./default-3d)

(defn render-2d [subject]
  (default-2d/render subject))

(defn render-3d [subject]
  (default-3d/render subject))

(defn render [env glsl-version]
  (def subject (get-var env 'subject))
  (unless subject
    (error "nothing to render"))

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
