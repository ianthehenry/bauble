(use judge)
(import pat)
(use ../util)
(import ../../jlsl)
(import ../../glsl)
(import ../shape)
(import ../dynvars)
(import ./default-2d)
(import ./default-3d)
(import ../expression-hoister)
(import ../../ordered)

(defn unhoist [env expr]
  (def hoisted-vars (in env expression-hoister/*hoisted-vars*))
  (def references (ordered/table/new))
  (def seen @{})
  (defn visit [node]
    (when (seen node) (break))
    (put seen node true)
    (if (jlsl/variable? node)
      (when-let [expr (in hoisted-vars node)]
        (unless (ordered/table/has-key? references node)
          (ordered/table/put references node expr)))
      (pat/match node
        ,(jlsl/@function/custom impl) (walk visit (impl :body))
        (walk visit node))))
  (walk visit expr)

  (def to-hoist (ordered/table/pairs references))
  (if (empty? to-hoist)
    expr
    (jlsl/with-expr to-hoist [] expr "hoist")))

(defn render-2d [env shape]
  (default-2d/render env
    (unhoist env (shape/get-field shape :distance))
    (unhoist env (shape/get-field shape :color))))

(defn render-3d [env shape]
  (default-3d/render env
    (unhoist env (shape/get-field shape :distance))
    (unhoist env (shape/get-field shape :color))))

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
