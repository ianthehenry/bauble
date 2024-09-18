(use judge)
(import pat)
(use ./util)
(import ../../jlsl)
(import ../../glsl)
(import ../expression-hoister)
(import ../../ordered)
(use ./samplers)
(import ../shape)

(defn unhoist [env expr]
  (def hoisted-vars (in env expression-hoister/*hoisted-vars*))
  (def references (ordered/table/new))
  (def seen @{})
  (defn visit [node]
    (when (seen node) (break))
    (put seen node true)
    (if (jlsl/variable? node)
      (when-let [expr (in hoisted-vars node)]
        (visit expr)
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

(import ../environment/dynvars)
# TODO: jlsl should probably just have a helper for this;
# I don't like that this knows the representation of a
# program
(defn animated? [program]
  (truthy? (some |(= dynvars/t (jlsl/param/var $))
    (jlsl/function/implicit-params (in program :main)))))

(use ../environment)

(defn render [env glsl-version]
  (def subject (get-var env 'subject))
  (def nearest-distance (get-env env 'nearest-distance))
  (def default-2d-color (typecheck (get-var env 'default-2d-color) jlsl/type/vec3))
  (def default-3d-color (typecheck (get-var env 'default-3d-color) jlsl/type/vec3))
  (def background-color (unhoist env (typecheck (get-var env 'background-color) jlsl/type/vec3)))
  (def camera (typecheck? (get-var env 'camera) Camera))

  (def subject (if subject (do
    (assertf (shape? subject) "%q is not a shape" subject)
    (shape/map subject (partial unhoist env)))))

  (def dimension (if subject
    (case (shape/type subject)
      jlsl/type/vec2 2
      jlsl/type/vec3 3
      (error "BUG"))
    0))

  (def aa-grid-size (jlsl/coerce-expr (int/u64 (or (get-var env 'aa-grid-size) 1))))

  (def program (sugar (program/new
    (precision highp float)
    (uniform ,camera-type)
    (uniform ,free-camera-target)
    (uniform ,free-camera-orbit)
    (uniform ,free-camera-zoom)
    (uniform ,origin-2d)
    (uniform ,render-type)
    (uniform ,t)
    (uniform ,viewport)
    (out :vec4 frag-color)
    (implement :float nearest-distance [] (return ,(@or (@and subject (shape/distance subject)) 0)))

    ,(def sample
      (if subject
        (case (shape/type subject)
          jlsl/type/vec2 (make-sample-2d nearest-distance background-color default-2d-color (shape/color subject))
          jlsl/type/vec3 (make-sample-3d nearest-distance camera background-color default-3d-color (shape/color subject))
          (error "BUG"))
        (jlsl/fn :vec3 sample [] (return background-color))))

    (defn :void main []
      (def gamma 2.2)
      (var color [0 0 0])
      (def aa-grid-size ,aa-grid-size)
      (def aa-sample-width (/ (float (+ 1:u aa-grid-size))))
      (def pixel-origin [0.5 0.5])
      (var local-frag-coord (gl-frag-coord.xy - viewport.xy))

      (var rotation (rotation-matrix 0.2))
      (for (var y 1:u) (<= y aa-grid-size) (++ y)
        (for (var x 1:u) (<= x aa-grid-size) (++ x)
          (var sample-offset (aa-sample-width * [(float x) (float y)] - pixel-origin))
          (set sample-offset (* rotation sample-offset))
          (set sample-offset (sample-offset + pixel-origin | fract - pixel-origin))
          (with [Frag-Coord (local-frag-coord + sample-offset)
                 resolution viewport.zw
                 frag-coord (Frag-Coord - (0.5 * resolution) / max resolution)]
            (+= color ((sample) | clamp 0 1)))
          ))
      (/= color (float (aa-grid-size * aa-grid-size)))

      (set frag-color [(pow color (/ gamma)) 1])))))

  (def glsl (jlsl/render/program program))

  [(glsl/render-program glsl glsl-version)
   dimension
   (animated? program)
   (not (nil? camera))])
