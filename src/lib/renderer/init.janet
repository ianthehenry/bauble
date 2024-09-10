(use judge)
(import pat)
(use ./util)
(import ../../jlsl)
(import ../../glsl)
(import ../expression-hoister)
(import ../../ordered)
(use ./samplers)
(import ../shape)
(use ../environment)

(defn unhoist [env expr]
  (def hoisted-vars (@in env expression-hoister/*hoisted-vars*))
  (def references (ordered/table/new))
  (def seen @{})
  (defn visit [node]
    (when (seen node) (break))
    (put seen node true)
    (if (jlsl/variable? node)
      (when-let [expr (@in hoisted-vars node)]
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

(defn render [env glsl-version]
  (def subject (get-var env 'subject))
  (def nearest-distance (get-env env 'nearest-distance))
  (unless subject (error "nothing to render"))
  (assertf (shape/is? subject) "%q is not a shape" subject)

  (def subject (shape/map subject (partial unhoist env)))

  (def aa-grid-size (jlsl/coerce-expr (keyword (or (get-var env 'aa-grid-size) :1))))

  (def program (sugar (program/new
    (precision highp float)
    (uniform ,camera-origin)
    (uniform ,camera-orientation)
    (uniform ,render-type)
    (uniform ,t)
    (uniform ,viewport)
    (out :vec4 frag-color)
    (implement :float nearest-distance [] (return ,(@or (shape/distance subject) 0)))

    ,(def sample
      (case (shape/type subject)
        jlsl/type/vec2 (make-sample-2d nearest-distance (shape/color subject))
        jlsl/type/vec3 (make-sample-3d nearest-distance render-type (shape/color subject))
        (error "BUG")))

    (defn :void main []
      (def gamma 2.2)
      (var color [0 0 0])
      (def aa-grid-size ,aa-grid-size)
      (def aa-sample-width (/ (float (+ :1 aa-grid-size))))
      (def pixel-origin [0.5 0.5])
      (var local-frag-coord (gl-frag-coord.xy - viewport.xy))

      (var rotation (rotation-matrix 0.2))
      (for (var y :1) (<= y aa-grid-size) (++ y)
        (for (var x :1) (<= x aa-grid-size) (++ x)
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

  # TODO: we should probably just have a helper for this;
  # I don't like that this knows the representation of
  # program
  [(truthy? (some |(= t (jlsl/param/var $))
    (jlsl/function/implicit-params (in program :main))))
   (glsl/render-program glsl glsl-version)])
