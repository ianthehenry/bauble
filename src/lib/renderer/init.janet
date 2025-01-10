(use judge)
(use ./util)
(import ../../jlsl)
(import ../../glsl)
(use ./samplers)
(import ../shape)

(import ../environment/dynvars)
(import ../environment/uniforms-private)
# TODO: jlsl should probably just have a helper for this;
# I don't like that this knows the representation of a
# program
(defn animated? [program]
  (truthy? (some |(= dynvars/t (jlsl/param/var $))
    (jlsl/function/implicit-params (in program :main)))))

(def @not not)

(use ../environment)

(defn render [env glsl-version &keys options]
  (def stdenv (table/getproto env))
  (def subject (get-var stdenv 'subject))
  (def nearest-distance (get-env stdenv 'nearest-distance))
  (def default-2d-color (typecheck (get-var stdenv 'default-2d-color) jlsl/type/vec3))
  (def default-3d-color (typecheck (get-var stdenv 'default-3d-color) jlsl/type/vec3))
  (def background-color (jlsl/coerce-expr (get-var stdenv 'background-color)))

  (def custom-uniforms (keys (in stdenv uniforms-private/*uniforms*)))

  (def background-color (case (jlsl/expr/type background-color)
    jlsl/type/vec3 (vec4 background-color 1)
    jlsl/type/vec4 background-color
    (error "background-color must be a vec3 or ve4")))

  (def camera (when-let [camera (get-var stdenv 'camera)]
    (assertf (camera? camera) "%q is not a camera")
    camera))

  (assertf (@or (nil? subject) (shape? subject)) "%q is not a shape" subject)

  (def dimension (if subject
    (case (shape/type subject)
      jlsl/type/vec2 2
      jlsl/type/vec3 3
      (error "BUG"))
    0))

  (if (@and camera (= dimension 2))
    (error "custom cameras not supported in 2D yet"))

  (def aa-grid-size (jlsl/coerce-expr (int/u64 (@or (get-var stdenv 'aa-grid-size) 1))))

  (jlsl/jlsl/implement :float nearest-distance [] (return ,(@or (@and subject (shape/distance subject)) 0)))
  (def sample
    (if subject
      (case (shape/type subject)
        jlsl/type/vec2 (make-sample-2d options nearest-distance background-color default-2d-color (shape/color subject))
        jlsl/type/vec3 (make-sample-3d options nearest-distance camera background-color default-3d-color (shape/color subject))
        (error "BUG"))
      (jlsl/fn :vec4 sample [] (return background-color))))
  (def frag-color (jlsl/variable/new "frag-color" jlsl/type/vec4))
  (def free-camera? (@or (options :dynamic-camera?) (@not camera)))
  (def program (jlsl/program/new*
    :pragmas ['(precision highp float)]
    :uniforms
      (filter truthy? [
        ;custom-uniforms
        (if (options :dynamic-camera?) camera-type)
        ;(if free-camera?
          (if (= dimension 2)
            [free-camera-zoom free-camera-target-2d]
            [free-camera-zoom free-camera-target-3d free-camera-orbit])
          [])
        t
        viewport
        (if (in options :crosshairs) crosshairs-3d)])
    :outputs [frag-color]

    :main (sugar (jlsl/fn :void main []
      (def gamma 2.2)
      (var color [0 0 0])
      (var alpha 0)
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
                 frag-coord (Frag-Coord - (0.5 * resolution) / max resolution * 2)]
            (var this-sample (clamp (sample) 0 1))
            (+= color (this-sample.rgb * this-sample.a))
            (+= alpha this-sample.a))
          ))
      (if (> alpha 0) (do
        (set color (color / alpha))
        (/= alpha (float (aa-grid-size * aa-grid-size)))))

      (set frag-color [(pow color (/ gamma)) alpha])))))

  (def glsl (jlsl/render/program program))

  [(glsl/render-program glsl glsl-version)
   dimension
   (animated? program)
   (not (nil? camera))])
