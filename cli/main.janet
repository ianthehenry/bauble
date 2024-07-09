(import jaylib)
(import cmd)
(import ./ray)
(import ../src :as bauble)

(defn to-shader [source]
  (let [[expr env] (bauble/bauble-evaluator/evaluate source)
        [animated? shader-source] (bauble/shade/compile-shape expr env "330")]
    shader-source))

(cmd/main (cmd/fn [outfile (required :file)]
  (jaylib/set-trace-log-level :warning)
  (jaylib/set-config-flags :window-hidden)
  (jaylib/init-window 0 0 "Bauble")

  (def resolution [512 512])

  (def frame-buffer (ray/make-fbo resolution :point))

  (def shader-source (to-shader ```
    (union :r 10 (sphere 50) (box 50))
  ```))

  (def shader (jaylib/load-shader-from-memory nil shader-source))

  (defn set-uniform [name type value]
    (jaylib/set-shader-value shader (jaylib/get-shader-location shader name) value type))
  (defn set-uniform-matrix [name value]
    (jaylib/set-shader-value-matrix shader (jaylib/get-shader-location shader name) value))

  (set-uniform "camera_origin" :vec3 [256 362 256])
  (set-uniform "camera_orientation" :vec3 [(* 0.125 math/pi 2) (* -0.125 math/pi 2) 0])

  (set-uniform "t" :float 0)
  (set-uniform "viewport" :vec4 [0 0 ;resolution])
  (set-uniform "render_type" :int 0)

  (ray/do-texture frame-buffer
    (ray/do-shader shader
      (jaylib/draw-rectangle-v [0 0] resolution :red)))
  (ray/save-screenshot frame-buffer outfile)))
