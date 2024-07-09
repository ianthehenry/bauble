(import jaylib)
(import cmd)
(import ./ray)
(import ../src :as bauble)

(defn compile-shader [source]
  (let [[expr env] (bauble/bauble-evaluator/evaluate source)
        [animated? shader-source] (bauble/shade/compile-shape expr env "330")]
    shader-source))

(def arg/resolution (cmd/peg "WxH" ~(/ (* (number :d+) "x" (number :d+) -1) ,|[$0 $1])))

(cmd/defn render "render a bauble to a png"
  [infile (required :file)
   outfile (required :file)
   --resolution (optional arg/resolution [512 512]) "default 512x512"]

  (def shader-source (compile-shader (slurp infile)))

  (jaylib/set-trace-log-level :warning)
  (jaylib/set-config-flags :window-hidden)
  (jaylib/init-window 0 0 "Bauble")

  (def frame-buffer (ray/make-fbo resolution :point))

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
  (ray/save-screenshot frame-buffer outfile))

(cmd/defn print-source "print fragment shader source to stdout"
  [infile (required :file)]
  (print (compile-shader (slurp infile))))

(cmd/main (cmd/group
  render render
  compile print-source))
