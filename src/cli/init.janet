(import jaylib)
(import cmd)
(use module)
(import ./ray)
(import ../glsl)
(use ./helpers)
(import ../lib :as bauble)

(def vec2 (cmd/peg "WxH" ~(+
  (/ (* (number :d+) -1) ,|[$ $])
  (/ (* (number :d+) "x" (number :d+) -1) ,|[$0 $1]))))

(defn read-input [filename]
  (if filename
    (slurp filename)
    (file/read stdin :all)))

(cmd/defn render "Render a single still image."
  [input (optional :file)
   [outfile -o --out] (required :file)
   --resolution (optional vec2 [512 512]) "default 512x512"
   -t (optional :number 0) "timestamp to use"
   --slices (optional vec2 [1 1]) "draw the image in multiple passes, e.g. 4x2 = 8 total draw calls"]
  (def source (read-input input))
  (def env (bauble/evaluator/evaluate source))
  (def [shader-source dimension animated? has-custom-camera?] (bauble/shade/compile-shape nil env "330"))
  (init-jaylib)
  (defn vec2 [[x y]] (string/format "%dx%d" x y))
  (def image (render-image shader-source
    :resolution resolution
    :orbit [0.125 -0.125]
    :t t
    :slices slices
    :on-render-start (fn [slice-size extra]
      (if (some pos? extra)
        (eprint "rendering slices at " (vec2 slice-size) " to " (vec2 (map + slice-size extra)))
        (eprint "rendering slices at " (vec2 slice-size))))
    :on-slice-end (fn [_ _] (eprin "."))
    :on-render-end (fn [] (eprint) (eprint "copying"))))
  (eprint "encoding")
  (jaylib/export-image image outfile))

(cmd/defn print-source "Print fragment shader source."
  [input (optional :file)
   [outfile -o --out] (optional :file)]
  (def source (read-input input))
  (def env (bauble/evaluator/evaluate source))
  (def [shader-source dimension animated? has-custom-camera?] (bauble/shade/compile-shape nil env "330"))
  (if outfile
    (spit outfile shader-source)
    (print shader-source)))

(import ../cubert)
(import ../jlsl)
(import ../glsl)
(use ../cubert/util)

(cmd/defn export-mesh "Export an OBJ file"
  [input (optional :file)
   [outfile -o --out] (required :file)
   -t (optional :number 0)
   --slices (optional :number 64) "the resolution of the final mesh"
   --size (optional :number 128) "the size (in Bauble units) of the bounding box to march"]
  (def slices [slices slices slices])
  (def [xs ys zs] slices)
  (assert (and (>= xs 2) (>= ys 2) (>= zs 2)) "not enough slices")
  (def origin [(- size) (- size) (- size)])
  (def end [size size size])
  (def cube-size (vec/ (vec- end origin) (map dec slices)))
  (def source (read-input input))

  (def env (bauble/evaluator/evaluate source))
  (def glsl (jlsl/render/program (cubert/get-program env)))
  (def shader-source (glsl/render-program glsl "330"))

  (init-jaylib)

  (def resolution [xs ys])
  (eprintf "origin=%q\ncube-size=%q\nslices=%q" origin cube-size slices)

  (def frame-buffer (ray/make-fbo resolution :point 8))
  (def shader (jaylib/load-shader-from-memory nil shader-source))

  (defn set-uniform [name type value]
    (jaylib/set-shader-value shader (jaylib/get-shader-location shader name) value type))

  (set-uniform "t" :float t)
  (set-uniform "cube_size" :vec3 cube-size)
  (set-uniform "origin" :vec3 origin)

  (defn get-samples [z]
    (set-uniform "z" :int z)
    (ray/do-texture frame-buffer
      (ray/do-shader shader
        (jaylib/draw-rectangle-v [0 0] resolution :red)))
    (def image (jaylib/load-image-from-texture (jaylib/get-render-texture-texture2d frame-buffer)))
    (def samples (jaylib/image-data image))
    (jaylib/unload-image image)
    samples)

  (defn corner-position [p]
    (vec+ origin (vec* cube-size p)))
  (def fiber (cubert/march origin cube-size slices))
  (resume fiber)
  (while (= (fiber/status fiber) :pending)
    (def z (fiber/last-value fiber))
    (resume fiber (get-samples z))
    (eprin "."))
  (print)

  (def [vertices triangles] (fiber/last-value fiber))

  (with [outfile (file/open outfile :w)]
    (each [x y z] vertices
      (xprintf outfile "v %f %f %f" x y z))
    (each [a b c] triangles
      (xprintf outfile "f %d// %d// %d//" a b c)))

  (eprintf "%d vertices, %d faces" (length vertices) (length triangles)))

(cmd/main (cmd/group
  render render
  compile print-source
  mesh export-mesh
  ))
