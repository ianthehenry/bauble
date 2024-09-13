(import jaylib)
(import cmd)
(use module)
(import ./ray)
(import ../glsl)
(use ./helpers)
(use ./test-glsl)
(use ./test-jlsl)
(import ../lib :as bauble)

(def arg/resolution (cmd/peg "WxH" ~(/ (* (number :d+) "x" (number :d+) -1) ,|[$0 $1])))

(cmd/defn render "render a bauble to a png"
  [infile (required :file)
   outfile (required :file)
   --resolution (optional arg/resolution [512 512]) "default 512x512"]
  (def source (slurp infile))
  (def env (bauble/evaluator/evaluate source))
  (def [animated? shader-source] (bauble/shade/compile-shape nil env "330"))
  (init-jaylib)
  (def image (render-image shader-source
    :resolution resolution
    :orbit [0.125 -0.125]))
  (jaylib/export-image image outfile))

(cmd/defn print-source "print fragment shader source to stdout"
  [infile (required :file)]
  (print (compile-shader (slurp infile))))

(cmd/main (cmd/group
  render render
  test-glsl test-glsl
  test-jlsl test-jlsl
  compile print-source))
