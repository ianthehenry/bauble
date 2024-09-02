(use judge)
(use ./import)

(defn split-axis [axis]
  (sugar (case axis
    x [p.x p.yz]
    y [p.y p.xz]
    z [p.z p.xy]
    (errorf "unknown axis %q" (jlsl/show axis)))))

# This calls defshape/3d three times, once for each axis.
# And then it makes a new wrapper function that chooses which of those
# to call.
#
# There is a hygiene issue here where parameters could conflict with the
# generated function names, e.g. a function called `cone` with a parameter
# called `cone-y` would be problematic. Fixing this without obfuscating the
# generated GLSL is too annoying so just don't do it
(defmacro deforiented [name bindings docstring & body]
  (def axes [[x "x"] [y "y"] [z "z"]])
  (def params (map 1 (partition 2 bindings)))
  ~(upscope
    ,;(seq [[axis axis-name] :in axes]
      (def [this-axis other-axes] (split-axis axis))
      ~(as-macro ,defshape/3d- ,(symbol name "-" axis-name) ,bindings ,docstring ,;[
        ~(var other-axes ,['unquote ~',other-axes])
        ~(var this-axis ,['unquote ~',this-axis])
        ;body]))
    (defn ,name ,docstring [axis ,;params]
      ((case axis
        ,;(catseq [[axis axis-name] :in axes]
          [~',axis (symbol name "-" axis-name)])
        (errorf "unknown axis %q" (jlsl/show axis)))
        ,;params))))

(test-macro (deforiented cone [:float radius :float height]
  ```
  TODOC
  ```
  (var q [radius (- height)])
  (return (sqrt d * sign s)))
  (upscope
    (as-macro @defshape/3d- cone-x [:float radius :float height] "TODOC" (var other-axes (unquote (quote (<1> dot (<1> identifier (<2> lexical <3> "p" (<4> vec (<5> float) 3))) yz)))) (var this-axis (unquote (quote (<1> dot (<1> identifier (<2> lexical <3> "p" (<4> vec (<5> float) 3))) x)))) (var q [radius (- height)]) (return (sqrt d * sign s)))
    (as-macro @defshape/3d- cone-y [:float radius :float height] "TODOC" (var other-axes (unquote (quote (<1> dot (<1> identifier (<2> lexical <3> "p" (<4> vec (<5> float) 3))) xz)))) (var this-axis (unquote (quote (<1> dot (<1> identifier (<2> lexical <3> "p" (<4> vec (<5> float) 3))) y)))) (var q [radius (- height)]) (return (sqrt d * sign s)))
    (as-macro @defshape/3d- cone-z [:float radius :float height] "TODOC" (var other-axes (unquote (quote (<1> dot (<1> identifier (<2> lexical <3> "p" (<4> vec (<5> float) 3))) xy)))) (var this-axis (unquote (quote (<1> dot (<1> identifier (<2> lexical <3> "p" (<4> vec (<5> float) 3))) z)))) (var q [radius (- height)]) (return (sqrt d * sign s)))
    (defn cone
      "TODOC"
      [axis radius height]
      ((case axis (quote (1 0 0)) cone-x (quote (0 1 0)) cone-y (quote (0 0 1)) cone-z (errorf "unknown axis %q" (jlsl/show axis))) radius height))))
