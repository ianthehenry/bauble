(use judge)
(use ./import)

# This is a minor convenience that lets us use the 3D vector
# x/y/-x/-y vectors as arguments to functions like move
# TODO: should we just truncate the vector instead? That's... easier and maybe better?
(defn axis-vector-2d [v]
  (case v
    x [1 0]
    y [0 1]
    -x [-1 0]
    -y [0 -1]
    v))

(defn split-axis [axis]
  (sugar (case axis
    x [p.x p.zy]
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
(defn deforiented-aux [definer name bindings docstring & body]
  (def axes [[x "x"] [y "y"] [z "z"]])
  (def params (seq [[type name] :in (partition 2 bindings)]
    [(symbol (string/triml name "!")) type (string/has-prefix? "!" name)]))
  (def add-round-param? (truthy? (some 2 params)))
  ~(upscope
    ,;(seq [[axis axis-name] :in axes]
      (def [this-axis other-axes] (split-axis axis))
      ~(as-macro ,defshape/3d- ,(symbol name "-" axis-name) ,bindings ,docstring ,;[
        ~(var other-axes ,['unquote ~',other-axes])
        ~(var this-axis ,['unquote ~',this-axis])
        ;body]))
    (as-macro ,definer ,name [axis ,;(map 0 params) ,;(if add-round-param? [:?r:round] [])]
      ,docstring
      ((case axis
        ,;(catseq [[axis axis-name] :in axes]
          [~',axis (symbol name "-" axis-name)])
        (errorf "unknown axis %q" (jlsl/show axis)))
        ,;(map 0 params)
        ,;(if add-round-param? ~[:r round] [])
        ))))

(def deforiented :macro (partial deforiented-aux defnamed))
(def deforiented- :macro (partial deforiented-aux defnamed-))

(test-macro (deforiented cone [:float radius :float height] "docstring"
  (var q [radius (- height)])
  (return (sqrt d * sign s)))
  (upscope
    (as-macro @defshape/3d- cone-x [:float radius :float height] "docstring" (var other-axes (unquote (quote (<1> dot (<1> identifier (<2> lexical <3> "p" (<4> vec (<5> float) 3))) zy)))) (var this-axis (unquote (quote (<1> dot (<1> identifier (<2> lexical <3> "p" (<4> vec (<5> float) 3))) x)))) (var q [radius (- height)]) (return (sqrt d * sign s)))
    (as-macro @defshape/3d- cone-y [:float radius :float height] "docstring" (var other-axes (unquote (quote (<1> dot (<1> identifier (<2> lexical <3> "p" (<4> vec (<5> float) 3))) xz)))) (var this-axis (unquote (quote (<1> dot (<1> identifier (<2> lexical <3> "p" (<4> vec (<5> float) 3))) y)))) (var q [radius (- height)]) (return (sqrt d * sign s)))
    (as-macro @defshape/3d- cone-z [:float radius :float height] "docstring" (var other-axes (unquote (quote (<1> dot (<1> identifier (<2> lexical <3> "p" (<4> vec (<5> float) 3))) xy)))) (var this-axis (unquote (quote (<1> dot (<1> identifier (<2> lexical <3> "p" (<4> vec (<5> float) 3))) z)))) (var q [radius (- height)]) (return (sqrt d * sign s)))
    (as-macro @partial cone [axis radius height] "docstring" ((case axis (quote (1 0 0)) cone-x (quote (0 1 0)) cone-y (quote (0 0 1)) cone-z (errorf "unknown axis %q" (jlsl/show axis))) radius height))))
