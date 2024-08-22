(use judge)
(import ../jlsl)

(defn cleanse-environment-entry [t]
  (if (table? t) (do
    (def t (table/proto-flatten t))
    (put t :source-map nil)
    (put t :private nil)
    (when (function? (t :value))
      (put t :value nil))
    (when (jlsl/variable? (t :value))
      (put t :value [:var (jlsl/variable/name (t :value)) (jlsl/show-type (jlsl/variable/type (t :value)))]))
    (when (function? (get-in t [:ref 0]))
      (put t :ref nil))
    t)
  t))

(defn cleanse-environment [t]
  (tabseq [[k v] :pairs t :when (symbol? k) :when (not (v :private))]
    k (cleanse-environment-entry v)))

(test (cleanse-environment (require "./environment"))
  @{% @{}
    * @{}
    + @{}
    +x @{:doc "`[1 0 0]`" :value [1 0 0]}
    +y @{:doc "`[0 1 0]`" :value [0 1 0]}
    +z @{:doc "`[0 0 1]`" :value [0 0 1]}
    - @{}
    -x @{:doc "`[-1 0 0]`" :value [-1 0 0]}
    -y @{:doc "`[0 -1 0]`" :value [0 -1 0]}
    -z @{:doc "`[0 0 -1]`" :value [0 0 -1]}
    . @{:doc "(. expr field)\n\nBehaves like `.` in GLSL, for accessing components of a vector or struct, e.g. `(. foo xy)`.\n\nBauble's dot syntax, `foo.xy`, expands to call this macro."
        :macro true}
    / @{}
    < @{}
    <= @{}
    = @{}
    > @{}
    >= @{}
    @and @{:doc "(and & forms)\n\nEvaluates to the last argument if all preceding elements are truthy, otherwise\nevaluates to the first falsey argument."
           :macro true}
    @in @{:doc "(in ds key &opt dflt)\n\nGet value in ds at key, works on associative data structures. Arrays, tuples, tables, structs, strings, symbols, and buffers are all associative and can be used. Arrays, tuples, strings, buffers, and symbols must use integer keys that are in bounds or an error is raised. Structs and tables can take any value as a key except nil and will return nil or dflt if not found."}
    @length @{:doc "(length ds)\n\nReturns the length or count of a data structure in constant time as an integer. For structs and tables, returns the number of key-value pairs in the data structure."}
    @or @{:doc "(or & forms)\n\nEvaluates to the last argument if all preceding elements are falsey, otherwise\nevaluates to the first truthy element."
          :macro true}
    P @{:value [:var "P" :vec3]}
    Q @{:value [:var "Q" :vec2]}
    abs @{}
    acos @{}
    acosh @{}
    and @{}
    asin @{}
    asinh @{}
    atan @{}
    atanh @{}
    bool @{}
    box @{:doc "(box size)\n\nit a box"}
    ceil @{}
    circle @{:doc "(circle r)\n\nit a circle"}
    clamp @{}
    color @{:doc "(color field-set color-expression)\n\ncolor"}
    cos @{}
    cosh @{}
    cross @{}
    cross-matrix @{:doc "Returns the matrix such that `(* (cross-matrix vec1) vec2)` = `(cross vec1 vec2)`."}
    d @{:value [:var "d" :float]}
    degrees @{}
    distance @{}
    dot @{}
    double @{}
    equal @{}
    exp @{}
    exp2 @{}
    faceforward @{}
    float @{}
    floor @{}
    fma @{}
    fract @{}
    gl-frag-coord @{:value [:var "gl_FragCoord" :vec4]}
    gl-frag-depth @{:value [:var "gl_FragDepth" :float]}
    gl-front-facing @{:value [:var "gl_FrontFacing" :bool]}
    gl-point-coord @{:value [:var "gl_PointCoord" :vec2]}
    gradient @{:value [:var "gradient" :vec2]}
    in @{:doc "(in & args)\n\n"}
    int @{}
    inversesqrt @{}
    length @{}
    log @{}
    log2 @{}
    mat2 @{}
    mat2x2 @{}
    mat2x3 @{}
    mat2x4 @{}
    mat3 @{}
    mat3x2 @{}
    mat3x3 @{}
    mat3x5 @{}
    mat4 @{}
    mat4x2 @{}
    mat4x3 @{}
    mat4x4 @{}
    max @{}
    min @{}
    mix @{}
    mod @{}
    move @{:doc "(move shape offset)\n\ntranslate"}
    normal @{:value [:var "normal" :vec3]}
    normalize @{}
    not @{}
    not-equal @{}
    not= @{}
    or @{}
    outer-product @{}
    p @{:value [:var "p" :vec3]}
    parallelogram @{:doc "(parallelogram width height skew)\n\nit a parallelogram"}
    pow @{}
    q @{:value [:var "q" :vec2]}
    quad-circle @{:doc "(quad-circle radius)\n\nit's like a circle but quaddier."}
    radians @{}
    rect @{:doc "(rect size)\n\nit a box"}
    reflect @{}
    refract @{}
    remap-plus @{:doc "Remap a number in the range `[-1 1]` into the range `[0 1]`."}
    rhombus @{:doc "(rhombus size)\n\nit rhomb"}
    rotate @{:doc "(rotate & args)\n\nlook, there are a lot of overloads"}
    rotate-x @{:doc "A rotation matrix about the X axis."}
    rotate-y @{:doc "A rotation matrix about the Y axis."}
    rotate-z @{:doc "A rotation matrix about the Z axis."}
    round @{}
    round-even @{}
    sign @{}
    sin @{}
    sinh @{}
    smooth-union @{:doc "(smooth-union r & shapes)\n\n"}
    smoothstep @{}
    sphere @{:doc "(sphere r)\n\nit a sphere"}
    sqrt @{}
    step @{}
    subject @{:doc "A variable that determines what Bauble will render.\n\nYou can set this variable explicitly to change your focus, or use the `view` macro to change your focus. If you don't set a subject, Bauble will render the last expression in your script that it knows how to render."
              :ref @[nil]}
    t @{:value [:var "t" :float]}
    tan @{}
    tanh @{}
    trunc @{}
    uint @{}
    union @{:doc "(union & shapes)\n\nJoin 'em up. Do it to it."}
    vec @{}
    vec2 @{}
    vec3 @{}
    vec4 @{}
    view @{:doc "(view subject)\n\nA shorthand for `(set subject _)` that fits nicely into pipe notation, e.g. `(sphere 50 | view)`."
           :macro true}
    x @{:doc "`[1 0 0]`" :value [1 0 0]}
    xor @{}
    y @{:doc "`[0 1 0]`" :value [0 1 0]}
    z @{:doc "`[0 0 1]`" :value [0 0 1]}})

(use ./environment)
# just make sure valid use cases don't error
(test (do (rotate (rect 10) 10) nil) nil)
(test (do (rotate (box 10) x 20) nil) nil)
(test (do (rotate (box 10) [1 2 3] 20) nil) nil)
(test (do (rotate (box 10) x 1 y 2) nil) nil)
(test-error (rotate (rect 10) x 10) "rotate-2d: no overload for arguments [:vec3]")
(test-error (rotate (box 10) 10 20) "rotate-around: no overload for arguments [:float :float]")
(test-error (rotate (box 10) 10) "angle required")
(test-error (rotate (box 10) x 10 [1 2] 3) "rotate-around: no overload for arguments [:vec2 :float]")
