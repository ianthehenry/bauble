(use judge)
(import ../../jlsl)

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

(test (cleanse-environment (require "."))
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
    box @{:doc "(box size)\n\nReturns a 3D shape, a box with corners at `(- size)` and `size`. `size` will be coerced to a `vec3`.\n\nThink of `size` like the \"radius\" of the box: a box with `size.x = 50` will be `100` units wide."}
    box-frame @{:doc "(box-frame size thickness)\n\nReturns a 3D shape, the outline of a box."}
    ceil @{}
    circle @{:doc "(circle radius)\n\nReturns a 2D shape."}
    clamp @{}
    color @{:doc "(color shape color)\n\nSet the color field of a shape."}
    cos @{}
    cosh @{}
    cross @{}
    cross-matrix @{:doc "(cross-matrix vec)\n\nReturns the matrix such that `(* (cross-matrix vec1) vec2)` = `(cross vec1 vec2)`."}
    d @{:value [:var "d" :float]}
    degrees @{}
    distance @{}
    dot @{}
    double @{}
    equal @{}
    exp @{}
    exp2 @{}
    extrude @{:doc "(extrude shape axis &opt distance)\n\nExtrude a 2D shape into 3D along the given `axis`.\n\n`distance` defaults to `0` and determines the width, length, or height of the final shape.\nYou can also pass `inf` to get an infinite extrusion (which is slightly cheaper to compute)."}
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
    inf @{:doc "The number representing positive infinity"
          :value 9e999}
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
    move @{:doc "(move shape & args)\n\nTranslate a shape. Usually you'd use this with a vector offset:\n\n```\n(move (box 50) [0 100 0])\n```\n\nBut you can also provide a vector and a scalar:\n\n```\n(move (box 50) y 100)\n```\n\nWhich is the same as `(move (box 50) (y * 100))`.\n\nIf you provide multiple vector-scalar pairs, their sum is the final offset:\n\n```\n(move (box 50) x 100 y 100 -z 20)\n```\n\nThat is the same as `(move (box 50) (+ (x * 100) (y * 100) (-z * 100)))`."}
    normal @{:value [:var "normal" :vec3]}
    normalize @{}
    not @{}
    not-equal @{}
    not= @{}
    or @{}
    outer-product @{}
    p @{:value [:var "p" :vec3]}
    parallelogram @{:doc "(parallelogram size skew)\n\nReturns a 2D shape. `size.x` is the width of the top and bottom edges, and `size.y` is the height of the parellogram.\n\n`skew` is how far the pallorelogram leans in the `x` direction, so the total width of the prellogram is `(size.x + skew) * 2`.\nA `skew` of `0` gives the same shape as `rect`.\""}
    perlin @{:doc "(perlin p)\n\nReturns perlin noise from -1 to 1. The input is a vector of any dimension."}
    perlin+ @{:doc "(perlin+ v)\n\nPerlin noise in the range 0 to 1."}
    pow @{}
    product @{:doc "(product v)\n\nMultiply the components of a vector."}
    q @{:value [:var "q" :vec2]}
    quad-circle @{:doc "(quad-circle radius)\n\nReturns a 2D shape, an approximation of a circle out of quadratic bezier curves.\n\nIt's like a circle, but quaddier."}
    radians @{}
    rect @{:doc "(rect size)\n\nReturns a 2D shape, a rectangle with corners at `(- size)` and `size`. `size` will be coerced to a `vec2`.\n\nThink of `size` like the \"radius\" of the rect: a rect with `size.x = 50` will be `100` units wide."}
    reflect @{}
    refract @{}
    remap+ @{:doc "(remap+ x)\n\nRemap a number in the range `[-1 1]` into the range `[0 1]`."}
    revolve @{:doc "(revolve shape axis &opt offset)\n\nRevolve a 2D shape around the given `axis` to return a 3D shape.\n\nYou can optionally supply an `offset` to move the shape away from the origin first (the default is `0`)."}
    rhombus @{:doc "(rhombus size)\n\nReturns a 2D shape. It rhombs with a kite."}
    rotate @{:doc "(rotate target & args)\n\nRotate a shape or a vector. Positive angles are counter-clockwise rotations.\n\nIn 3D, the arguments should be pairs of `axis angle`. For example:\n\n```\n(rotate (box 50) x 0.1 y 0.2)\n```\n\nAll `axis` arguments must be unit vectors. There are built-in axis variables `x`/`+y`/`-z`\nfor the cardinal directions, and these produce optimized rotation matrices. But you can\nrotate around an arbitrary axis:\n\n```\n(rotate (box 50) (normalize [1 1 1]) t)\n```\n\nThe order of the arguments is significant, as rotations are not commutative.\n\nIn 2D, the arguments should just be angles; no axis is allowed."}
    rotate-around @{:doc "(rotate-around axis angle)\n\nA rotation matrix about an arbitrary axis. More expensive to compute than the axis-aligned rotation matrices."}
    rotate-x @{:doc "(rotate-x angle)\n\nA rotation matrix about the X axis."}
    rotate-y @{:doc "(rotate-y angle)\n\nA rotation matrix about the Y axis."}
    rotate-z @{:doc "(rotate-z angle)\n\nA rotation matrix about the Z axis."}
    rotation-matrix @{:doc "(rotation-matrix args)\n\n"}
    round @{}
    round-even @{}
    round-rect @{:doc "(round-rect size radii)\n\nLike `rect`, but rounded. `radii` can be a single radius or a `vec4` of `[top-left top-right bottom-right bottom-left]`.`"}
    sign @{}
    sin @{}
    sinh @{}
    slice @{:doc "(slice shape axis &opt position)\n\nTake a 2D slice of a 3D shape at a given `position` along the supplied `axis`.\n\n`position` defaults to `0`."}
    smooth-union @{:doc "(smooth-union r & shapes)\n\n"}
    smoothstep @{}
    sphere @{:doc "(sphere radius)\n\nReturns a 3D shape."}
    sqrt @{}
    step @{}
    subject @{:doc "A variable that determines what Bauble will render.\n\nYou can set this variable explicitly to change your focus, or use the `view` macro to change your focus. If you don't set a subject, Bauble will render the last shape in your script."
              :ref @[nil]}
    sum @{:doc "(sum v)\n\nAdd the components of a vector."}
    t @{:value [:var "t" :float]}
    tan @{}
    tanh @{}
    torus @{:doc "(torus axis radius thickness)\n\nReturns a 3D shape, a torus around the provided `axis`."}
    trunc @{}
    uint @{}
    union @{:doc "(union & shapes)\n\nJoin 'em up. Do it to it."}
    vec @{}
    vec2 @{}
    vec3 @{}
    vec4 @{}
    view @{:doc "(view subject)\n\nA shorthand for `(set subject _)` that fits nicely into pipe notation, e.g. `(sphere 50 | view)`."
           :macro true}
    worley @{:doc "(worley p)\n\n2D Worley noise, also called cellular noise or voronoi noise. Returns the nearest distance to points distributed randomly within the tiles of a square grid."}
    worley2 @{:doc "(worley2 p)\n\nLike `worley`, but returns the nearest distance in `x` and the second-nearest distance in `y`."}
    x @{:doc "`[1 0 0]`" :value [1 0 0]}
    xor @{}
    y @{:doc "`[0 1 0]`" :value [0 1 0]}
    z @{:doc "`[0 0 1]`" :value [0 0 1]}})

(use .)
# just make sure valid use cases don't error
(test (do (rotate (rect 10) 10) nil) nil)
(test (do (rotate (box 10) x 20) nil) nil)
(test (do (rotate (box 10) [1 2 3] 20) nil) nil)
(test (do (rotate (box 10) x 1 y 2) nil) nil)
(test-error (rotate (rect 10) x 10) "expected angle, got (1 0 0)")
(test-error (rotate (box 10) 10 20) "rotate-around: no overload for arguments [:float :float]")
(test-error (rotate (box 10) 10) "angle required")
(test-error (rotate (box 10) x 10 [1 2] 3) "rotate-around: no overload for arguments [:vec2 :float]")
