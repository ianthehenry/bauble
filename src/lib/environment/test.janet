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
    align @{:doc "(align target from to)\n\nAlign a shape or a vector to another vector. Both the `from` and `to` vectors must have unit length.\n\nThis function is useful for \"pointing\" one shape towards another. For example:\n\n```\n(def pos [(sin (t * 2) * 50) (sin (t * 3) * 100) (cos (t * 5) * 50)])\n(union\n  (cone y 10 80 | align y (normalize pos))\n  (box 10 | move pos))\n```\n\nThe tip of the cone points towards the moving target. In this case the `from` vector is equal to the\naxis of the cone.\n\nIf `from` = `(- to)`, the result is undefined: there are infinitely many rotation matrices that reverse\na vector's direction."}
    alignment-matrix @{:doc "(alignment-matrix from to)\n\nReturn a 3D rotation matrix that aligns one normalized vector to another.\n\nBoth input vectors must have a unit length!\n\nIf `from` = `(- to)`, the result is undefined."}
    and @{}
    arc @{:doc "(arc radius angle thickness)\n\nTODOC"}
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
    cone @{:doc "(cone axis radius height)\n\nTODOC"}
    cos @{}
    cosh @{}
    cross @{}
    cross-matrix @{:doc "(cross-matrix vec)\n\nReturns the matrix such that `(* (cross-matrix vec1) vec2)` = `(cross vec1 vec2)`."}
    cut-disk @{:doc "(cut-disk radius bottom)\n\nTODOC"}
    d @{:value [:var "d" :float]}
    degrees @{}
    distance @{}
    dot @{}
    double @{}
    ellipsoid @{:doc "(ellipsoid size)\n\nReturns a 3D shape **with an incorrect distance field**.\n\nThe distance is a bound.\n\nThis means that some operations, like a smooth union, will not behave\ncorrectly on ellipsoids. Soft shadows will also appear too soft."}
    elongate @{:doc "(elongate shape size)\n\nStretch a shape."}
    equal @{}
    equilateral-triangle @{:doc "(equilateral-triangle r)\n\nTODOC"}
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
    hexagon @{:doc "(hexagon r)\n\nTODOC"}
    hexagram @{:doc "(hexagram r)\n\nTODOC"}
    in @{:doc "(in & args)\n\n"}
    inf @{:doc "The number representing positive infinity"
          :value 9e999}
    int @{}
    inversesqrt @{}
    isosceles-triangle @{:doc "(isosceles-triangle size)\n\nTODOC"}
    length @{}
    line @{:doc "(line start end width)\n\nTODOC"}
    log @{}
    log2 @{}
    map-distance @{:doc "(map-distance shape f)\n\n"}
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
    octagon @{:doc "(octagon r)\n\nTODOC"}
    octahedron @{:doc "(octahedron radius)\n\nTODOC"}
    offset @{:doc "(offset shape amount)\n\nOffsets the provided shape, rounding corners in the process.\n\nThis is the same as subtracting `amount` from the distance. It's more accurate\nto say that this \"moves between isosurfaces,\" so it may not actually\nround anything if the provided shape is not an exact distance field."}
    or @{}
    oriented-rect @{:doc "(oriented-rect start end width)\n\nTODOC"}
    outer-product @{}
    p @{:value [:var "p" :vec3]}
    parallelogram @{:doc "(parallelogram size skew)\n\nReturns a 2D shape. `size.x` is the width of the top and bottom edges, and `size.y` is the height of the parellogram.\n\n`skew` is how far the pallorelogram leans in the `x` direction, so the total width of the prellogram is `(size.x + skew) * 2`.\nA `skew` of `0` gives the same shape as `rect`.\""}
    pentagon @{:doc "(pentagon r)\n\nTODOC"}
    perlin @{:doc "(perlin p)\n\nReturns perlin noise from -1 to 1. The input is a vector of any dimension."}
    perlin+ @{:doc "(perlin+ v)\n\nPerlin noise in the range 0 to 1."}
    pi @{:value 3.1415926535897931}
    pi/10 @{:value 0.31415926535897931}
    pi/11 @{:value 0.28559933214452665}
    pi/12 @{:value 0.26179938779914941}
    pi/12*10 @{:value 2.617993877991494}
    pi/12*11 @{:value 2.8797932657906435}
    pi/12*2 @{:value 0.52359877559829882}
    pi/12*3 @{:value 0.78539816339744828}
    pi/12*4 @{:value 1.0471975511965976}
    pi/12*5 @{:value 1.308996938995747}
    pi/12*6 @{:value 1.5707963267948966}
    pi/12*7 @{:value 1.8325957145940459}
    pi/12*8 @{:value 2.0943951023931953}
    pi/12*9 @{:value 2.3561944901923448}
    pi/2 @{:value 1.5707963267948966}
    pi/3 @{:value 1.0471975511965976}
    pi/3*2 @{:value 2.0943951023931953}
    pi/4 @{:value 0.78539816339744828}
    pi/4*2 @{:value 1.5707963267948966}
    pi/4*3 @{:value 2.3561944901923448}
    pi/5 @{:value 0.62831853071795862}
    pi/6 @{:value 0.52359877559829882}
    pi/6*2 @{:value 1.0471975511965976}
    pi/6*3 @{:value 1.5707963267948966}
    pi/6*4 @{:value 2.0943951023931953}
    pi/6*5 @{:value 2.617993877991494}
    pi/7 @{:value 0.44879895051282759}
    pi/8 @{:value 0.39269908169872414}
    pi/8*2 @{:value 0.78539816339744828}
    pi/8*3 @{:value 1.1780972450961724}
    pi/8*4 @{:value 1.5707963267948966}
    pi/8*5 @{:value 1.9634954084936207}
    pi/8*6 @{:value 2.3561944901923448}
    pi/8*7 @{:value 2.748893571891069}
    pi/9 @{:value 0.3490658503988659}
    pie @{:doc "(pie radius angle)\n\nTODOC"}
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
    ring @{:doc "(ring radius angle thickness)\n\nTODOC"}
    rotate @{:doc "(rotate target & args)\n\nRotate a shape or a vector. Positive angles are counter-clockwise rotations.\n\nIn 3D, the arguments should be pairs of `axis angle`. For example:\n\n```\n(rotate (box 50) x 0.1 y 0.2)\n```\n\nAll `axis` arguments must be unit vectors. There are built-in axis variables `x`/`+y`/`-z`\nfor the cardinal directions, and these produce optimized rotation matrices. But you can\nrotate around an arbitrary axis:\n\n```\n(rotate (box 50) (normalize [1 1 1]) t)\n```\n\nThe order of the arguments is significant, as rotations are not commutative.\n\nIn 2D, the arguments should just be angles; no axis is allowed."}
    rotation-around @{:doc "(rotation-around axis angle)\n\nA rotation matrix about an arbitrary axis. More expensive to compute than the axis-aligned rotation matrices."}
    rotation-matrix @{:doc "(rotation-matrix & args)\n\nReturn a rotation matrix. Takes the same arguments as `rotate`, minus the initial thing to rotate."}
    rotation-x @{:doc "(rotation-x angle)\n\nA rotation matrix about the X axis."}
    rotation-y @{:doc "(rotation-y angle)\n\nA rotation matrix about the Y axis."}
    rotation-z @{:doc "(rotation-z angle)\n\nA rotation matrix about the Z axis."}
    round @{}
    round-even @{}
    round-rect @{:doc "(round-rect size radii)\n\nLike `rect`, but rounded. `radii` can be a single radius or a `vec4` of `[top-left top-right bottom-right bottom-left]`.`"}
    shell @{:doc "(shell shape &opt thickness)\n\nReturns a hollow version of the provided shape (the absolute value of the distance field)."}
    sign @{}
    sin @{}
    sinh @{}
    slice @{:doc "(slice shape axis &opt position)\n\nTake a 2D slice of a 3D shape at a given `position` along the supplied `axis`.\n\n`position` defaults to `0`."}
    smooth-union @{:doc "(smooth-union r & shapes)\n\n"}
    smoothstep @{}
    sphere @{:doc "(sphere radius)\n\nReturns a 3D shape."}
    sqrt @{}
    star @{:doc "(star outer-radius inner-radius)\n\nTODOC"}
    step @{}
    subject @{:doc "A variable that determines what Bauble will render.\n\nYou can set this variable explicitly to change your focus, or use the `view` macro to change your focus. If you don't set a subject, Bauble will render the last shape in your script."
              :ref @[nil]}
    sum @{:doc "(sum v)\n\nAdd the components of a vector."}
    t @{:value [:var "t" :float]}
    tan @{}
    tanh @{}
    tau @{:value 6.2831853071795862}
    tau/10 @{:value 0.62831853071795862}
    tau/11 @{:value 0.5711986642890533}
    tau/12 @{:value 0.52359877559829882}
    tau/12*10 @{:value 5.2359877559829879}
    tau/12*11 @{:value 5.7595865315812871}
    tau/12*2 @{:value 1.0471975511965976}
    tau/12*3 @{:value 1.5707963267948966}
    tau/12*4 @{:value 2.0943951023931953}
    tau/12*5 @{:value 2.617993877991494}
    tau/12*6 @{:value 3.1415926535897931}
    tau/12*7 @{:value 3.6651914291880918}
    tau/12*8 @{:value 4.1887902047863905}
    tau/12*9 @{:value 4.71238898038469}
    tau/2 @{:value 3.1415926535897931}
    tau/3 @{:value 2.0943951023931953}
    tau/3*2 @{:value 4.1887902047863905}
    tau/4 @{:value 1.5707963267948966}
    tau/4*2 @{:value 3.1415926535897931}
    tau/4*3 @{:value 4.71238898038469}
    tau/5 @{:value 1.2566370614359172}
    tau/6 @{:value 1.0471975511965976}
    tau/6*2 @{:value 2.0943951023931953}
    tau/6*3 @{:value 3.1415926535897931}
    tau/6*4 @{:value 4.1887902047863905}
    tau/6*5 @{:value 5.2359877559829879}
    tau/7 @{:value 0.89759790102565518}
    tau/8 @{:value 0.78539816339744828}
    tau/8*2 @{:value 1.5707963267948966}
    tau/8*3 @{:value 2.3561944901923448}
    tau/8*4 @{:value 3.1415926535897931}
    tau/8*5 @{:value 3.9269908169872414}
    tau/8*6 @{:value 4.71238898038469}
    tau/8*7 @{:value 5.497787143782138}
    tau/9 @{:value 0.69813170079773179}
    torus @{:doc "(torus axis radius thickness)\n\nReturns a 3D shape, a torus around the provided `axis`."}
    trapezoid @{:doc "(trapezoid bottom-width top-width height)\n\nTODOC"}
    triangle @{:doc "(triangle a b c)\n\nTODOC"}
    trunc @{}
    uint @{}
    uneven-capsule @{:doc "(uneven-capsule r1 r2 h)\n\nTODOC"}
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
(test-error (rotate (box 10) 10 20) "rotation-around: no overload for arguments [:float :float]")
(test-error (rotate (box 10) 10) "angle required")
(test-error (rotate (box 10) x 10 [1 2] 3) "rotation-around: no overload for arguments [:vec2 :float]")
