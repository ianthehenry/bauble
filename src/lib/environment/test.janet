(use judge)
(import ../../jlsl)
(import ./derive)
(use ./util)

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
    # this is a hack so that we don't render the default lights...
    (when (array? (get-in t [:ref 0]))
      (put t :ref '<array>))
    (when (t :value)
      (put t :value (jlsl/show (t :value))))
    (when (t :ref)
      (put t :ref @[(jlsl/show (get-in t [:ref 0]))]))
    t)
  t))

(defn cleanse-environment [t]
  (tabseq [[k v] :pairs t :when (symbol? k) :when (table? v)]
    k (cleanse-environment-entry v)))

(test (cleanse-environment (proto-flatten-to-root (derive/new)))
  @{% @{}
    * @{:doc "(* & xs)\n\nOverloaded to work with tuples, arrays, and expressions."}
    *lights* @{:doc "The default lights used by surfacing functions like `blinn-phong`.\nYou can manipulate this using `setdyn` or `with-dyns` like any other\ndynamic variable, but there is a dedicated `with-lights` function to\nset it in a way that fits nicely into a pipeline."
               :dyn true
               :value :lights}
    + @{:doc "(+ & xs)\n\nOverloaded to work with tuples, arrays, and expressions."}
    +x @{:doc "`[1 0 0]`" :value [1 0 0]}
    +y @{:doc "`[0 1 0]`" :value [0 1 0]}
    +z @{:doc "`[0 0 1]`" :value [0 0 1]}
    - @{:doc "(- & xs)\n\nOverloaded to work with tuples, arrays, and expressions."}
    -pi/10 @{:value -0.31415926535897931}
    -pi/11 @{:value -0.28559933214452665}
    -pi/12 @{:value -0.26179938779914941}
    -pi/12*10 @{:value -2.617993877991494}
    -pi/12*11 @{:value -2.8797932657906435}
    -pi/12*2 @{:value -0.52359877559829882}
    -pi/12*3 @{:value -0.78539816339744828}
    -pi/12*4 @{:value -1.0471975511965976}
    -pi/12*5 @{:value -1.308996938995747}
    -pi/12*6 @{:value -1.5707963267948966}
    -pi/12*7 @{:value -1.8325957145940459}
    -pi/12*8 @{:value -2.0943951023931953}
    -pi/12*9 @{:value -2.3561944901923448}
    -pi/2 @{:value -1.5707963267948966}
    -pi/3 @{:value -1.0471975511965976}
    -pi/3*2 @{:value -2.0943951023931953}
    -pi/4 @{:value -0.78539816339744828}
    -pi/4*2 @{:value -1.5707963267948966}
    -pi/4*3 @{:value -2.3561944901923448}
    -pi/5 @{:value -0.62831853071795862}
    -pi/6 @{:value -0.52359877559829882}
    -pi/6*2 @{:value -1.0471975511965976}
    -pi/6*3 @{:value -1.5707963267948966}
    -pi/6*4 @{:value -2.0943951023931953}
    -pi/6*5 @{:value -2.617993877991494}
    -pi/7 @{:value -0.44879895051282759}
    -pi/8 @{:value -0.39269908169872414}
    -pi/8*2 @{:value -0.78539816339744828}
    -pi/8*3 @{:value -1.1780972450961724}
    -pi/8*4 @{:value -1.5707963267948966}
    -pi/8*5 @{:value -1.9634954084936207}
    -pi/8*6 @{:value -2.3561944901923448}
    -pi/8*7 @{:value -2.748893571891069}
    -pi/9 @{:value -0.3490658503988659}
    -tau/10 @{:value -0.62831853071795862}
    -tau/11 @{:value -0.5711986642890533}
    -tau/12 @{:value -0.52359877559829882}
    -tau/12*10 @{:value -5.2359877559829879}
    -tau/12*11 @{:value -5.7595865315812871}
    -tau/12*2 @{:value -1.0471975511965976}
    -tau/12*3 @{:value -1.5707963267948966}
    -tau/12*4 @{:value -2.0943951023931953}
    -tau/12*5 @{:value -2.617993877991494}
    -tau/12*6 @{:value -3.1415926535897931}
    -tau/12*7 @{:value -3.6651914291880918}
    -tau/12*8 @{:value -4.1887902047863905}
    -tau/12*9 @{:value -4.71238898038469}
    -tau/2 @{:value -3.1415926535897931}
    -tau/3 @{:value -2.0943951023931953}
    -tau/3*2 @{:value -4.1887902047863905}
    -tau/4 @{:value -1.5707963267948966}
    -tau/4*2 @{:value -3.1415926535897931}
    -tau/4*3 @{:value -4.71238898038469}
    -tau/5 @{:value -1.2566370614359172}
    -tau/6 @{:value -1.0471975511965976}
    -tau/6*2 @{:value -2.0943951023931953}
    -tau/6*3 @{:value -3.1415926535897931}
    -tau/6*4 @{:value -4.1887902047863905}
    -tau/6*5 @{:value -5.2359877559829879}
    -tau/7 @{:value -0.89759790102565518}
    -tau/8 @{:value -0.78539816339744828}
    -tau/8*2 @{:value -1.5707963267948966}
    -tau/8*3 @{:value -2.3561944901923448}
    -tau/8*4 @{:value -3.1415926535897931}
    -tau/8*5 @{:value -3.9269908169872414}
    -tau/8*6 @{:value -4.71238898038469}
    -tau/8*7 @{:value -5.497787143782138}
    -tau/9 @{:value -0.69813170079773179}
    -x @{:doc "`[-1 0 0]`" :value [-1 0 0]}
    -y @{:doc "`[0 -1 0]`" :value [0 -1 0]}
    -z @{:doc "`[0 0 -1]`" :value [0 0 -1]}
    . @{:doc "(. expr field)\n\nBehaves like `.` in GLSL, for accessing components of a vector or struct, e.g. `(. foo xy)`.\n\nBauble's dot syntax, `foo.xy`, expands to call this macro. The second argument to `.` will be\nquasiquoted, so you can dynamically select a dynamic field with `(. foo ,axis)`."
        :macro true}
    / @{:doc "(/ & xs)\n\nOverloaded to work with tuples, arrays, and expressions."}
    < @{}
    <= @{}
    = @{}
    > @{}
    >= @{}
    @* @{}
    @+ @{}
    @- @{}
    @/ @{}
    @and @{:doc "(and & forms)\n\nEvaluates to the last argument if all preceding elements are truthy, otherwise\nevaluates to the first falsey argument."
           :macro true}
    @in @{:doc "(in ds key &opt dflt)\n\nGet value in ds at key, works on associative data structures. Arrays, tuples, tables, structs, strings, symbols, and buffers are all associative and can be used. Arrays, tuples, strings, buffers, and symbols must use integer keys that are in bounds or an error is raised. Structs and tables can take any value as a key except nil and will return nil or dflt if not found."}
    @length @{:doc "(length ds)\n\nReturns the length or count of a data structure in constant time as an integer. For structs and tables, returns the number of key-value pairs in the data structure."}
    @or @{:doc "(or & forms)\n\nEvaluates to the last argument if all preceding elements are falsey, otherwise\nevaluates to the first truthy element."
          :macro true}
    Camera @{:doc "(Camera position direction up fov)\n\n"}
    Frag-Coord @{:doc "The center of the current pixel being rendered. Pixel centers are at `[0.5 0.5]`, so with no anti-aliasing this will have values like `[0.5 0.5]`, `[1.5 0.5]`, etc. If you are using multisampled antialiasing, this will have off-centered values like [0.3333 0.3333]."
                 :value [:var "Frag-Coord" :vec2]}
    Light @{:doc "(Light color direction brightness)\n\n"}
    P @{:doc "The global point in 3D space. This is the position of the current ray before any transformations are applied to it."
        :value [:var "P" :vec3]}
    Q @{:doc "The global point in 2D space."
        :value [:var "Q" :vec2]}
    Ray @{:doc "(Ray origin direction)\n\n"}
    aa-grid-size @{:doc "The size of the grid used to sample a single pixel. The total samples per pixel will\nbe the square of this number. The default value is 1 (no anti-aliasing)."
                   :ref @[nil]}
    abs @{}
    acos @{}
    acosh @{}
    align @{:doc "(align target from to)\n\nAlign a shape or a vector to another vector. Both the `from` and `to` vectors must have unit length.\n\nThis function is useful for \"pointing\" one shape towards another. For example:\n\n```\n(def pos [(sin (t * 2) * 50) (sin (t * 3) * 100) (cos (t * 5) * 50)])\n(union\n  (cone y 10 80 | align y (normalize pos))\n  (box 10 | move pos))\n```\n\nThe tip of the cone points towards the moving target. In this case the `from` vector is equal to the\naxis of the cone.\n\nIf `from` = `(- to)`, the result is undefined: there are infinitely many rotation matrices that reverse\na vector's direction."}
    alignment-matrix @{:doc "(alignment-matrix from to)\n\nReturn a 3D rotation matrix that aligns one normalized vector to another.\n\nBoth input vectors must have a unit length!\n\nIf `from` = `(- to)`, the result is undefined."}
    and @{}
    arc @{:doc "(arc radius angle thickness)\n\n```example\n(arc 100 (osc t 5 tau) (osc t 2 5 20))\n```"}
    asin @{}
    asinh @{}
    atan @{}
    atan2 @{:doc "(atan2 y x)\n\nReturns a value in the range `[0, tau)` representing the angle\nbetween the (2D) `+x` axis and the point `[x y]`.\n\nThis is an alternative to the built-in `atan`'s two argument\noverload that is well-defined when `x = 0`.\n\nYou can also invoke this with a single `vec2` whose coordinates\nwill act as `x` and `y`."}
    atanh @{}
    background-color @{:doc "A variable that determines the background color of the canvas.\n\nDefault is `graydient`."
                       :ref @[[do]]}
    ball @{:doc "(ball size)\n\nReturns a 3D shape, which is either a sphere or an ellipsoid, depending on the type of `size`.\n\n```example\n(ball 100)\n```\n\n```example\n(ball [50 80 120])\n```\n\nEllipsoids do not have correct distance fields. Their distance field is only a bound, and\nit has strange isosurfaces that can make it combine with other shapes oddly:\n\n```example\n(ball [30 50 80] | slice y)\n```"}
    black @{:value [0.03 0.03 0.03]}
    blinn-phong @{:doc "(blinn-phong shape color [:s shininess] [:g glossiness])\n\nTODOC"}
    blue @{:value [hsv 0.66666666666666663 0.98 1]}
    bool @{}
    box @{:doc "(box size [:r round])\n\nReturns a 3D shape, a box with corners at `(- size)` and `size`. `size` will be coerced to a `vec3`.\n\nThink of `size` like the \"radius\" of the box: a box with `size.x = 50` will be `100` units wide.\n\n```example\n(box 100 :r (osc t 2 0 10))\n```\n\n```example\n(box [100 (osc t 2 50 100) (oss t 2 50 100)])\n```"}
    box-frame @{:doc "(box-frame size thickness [:r round])\n\nReturns a 3D shape, the outline of a box.\n\n```example\n(union\n  (box-frame 100 5 :r (osc t 3 5))\n  (box-frame [(osc t 4 30 100) (osc t 5 30 100) (oss t 6 30 100)] 1))\n```"}
    calculate-gradient @{:doc "(calculate-gradient expr)\n\nEvaluates the given 2D distance expression four times, and returns an approximation\nof the expression's gradient."}
    calculate-normal @{:doc "(calculate-normal expr)\n\nEvaluates the given 3D distance expression four times, and returns an approximation\nof the expression's gradient."}
    calculate-occlusion @{}
    camera @{:doc "An expression for a `ray` that determines the position and direction of the camera."
             :ref @[nil]}
    camera/dolly @{:doc "(camera/dolly camera amount)\n\nMove the camera forward or backward.\n\n```example\n(morph (ball 50) (box 50) 2\n| union (circle 200 | extrude y 10 | move y -100)\n| blinn-phong (vec3 0.75))\n(set camera (camera/perspective [0 100 600] [0 0 0] :fov 45\n| camera/dolly (sin t * 100)))\n```\n\n```example\n(morph (ball 50) (box 50) 2\n| union\n  (circle 200 | extrude y 10 | move y -100)\n  (box [100 200 50] | tile [300 0 300] :limit 4 | move [0 0 -1000])\n| blinn-phong (vec3 0.75))\n\n# hitchcock zoom\n(set camera (camera/perspective [0 100 600] [0 0 0] :fov 45\n| camera/dolly (sin+ t * -500)\n| camera/zoom (sin+ t + 1)\n))\n```"}
    camera/pan @{:doc "(camera/pan camera angle [:up up])\n\nRotate the camera left and right.\n\n```example\n(morph (ball 50) (box 50) 2\n| union (circle 200 | extrude y 10 | move y -100)\n| blinn-phong (vec3 0.75))\n(set camera (camera/perspective [0 100 600] [0 0 0] :fov 45\n| camera/pan (sin t * 0.2)))\n```\n\nBy default this rotation is relative to the camera's current\norientation, so the image you see will always appear to be moving\nhorizontally during a pan. But you can provide an absolute\n`:up` vector to ignore the camera's roll. (I think the difference\nis easier to understand if you unroll the camera afterward.)\n\n  ```example\n(morph (ball 50) (box 50) 2\n| union (circle 200 | extrude y 10 | move y -100)\n| blinn-phong (vec3 0.75))\n(set camera (camera/perspective [0 100 600] [0 0 0] :fov 45\n| camera/roll pi/4\n| camera/pan (sin t * 0.2)\n# | camera/roll -pi/4\n))\n```\n\n```example\n(morph (ball 50) (box 50) 2\n| union (circle 200 | extrude y 10 | move y -100)\n| blinn-phong (vec3 0.75))\n(set camera (camera/perspective [0 100 600] [0 0 0] :fov 45\n| camera/roll pi/4\n| camera/pan (sin t * 0.2) :up y\n# | camera/roll -pi/4\n))\n```"}
    camera/perspective @{:doc "(camera/perspective position target [:fov fov])\n\nReturns the camera located at `position` and aiming towards `target`\nthat has no roll.\n\nYou can change the field of view by passing `:fov` with a number of degrees. The default is `60`, and\nthe default orbiting free camera uses `45`.\n\n```example\n(morph (ball 50) (box 50) 2\n| union (circle 200 | extrude y 10 | move y -100)\n| blinn-phong (vec3 0.75))\n(def pos [(sin t * 200) (cos+ (t / 2) * 300) 500])\n(set camera (camera/perspective pos [0 0 0] :fov 45))\n```"}
    camera/ray @{:doc "(camera/ray camera)\n\nReturns the perspective-adjusted ray from this camera for\nthe current `frag-coord`. You probably don't need to call\nthis."}
    camera/roll @{:doc "(camera/roll camera angle)\n\nRoll the camera around.\n\n```example\n(morph (ball 50) (box 50) 2\n| union (circle 200 | extrude y 10 | move y -100)\n| blinn-phong (vec3 0.75))\n(set camera (camera/perspective [0 100 600] [0 0 0] :fov 45\n| camera/roll (sin t * 0.2)))\n```"}
    camera/tilt @{:doc "(camera/tilt camera angle [:up up])\n\nRotate the camera up and down.\n\n```example\n(morph (ball 50) (box 50) 2\n| union (circle 200 | extrude y 10 | move y -100)\n| blinn-phong (vec3 0.75))\n(set camera (camera/perspective [0 100 600] [0 0 0] :fov 45\n| camera/tilt (sin t * 0.2)))\n```\n\nAs with `pan`, you can supply an absolute `:up` vector to use\ninstead of the camera's current roll.\n"}
    camera/zoom @{:doc "(camera/zoom camera amount)\n\nZoom the camera by changing its field of view.\n\n```example\n(morph (ball 50) (box 50) 2\n| union (circle 200 | extrude y 10 | move y -100)\n| blinn-phong (vec3 0.75))\n(set camera (camera/perspective [0 100 600] [0 0 0] :fov 45\n| camera/zoom (sin t * 0.2 + 1)))\n```"}
    camera? @{:doc "(camera? value)\n\nReturns `true` if `value` is a GLSL expression with type `Camera`."}
    cast-light-hard-shadow @{:doc "(cast-light-hard-shadow light-color light-position)\n\nTODOC"}
    cast-light-no-shadow @{:doc "(cast-light-no-shadow light-color light-position)\n\nTODOC"}
    cast-light-soft-shadow @{:doc "(cast-light-soft-shadow light-color light-position softness)\n\nTODOC"}
    ceil @{}
    circle @{:doc "(circle radius)\n\nReturns a 2D shape.\n\n```example\n(circle 100)\n```"}
    clamp @{}
    color @{:doc "(color shape color)\n\nSet a shape's color field."}
    cone @{:doc "(cone axis radius height)\n\nReturns a 3D shape. The `height` is the extent in only a single direction.\n\n```example\n(cone y 50 (sin t * 150))\n```"}
    cos @{}
    cos+ @{:doc "(cos+ x)\n\nLike `cos`, but returns a number in the range `0` to `1`."}
    cos- @{:doc "(cos- x)\n\nLike `cos`, but returns a number in the range `0` to `-1`."}
    cosh @{}
    cross @{}
    cross-matrix @{:doc "(cross-matrix vec)\n\nReturns the matrix such that `(* (cross-matrix vec1) vec2)` = `(cross vec1 vec2)`."}
    cube @{:doc "(cube size [:r round])\n\nThis is an alias for the `float` overload of `box`."}
    cut-disk @{:doc "(cut-disk radius bottom)\n\nReturns a 2D shape.\n\n```example\n(cut-disk 100 (sin t * 80))\n```"}
    cyan @{:value [hsv 0.5 0.98 1]}
    cylinder @{:doc "(cylinder axis radius height [:r round])\n\nReturns a 3D shape, a cylinder oriented along the given `axis`.\n\n```example\n(cylinder y 50 100)\n```\n\nThe second argument is twice the length of the cylinder. Like many shapes,\nyou can round it with `:r`.\n\n```example\n(cylinder z 100 50 :r (osc t 2 0 10))\n```"}
    dark-gray @{:value [0.25 0.25 0.25]}
    default-2d-color @{:doc "A variable that determines the default color to use when rendering a 2D shape with no color field.\n\nDefault is `isolines`."
                       :ref @[[isolines]]}
    default-3d-color @{:doc "A variable that determines the default color to use when rendering a 3D shape with no color field.\n\nDefault is `normal+`."
                       :ref @[[+ 0.5 [* 0.5 normal]]]}
    degrees @{}
    depth @{:doc "The distance that the current ray has marched, equal to `(distance ray-origin P)`. Not defined in 2D."
            :value [:var "depth" :float]}
    dist @{:doc "(Color only!) The value of the global distance field at `P`. In 3D, this should be a very small positive number, assuming the ray was able to converge correctly. In 2D, this gives a more useful value."
           :value [:var "dist" :float]}
    distance @{}
    dot @{}
    double @{}
    ellipsoid @{:doc "(ellipsoid size)\n\nReturns a 3D shape. This is an alias for the `vec3` overload of `ball`."}
    elongate @{:doc "(elongate shape size)\n\nStretch the center of a shape, leaving the sides untouched.\n\n```example\n(cone y 50 100 | elongate [(osc t 3 50) 0 (osc t 6 100)])\n```\n\n```example\n(torus x 50 20 | elongate [0 100 0])\n```\n\n```example\n(rhombus [100 (gl/if (< q.y 0) 100 50)] | elongate [0 (osc t 2 0 20)])\n```"}
    equal @{}
    equilateral-triangle @{:doc "(equilateral-triangle radius [:r round])\n\nTODOC"}
    exp @{}
    exp2 @{}
    expand @{:doc "(expand shape amount)\n\nExpands the provided shape, rounding corners in the process.\n\nThis is the same as subtracting `amount` from the distance field.\nIt's more accurate to say that this \"moves between isosurfaces,\" so\nit may not actually round anything if the provided shape is not an\nexact distance field.\n\nFor example, this produces a nicely expanded shape:\n\n```example\n(rect 90 | expand (sin+ t * 30))\n```\n\nBut this does something weird, because subtraction does not produce\nan exact distance field:\n\n```example\n(rect 90\n| subtract (rect 100 | move x 150)\n| expand (sin+ t * 30))\n```"}
    extrude @{:doc "(extrude shape axis &opt distance)\n\nExtrude a 2D shape into 3D along the given `axis`.\n\n`distance` defaults to `0` and determines the width, length, or height of the final shape.\nYou can also pass `inf` to get an infinite extrusion (which is slightly cheaper to compute)."}
    faceforward @{}
    float @{}
    floor @{}
    fma @{}
    fract @{}
    frag-coord @{:doc "The logical position of the current fragment being rendered, in the approximate\nrange `-0.5` to `0.5`, with `[0 0]` as the center of the screen. Note though that\nwe always shade pixel centers, so we never actual render `-0.5` or `0.5`, just\nnearby subpixel approximations depending on the antialiasing level.\n\nThis is equal to `(Frag-Coord - (resolution * 0.5) / max resolution)`."
                 :value [:var "frag-coord" :vec2]}
    fresnel @{:doc "(fresnel subject [:color color] [:exponent exponent])\n\nTint a shape with an approximation of Fresnel reflectivity.\n\n`:color` defaults to `[1 1 1]`; `:exponent` defaults to `5`."}
    gl-frag-coord @{:value [:var "gl_FragCoord" :vec4]}
    gl-frag-depth @{:value [:var "gl_FragDepth" :float]}
    gl-front-facing @{:value [:var "gl_FrontFacing" :bool]}
    gl-point-coord @{:value [:var "gl_PointCoord" :vec2]}
    gl/def @{:doc "(gl/def name expression)\n\nYou can use `gl/def` to create new top-level GLSL variables which will only\nbe evaluated once (per distance and color field evaluation). This is useful in\norder to re-use an expensive value in multiple places, when that value only\ndepends on values that are available at the beginning of shading.\n\n```example\n(gl/def signal (perlin+ (p / 20)))\n(shape/3d (signal * 0.5)\n| intersect (ball 100)\n| color [signal (pow signal 2) 0])\n```\n\nThis is shorthand for `(def foo (hoist expression \"foo\"))`.\n\nNote that since the signal is evaluated at the top-level, `p` will always be the\nsame as `P`. Consider this example:\n\n```example\n(gl/def signal (perlin+ (p / 20)))\n(shape/3d (signal * 0.5)\n| intersect (ball 100)\n| color [signal (pow signal 2) 0]\n| move x (sin t * 100)\n)\n```\n\nChange the `gl/def` to a regular `def` to see some of the impliciations of hoisting\na computation."
             :macro true}
    gl/defn @{:doc "(gl/defn return-type name params & body)\n\nDefines a GLSL function. You must explicitly annotate the return type\nand the type of all arguments. The body of the function uses the GLSL\nDSL, i.e. it is not normal Janet code.\n\n```\n(gl/defn :vec3 hsv [:float hue :float saturation :float value]\n  (var c (hue * 6 + [0 4 2] | mod 6 - 3 | abs))\n  (return (value * (mix (vec3 1) (c - 1 | clamp 0 1) saturation))))\n```"
              :macro true}
    gl/do @{:doc "(gl/do & body)\n\nExecute a series of GLSL statements and return the final expression.\n\n```example\n(ball 100 | color \n  (gl/do \"optional-label\"\n    (var c [1 0 1])\n    (for (var i 0:u) (< i 10:u) (++ i)\n      (+= c.g 0.01))\n    c))\n```\n\nThe body of this macro is not regular Janet code, but a special DSL\nthat is not really documented anywhere, making it pretty hard to use."
            :macro true}
    gl/hoist @{:doc "(gl/hoist expression &opt name)\n\nReturn a hoisted version of the expression See the documentation for `gl/def`\nfor an explanation of this."
               :macro true}
    gl/if @{:doc "(gl/if condition then else)\n\nA GLSL ternary conditional expression.\n\n```example\n(ball 100 | color \n  (gl/if (< normal.y 0) \n    [1 0 0] \n    [1 1 0]))\n```"}
    gl/iife @{:doc "(gl/iife & body)\n\nLike `gl/do`, except that you can explicitly return early.\n\n```example\n(ball 100 | color\n  (gl/iife \"optional-label\"\n    (var c [1 0 1])\n    (if (< normal.y 0)\n      (return c))\n    (for (var i 0:u) (< i 10:u) (++ i)\n      (+= c.g (p.x / 100 / 10)))\n    c))\n```"
              :macro true}
    gl/let @{:doc "(gl/let bindings & body)\n\nLike `let`, but creates GLSL bindings instead of a Janet bindings. You can use this\nto reference an expression multiple times while only evaluating it once in the resulting\nshader.\n\nFor example:\n\n```\n(let [s (sin t)]\n  (+ s s))\n```\n\nProduces GLSL code like this:\n\n```\nsin(t) + sin(t)\n```\n\nBecause `s` refers to the GLSL *expression* `(sin t)`.\n\nMeanwhile:\n\n```\n(gl/let [s (sin t)]\n  (+ s s))\n```\n\nProduces GLSL code like this:\n\n```\nfloat let(float s) {\n  return s + s;\n}\n\nlet(sin(t))\n```\n\nOr something equivalent. Note that the variable is hoisted into an immediately-invoked function\nbecause it's the only way to introduce a new identifier in a GLSL expression context.\n\nYou can also use Bauble's underscore notation to fit this into a pipeline:\n\n```\n(s + s | gl/let [s (sin t)] _)\n```\n\nIf the body of the `gl/let` returns a shape, the bound variable will be available in all of its\nfields. If you want to refer to variables or expressions that are only available in some fields,\npass a keyword as the first argument:\n\n```example\n(gl/let :color [banding (dist * 10)]\n  (box 100 | blinn-phong [1 banding 0]))\n```"
             :macro true}
    gl/with @{:doc "(gl/with bindings & body)\n\nLike `gl/let`, but instead of creating a new binding, it alters the value of an existing\nvariable. You can use this to give new values to dynamic variables. For example:\n\n```example\n# implement your own `move`\n(gl/with [p (- p [0 (sin t * 50) 0])] (ball 100))\n```\n\nYou can also use Bauble's underscore notation to fit this into a pipeline:\n\n```example\n(ball 100 | gl/with [p (- p [0 (sin t * 50) 0])] _)\n```\n\nYou can -- if you really want -- use this to alter `P` or `Q` to not refer to the point in\nglobal space, or use it to pretend that the camera `ray` is actually coming at a different angle.\n\nThe variables you change in `gl/with` will, by default, apply to all of the fields of a shape.\nYou can pass a keyword as the first argument to only change a particular field. This allows you\nto refer to variables that only exist in color expressions:\n\n```example\n(gl/with :color [normal (normal + (perlin p * 0.1))]\n  (ball 100 | blinn-phong [1 0 0]))\n```"
              :macro true}
    gradient @{:doc "(Color only!) An approximation of the 2D distance field gradient at `Q`."
               :value [:var "gradient" :vec2]}
    gray @{:value [0.5 0.5 0.5]}
    graydient @{:doc "The default background color, a gray gradient."
                :value [do]}
    green @{:value [hsv 0.33333333333333331 0.98 1]}
    hash @{:doc "(hash v)\n\nReturn a pseudorandom float. The input can be a float or vector.\n\nThis should return consistent results across GPUs, unlike high-frequency sine functions."}
    hash2 @{:doc "(hash2 v)\n\nReturn a pseudorandom `vec2`. The input can be a float or vector.\n\nThis should return consistent results across GPUs, unlike high-frequency sine functions."}
    hash3 @{:doc "(hash3 v)\n\nReturn a pseudorandom `vec3`. The input can be a float or vector.\n\nThis should return consistent results across GPUs, unlike high-frequency sine functions."}
    hash4 @{:doc "(hash4 v)\n\nReturn a pseudorandom `vec4`. The input can be a float or vector.\n\nThis should return consistent results across GPUs, unlike high-frequency sine functions."}
    hexagon @{:doc "(hexagon radius [:r round])\n\n```example\n(hexagon 100 :r (osc t 2 20))\n```"}
    hexagram @{:doc "(hexagram radius [:r round])\n\n```example\n(hexagram 100 :r (osc t 2 20))\n```"}
    hot-pink @{:value [hsv 0.91666666666666663 0.98 1]}
    hsl @{:doc "(hsl hue saturation lightness)\n\nReturns a color."}
    hsv @{:doc "(hsv hue saturation value)\n\nReturns a color."}
    in @{:doc "(in & args)\n\n"}
    inf @{:doc "The number representing positive infinity"
          :value 9e999}
    int @{}
    intersect @{:doc "(intersect & shapes [:r r] [:rs rs] [:distance distance] [:color color])\n\nIntersect two or more shapes. The named arguments here produce a smooth intersection,\nand are similar to the arguments to `union`.\n\nIf you're performing rounded intersections with surfaced shapes in 3D, the color\nfield produced by `:rs` might give more intuitive results. This is because\nthe color field of the first shape is only visible as a thin, two-dimensional\nsurface, and as soon as you start to blend it with the second shape it will be\novertaken.\n\nMeanwhile if you're working in 2D, or looking at the interior distance field of a 3D\nintersection (i.e. slicing into the intersection, or transplanting the color field),\nthe asymmetric `:r` rounding will probably be more intuitive."}
    inversesqrt @{}
    isolines @{:doc "A color that represents the visualization of the 2D gradient. This is\nthe default color used when rendering a 2D shape with no color field."
               :value [isolines]}
    isosceles-triangle @{:doc "(isosceles-triangle size)\n\nTODOC"}
    length @{}
    light-gray @{:value [0.75 0.75 0.75]}
    light/ambient @{:doc "(light/ambient color [offset] [:brightness brightness] [:hoist hoist])\n\nShorthand for `(light/point color (P + offset))`.\n\nWith no offset, the ambient light will be completely directionless, so it won't\ncontribute to specular highlights. By offsetting by a multiple of the surface\nnormal, or by the surface normal plus some constant, you can create an ambient\nlight with specular highlights, which provides some depth in areas of your scene\nthat are in full shadow."}
    light/directional @{:doc "(light/directional color dir dist [:shadow softness] [:brightness brightness] [:hoist hoist])\n\nA light that hits every point at the same angle.\n\nShorthand for `(light/point color (P - (dir * dist)))`."}
    light/map @{:doc "(light/map light f)\n\n`f` takes and returns a `Light` expression."}
    light/map-brightness @{:doc "(light/map-brightness light f)\n\n`f` takes and returns a `:float` expression."}
    light/map-color @{:doc "(light/map-color light f)\n\n`f` takes and returns a `:vec3` expression."}
    light/point @{:doc "(light/point color position [:shadow softness] [:brightness brightness] [:hoist hoist])\n\nReturns a new light, which can be used as an input to some shading functions.\n\nAlthough this is called a point light, the location of the \"point\" can vary\nwith a dynamic expression. A light that casts no shadows and is located at `P`\n(no matter where `P` is) is an ambient light. A light that is always located at\na fixed offset from `P` is a directional light.\n\nBy default lights don't cast shadows, but you can change that by passing a\n`:shadow` argument. `0` will cast hard shadows, and any other expression will\ncast a soft shadow (it should be a number roughly in the range `0` to `1`).\n\nShadow casting affects the `brightness` of the light. You can also specify a baseline\n`:brightness` explicitly, which defaults to `1`.\n\nShadow casting always occurs in the global coordinate space, so you should position\nlights relative to `P`, not `p`.\n\nBy default light calculations are hoisted. This is an optimization that's helpful\nif you have a light that casts shadows that applies to multiple shaded surfaces that\nhave been combined with a smooth `union` or `morph` or other shape combinator. Instead\nof computing shadows twice and mixing them together, the shadow calculation will be\ncomputed once at the top level of the shader. Note though that this will prevent you\nfrom referring to variables that don't exist at the top level -- e.g. anything defined\nwith `gl/let`, or the index argument of `tiled` shape. If you want to make a light that\ndynamically varies, pass `:hoist false`."}
    light? @{:doc "(light? value)\n\nReturns `true` if `value` is a GLSL expression with type `Light`."}
    lime @{:value [hsv 0.25 0.98 1]}
    line @{:doc "(line start end width)\n\nTODOC"}
    log @{}
    log2 @{}
    magenta @{:value [hsv 0.83333333333333337 0.98 1]}
    map-color @{:doc "(map-color shape f)\n\nApply a function `f` to the shape's color field. `f` should take and return a\n`:vec3` expression.\n\nThe returned shape has the same dimensions as the input.\n\nThis differs from `shape/map-color` in that the expression is wrapped in `gl/let`,\nso you can refer to it multiple times."}
    map-distance @{:doc "(map-distance shape f)\n\nApply a function `f` to the shape's distance field. `f` should take and return a\n`:float` expression.\n\nThe returned shape has the same dimensions as the input.\n\nThis differs from `shape/map-distance` in that the expression is wrapped in `gl/let`,\nso you can refer to it multiple times."}
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
    mirror @{:doc "(mirror shape & axes [:r r])\n\nMirror a shape across one or more axes. Normally this takes the absolute value\nof the coordinates, but if you supply `:r` it will take a biased square root to\ngive a smooth mirror effect.\n\n```example\n(box 50 | rotate x t y t\n| move x 50\n| mirror x :r (sin t * 20 | max 0))\n```"}
    mix @{}
    mod @{}
    morph @{:doc "(morph shape1 amount shape2 [:distance amount] [:color amount])\n\nMorph linearly interpolates between two shapes.\n\n```\n# 50% box, 50% sphere\n(box 50 | morph (ball 50))\n\n# 75% box, 25% sphere\n(box 50 | morph 0.25 (ball 50))\n```\n\nConcretely this means that it returns a new shape whose individual fields\nare linear interpolations of its inputs. With an anonymous `amount` coefficient,\nboth the distance and color fields will be interpolated with the same value.\nBut you can also specify per-field overrides:\n\n```\n# distance is a 50% blend, but the color is 90% red\n(box 50 | color [1 0 0] | morph :color 0.1 (ball 50 | color [0 1 0]))\n```"}
    move @{:doc "(move subject & args)\n\nTranslate a shape. Usually you'd use this with a vector offset:\n\n```\n(move (box 50) [0 100 0])\n```\n\nBut you can also provide a vector and a scalar:\n\n```\n(move (box 50) y 100)\n```\n\nWhich is the same as `(move (box 50) (y * 100))`.\n\nIf you provide multiple vector-scalar pairs, their sum is the final offset:\n\n```\n(move (box 50) x 100 y 100 -z 20)\n```\n\nThat is the same as `(move (box 50) (+ (x * 100) (y * 100) (-z * 100)))`.\n\n`move` can take a shape, a vector, or a camera."}
    nearest-distance @{:doc "(nearest-distance)\n\nThis is the forward declaration of the function that will become the eventual\ndistance field for the shape we're rendering. This is used in the main raymarcher,\nas well as the shadow calculations. You can refer to this function to sample the\ncurrent distance field at the current value of `p` or `q`, for example to create\na custom ambient occlusion value."}
    normal @{:doc "(Color only!) A normalized vector that approximates the 3D distance field gradient at `P` (in other words, the surface normal for shading)."
             :value [:var "normal" :vec3]}
    normal+ @{:doc "A color that represents the visualization of the 3D normal. This is\nthe default color used when rendering a 3D shape with no color field."
              :value [+ 0.5 [* 0.5 normal]]}
    normalize @{}
    not @{}
    not-equal @{}
    not= @{}
    occlusion @{:doc "(occlusion [:steps step-count] [:dist dist] [:dir dir] [:hoist hoist])\n\nApproximate ambient occlusion by sampling the distance field at `:steps` positions\n(default 8) linearly spaced from 0 to `:dist` (default `10`). The result will range\nfrom 1 (completely unoccluded) to 0 (fully occluded).\n\nBy default the occlusion samples will be taken along the surface normal of the point\nbeing shaded, but you can pass a custom `:dir` expression to change that. You can use\nthis to e.g. add jitter to the sample direction, which can help to improve the\nquality.\n\nOcclusion is somewhat expensive to calculate, so by default the result will\nbe hoisted, so that it's only calculated once per iteration (without you having to\nexplicitly `gl/def` the result). However, this means that the occlusion calculation\nwon't take into account local normal adjustments, so you might want to pass\n`:hoist false`."}
    octagon @{:doc "(octagon radius [:r round])\n\n```example\n(octagon 100 :r (osc t 2 20))\n```"}
    octahedron @{:doc "(octahedron radius)\n\nReturns a 3D shape.\n\n```example\n(octahedron 100 | rotate x t)\n```"}
    or @{}
    orange @{:value [hsv 0.083333333333333329 0.98 1]}
    oriented-rect @{:doc "(oriented-rect start end width)\n\nTODOC"}
    osc @{:doc "(osc &opt period lo hi)\n\nReturns a number that oscillates with the given period. There are several overloads:\n\n```\n# 0 to 1 to 0 every second\n(osc t)\n\n# 0 to 1 to 0 every 10 seconds\n(osc t 10)\n\n# 0 to 100 to 0 every 10 seconds\n(osc t 10 100)\n\n# 50 to 100 to 50 every 10 seconds\n(osc t 10 50 100)\n```"}
    oss @{:doc "(oss &opt period lo hi)\n\nLike `osc`, but uses a sine wave instead of a cosine wave,\nso the output begins halfway between `lo` and `hi`."}
    outer-product @{}
    p @{:doc "The local point in 3D space. This is position of the current ray, with any transformations applied to it."
        :value [:var "p" :vec3]}
    parallelogram @{:doc "(parallelogram size skew)\n\nReturns a 2D shape. `size.x` is the width of the top and bottom edges, and `size.y` \nis the height of the parellogram.\n\n```example\n(parallelogram [80 100] (sin t * 100))\n```\n\n`skew` is how far the pallorelogram leans in the `x` direction, so the total\nwidth of the prellogram is `(size.x + skew) * 2`. A `skew` of `0` gives the\nsame shape as `rect`."}
    pentagon @{:doc "(pentagon radius [:r round])\n\n```example\n(pentagon 100 :r (osc t 2 20))\n```"}
    perlin @{:doc "(perlin point)\n\nReturns perlin noise ranging from `-1` to `1`. The input `point` can be a vector of any dimension.\n\nUse `perlin+` to return noise in the range `0` to `1`."}
    perlin+ @{:doc "(perlin+ point)\n\nPerlin noise in the range `0` to `1`.\n\n```example\n(ball 100 | color (perlin+ (p.xy / 10) | vec3))\n```\n```example\n(ball 100 | color (perlin+ (p / 10) | vec3))\n```\n```example\n(ball 100 | color (perlin+ [(p / 10) t] | vec3))\n```"}
    perspective-vector @{:doc "(perspective-vector fov)\n\nReturns a unit vector pointing in the `+z` direction for the\ngiven camera field-of-view (in degrees)."}
    pi @{:doc "I think it's around three.\n\nNote that there are also values like `pi/4` and `pi/6*5` and related helpers all the way up to `pi/12`. They don't show up in autocomplete because they're annoying, but they're there."
         :value 3.1415926535897931}
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
    pie @{:doc "(pie radius angle)\n\nReturns a 2D shape, something like a pie slice or a pacman depending on `angle`.\n\n```example\n(pie 100 (osc t 5 tau))\n```"}
    pivot @{:doc "(pivot (operation subject & args) point)\n\nApply a transformation with a different pivot point. You can combine this with any\noperation, but it's probably most useful with `rotate` and `scale`.\n\nThis is a syntactic transformation, so it requires a particular kind of invocation.\nIt's designed to fit into a pipeline, immediately after the operation you want to apply:\n\n```example\n# rotate around one corner\n(rect 50 | rotate t | pivot [50 50])\n```\n\nThis essentially rewrites its argument to:\n\n```example\n(gl/let [$pivot [50 50]]\n  (rect 50 | move (- $pivot) | rotate t | move $pivot))\n```"
            :macro true}
    plane @{:doc "(plane normal [offset])\n\nReturns a 3D shape that represents the infinite extrusion\nof a plane in a single direction. `normal` should be a\nnormalized vector.\n\nBecause of its infinite size and featurelessness, this shape\nis difficult to visualize on its own. You'll probably want to\nuse it in combination with boolean operations:\n\n```example\n(ball 100 | intersect (plane x (sin t * 50)))\n```\n\n`(plane x)` is the shape that faces the `+x` direction and extends\ninfinitely in the `-x` direction, and a positive offset will move it\nin the `+x` direction."}
    pow @{}
    product @{:doc "(product v)\n\nMultiply the components of a vector."}
    purple @{:value [hsv 0.75 0.98 1]}
    q @{:doc "The local point in 2D space. This is the position being shaded, with any transformations applied."
        :value [:var "q" :vec2]}
    quad-circle @{:doc "(quad-circle radius)\n\nReturns a 2D shape, an approximation of a circle made out of quadratic bezier curves.\n\n```example\n(quad-circle 100)\n```\n\nIt's like a circle, but quaddier."}
    quantize @{:doc "(quantize value count)\n\nRounds a value to the nearest multiple of `count`."}
    r2 @{:doc "A 2D shape with zero distance everywhere."
         :value {:fields {:distance [<2>
                                     literal
                                     [<3> primitive [<4> float]]
                                     0]}
                 :tag <1>
                 :type [<3> vec [<4> float] 2]}}
    r3 @{:doc "A 2D shape with zero distance everywhere."
         :value {:fields {:distance [<2>
                                     literal
                                     [<3> primitive [<4> float]]
                                     0]}
                 :tag <1>
                 :type [<3> vec [<4> float] 3]}}
    radial @{:doc "(radial shape [axis] count [offset] [:oversample oversample] [:sample-from sample-from] [:sample-to sample-to])\n\nTODOC"}
    radial* @{:doc "(radial* [axis] count [offset] get-shape [:oversample oversample] [:sample-from sample-from] [:sample-to sample-to])\n\nTODOC"}
    radial: @{:doc "(radial: shape $i & args)\n\nTODOC"
              :macro true}
    radians @{}
    ray @{:doc "The current ray being used to march and shade the current fragment. This always represents\nthe ray from the camera, even when raymarching for shadow casting.\n\nA ray has two components: an `origin` and a `dir`ection. `origin` is a point in the \nglobal coordinate space, and you can intuitively think of it as \"the location of the camera\"\nwhen you're using the default perspective camera (orthographic cameras shoot rays from different\norigins).\n\nThe direction is always normalized."
          :value [:var "ray" Ray]}
    ray? @{:doc "(ray? value)\n\nReturns `true` if `value` is a GLSL expression with type `Ray`."}
    recolor @{:doc "(recolor dest-shape source-shape)\n\nReplaces the color field on `dest-shape` with the color field on `source-shape`. Does not affect the distance field."}
    rect @{:doc "(rect size [:r radius])\n\nReturns a 2D shape, a rectangle with corners at `(- size)` and `size`. `size` will be coerced to a `vec2`.\n\nThink of `size` like the \"radius\" of the rect: a rect with `size.x = 50` will be `100` units wide.\n\n`radii` can be a single radius or a `vec4` of `[top-left` `top-right` `bottom-right` `bottom-left]`.\n\n```example\n(union\n  (rect 50 | move [-100 100])\n  (rect 50 :r 10 | move [100 100])\n  (rect 50 :r [0 10 20 30] | move [-100 -100])\n  (rect 50 :r [0 30 0 30] | move [100 -100]))\n```"}
    red @{:value [hsv 0 0.98 1]}
    reflect @{}
    refract @{}
    remap+ @{:doc "(remap+ x)\n\nLinearly transform a number in the range `[-1 1]` to `[0 1]`."}
    remap- @{:doc "(remap- x)\n\nLinearly transform a number in the range `[-1 1]` to `[0 -1]`."}
    resolution @{:doc "The size, in physical pixels, of the canvas being rendered. In quad view, this will be smaller than the physical canvas."
                 :value [:var "resolution" :vec2]}
    revolve @{:doc "(revolve shape axis &opt offset)\n\nRevolve a 2D shape around the given `axis` to return a 3D shape.\n\nYou can optionally supply an `offset` to move the shape away from the origin first (the default is `0`)."}
    rhombus @{:doc "(rhombus size)\n\nReturns a 2D shape. It rhombs with a kite.\n\n```example\n(rhombus [100 (osc t 3 50 150)])\n```"}
    ring @{:doc "(ring radius angle thickness)\n\n```example\n(ring 100 (osc t 5 tau) (osc t 2 5 20))\n```"}
    rotate @{:doc "(rotate subject & args)\n\nRotate a shape or a vector. Positive angles are counter-clockwise rotations.\n\nIn 3D, the arguments should be pairs of `axis angle`. For example:\n\n```example\n(rotate (box 100) x t y (sin t))\n```\n\nAll `axis` arguments must be unit vectors. There are built-in axis variables `x`/`+y`/`-z`\nfor the cardinal directions, and these produce optimized rotation matrices. But you can\nrotate around an arbitrary axis:\n\n```example\n(rotate (box 100) [1 1 1 | normalize] t)\n```\n\nThe order of the arguments is significant, as rotations are not commutative.\n\nThe first argument to `rotate` can be a shape, vector, or camera.\n\nIn 2D, the arguments should just be angles; no axis is allowed.\n\nYou can use `rotate` to make lots of cool effects. By varying the angle\nof rotation, you can create twists:\n\n```example\n(box [50 100 50] | rotate y (p.y / 100 * (cos+ t)))\n```\n\nTwirls:\n\n```example\n(box [100 50 100] | rotate y (length p.xz / 50 * (cos+ t)))\n```\n\nAnd bends:\n\n```example\n(box [50 100 100] | rotate y (p.z / 100 * (cos+ t)))\n```\n\nOr any number of other cool effects!\n\n```example\n(box [50 100 50] | rotate y (sin (p.y / 10) * sin t * 0.2))\n```"}
    rotation-around @{:doc "(rotation-around axis angle)\n\nA rotation matrix about an arbitrary axis. More expensive to compute than the axis-aligned rotation matrices."}
    rotation-matrix @{:doc "(rotation-matrix & args)\n\nReturn a rotation matrix. Takes the same arguments as `rotate`, minus the initial thing to rotate."}
    rotation-x @{:doc "(rotation-x angle)\n\nA rotation matrix about the X axis."}
    rotation-y @{:doc "(rotation-y angle)\n\nA rotation matrix about the Y axis."}
    rotation-z @{:doc "(rotation-z angle)\n\nA rotation matrix about the Z axis."}
    round @{}
    round-even @{}
    scale @{:doc "(scale shape & args)\n\nScale a shape. If the scale factor is a float, this will produce an exact\ndistance field.\n\n```example\n(rect 50 | scale 2)\n```\n\nIf the scale factor is a vector, space will be distorted by the smallest\ncomponent of that vector, and produce an approximate distance field:\n\n```example\n(rect 50 | scale [2 1])\n```\n\nWith an even number of arguments, `scale` expects `axis amount` pairs.\nUnlike `rotate`, it won't work with arbitrary axes -- you must give it\na cardinal axis.\n\n```example\n(rect 50 | scale x 0.5 y 2)\n```"}
    shape/2d @{:doc "(shape/2d distance)\n\nReturns a new 2D shape with the given distance field."}
    shape/3d @{:doc "(shape/3d distance)\n\nReturns a new 3D shape with the given distance field."}
    shape/color @{:doc "(color shape)\n\nShorthand for `(shape/get-field shape :color)`."}
    shape/distance @{:doc "(distance shape)\n\nShorthand for `(shape/get-field shape :distance)`."}
    shape/get-field @{:doc "(get-field shape field)\n\nLook up a single field on a shape. If the field does not exist, this will return `nil`."}
    shape/map @{:doc "(map shape f &opt type)\n\nAlter the fields on a shape, optionally changing its dimension in the process.\n\n`f` will be called with the value of the field. If you want to do something different\nfor each field, use `shape/map-fields`."}
    shape/map-color @{:doc "(map-color shape f)\n\nShorthand for `(shape/map-field shape :color f)`."}
    shape/map-distance @{:doc "(map-distance shape f)\n\nShorthand for `(shape/map-field shape :distance f)`."}
    shape/map-field @{:doc "(map-field shape field f)\n\nMap a single field on a shape. If the field does not exist, this does nothing."}
    shape/map-fields @{:doc "(map-fields shape f &opt type)\n\nLike `shape/map`, but `f` will be called with two arguments: the field name (as a keyword) and its value."}
    shape/merge @{:doc "(merge shapes f)\n\nMerge multiple shapes together. `shapes` should be a list of shapes that all\nhave the same dimension.\n\n`f` will be called with an array of all of the fields from each shape, and\nshould return a struct with the fields for the new shape.\n\n`merge` returns a new shape with the same dimension as its inputs."}
    shape/new @{:doc "(new type & fields)\n\nReturns a new shape with the given type and fields.\n\n```\n# red circle with radius 10\n(shape/new jlsl/type/vec2\n  :distance (length q - 10)\n  :color [1 0 0])\n```"}
    shape/shape? @{:doc "(shape? value)\n\nReturns `true` if `value` is a shape."}
    shape/transplant @{:doc "(transplant dest-shape field source-shape)\n\nShorthand for `(shape/with dest-shape field (shape/get-field source-shape field))`."}
    shape/type @{:doc "(type shape)\n\nReturns the dimension of a shape, as a JLSL type equal to the dimension of a point\nin the shape -- either `vec2` or `vec3`."}
    shape/with @{:doc "(with shape & new-kvs)\n\nReplace arbitrary fields on a shape.\n\nYou probably don't want to use this. Theoretically shapes\nin Bauble are collections of arbitrary fields, but in practice\n`:color` and `:distance` are the only fields that are really\nsupported in a meaningful way.\n\nBut you could associate other fields with shapes, and use that to\nmodel, for example, analytic normals. But none of the existing\ninfrastructure will understand you if you do this."}
    shape? @{:doc "(shape? value)\n\nReturns `true` if `value` is a shape."}
    shell @{:doc "(shell shape &opt thickness)\n\nReturns a hollow version of the provided shape (the absolute value of the distance field).\n\n```example\n(circle 100 | shell 5)\n```\n\nIn 3D, it's hard to see the effect without cutting into the result:\n\n```example\n(ball 100 | shell 5 | intersect (plane x (osc t 3 0 100)))\n```"}
    sign @{}
    sin @{}
    sin+ @{:doc "(sin+ x)\n\nLike `sin`, but returns a number in the range `0` to `1`."}
    sin- @{:doc "(sin- x)\n\nLike `sin`, but returns a number in the range `0` to `-1`."}
    sinh @{}
    sky @{:value [hsv 0.58333333333333337 0.98 1]}
    slice @{:doc "(slice shape axis &opt position)\n\nTake a 2D slice of a 3D shape at a given `position` along the supplied `axis`.\n\n`position` defaults to `0`."}
    slow @{:doc "(slow shape amount)\n\nScales the shape's distance field, causing the raymarcher to converge more slowly.\nThis is useful for raymarching distance fields that vary based on `p` -- shapes\nthat don't actually provide an accurate distance field unless you are very close\nto their surfaces.\n\n```example\n(box 100\n| rotate y (p.y / 30)\n| rotate x t\n| slow (osc t 5 1 0.25))\n```"}
    smoothstep @{}
    sphere @{:doc "(sphere radius)\n\nReturns a 3D shape. This is an alias for the float overload of `ball`."}
    sqrt @{}
    ss @{:doc "(ss x &opt from-range to-range)\n\nThis is a wrapper around `smoothstep` with a different argument order, which also\nallows the input edges to occur in descending order.\n\nThere are several overloads:\n\n```\n(ss x)\n# becomes\n(smoothstep 0 1 x)\n```\n\n```\n(ss x [from-start from-end])\n# becomes\n(if (< from-start from-end)\n  (smoothstep from-start from-end x)\n  (1 - smoothstep from-end from-start x))\n``` \n\n```\n(ss x from [to-start to-end])\n# becomes\n(ss x from * (- to-end to-start) + to-start)\n```"}
    star @{:doc "(star outer-radius inner-radius [:r round])\n\n```example\n(star 100 70 :r (osc t 2 20))\n```"}
    step @{}
    subject @{:doc "A variable that determines what Bauble will render.\n\nYou can set this variable explicitly to change your focus, or use\nthe `view` macro to change your focus. If you don't set a subject,\nBauble will render the last shape in your script."
              :ref @[nil]}
    subtract @{:doc "(subtract & shapes [:r r] [:rs rs] [:distance distance] [:color color])\n\nSubtract one or more shapes from a source shape. The named arguments\nhere produce a smooth subtraction, and are similar to the arguments to `union`.\n\nIf you're performing rounded subtractions with surfaced shapes in 3D, the color\nfield produced by `:rs` might give more intuitive results. This is because\nthe color field of the first shape is only visible as a thin, two-dimensional\nsurface, and as soon as you start to blend it with the second shape it will be\novertaken.\n\nMeanwhile if you're working in 2D, or looking at the interior distance field of a 3D\nsubtraction (i.e. slicing into the subtracting shape), the asymmetric `:r` rounding\nwill probably be more intuitive.\n"}
    sum @{:doc "(sum v)\n\nAdd the components of a vector."}
    t @{:doc "The current time in seconds."
        :value [:var "t" :float]}
    tan @{}
    tanh @{}
    tau @{:doc "Bigger than six, but smaller than seven.\n\nNote that there are also values like `tau/4` and `tau/6*5` and related helpers all the way up to `tau/12`.  They don't show up in autocomplete because they're annoying, but they're there."
          :value 6.2831853071795862}
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
    teal @{:value [hsv 0.41666666666666669 0.98 1]}
    tile @{:doc "(tile shape size [:limit limit] [:oversample oversample] [:sample-from sample-from] [:sample-to sample-to])\n\nRepeat the region of space `size` units around the origin. Pass `:limit` to constrain\nthe number of repetitions. See `tile:` or `tile*` if you want to produce a shape that\nvaries as it repeats.\n\nTo repeat space only along some axes, pass `0`. For example, to only tile in the `y` direction:\n\n```example\n(tile (ball 50) [0 100 0])\n```\n\nIf you're repeating a shape that is not symmetric, you can use `:oversample true` to evaluate\nmultiple instances at each pass, essentially considering the distance not only to this\ntile, but also to neighboring tiles. Compare these two distance fields:\n\n```example\n(rect 30 | rotate 0.3 | tile [80 80] :oversample false)\n```\n```example\n(rect 30 | rotate 0.3 | tile [80 80] :oversample true)\n```\n\nThe default oversampling is `:sample-from 0` `:sample-to 1`, which means looking at one adjacent\ntile, asymmetrically based on the location of the point (so when evaluating a point near\nthe right edge of a tile, it will look at the adjacent tile to the right, but not the tile\nto the left). By passing `:sample-from -1`, you can also look at the tile to the left.\nBy passing `:sample-from 0 :sample-to [2 1 1]`, it will look at two tiles to the right in the\n`x` direction, and one tile up/down/in/out.\n\nThis can be useful when raymarching a 3D space where each tile is quite different, but note\nthat it's very costly to increase these values. If you're tiling a 3D shape in all directions,\nthe default `:oversample` parameters will do 8 distance field evaluations;\n`:sample-from -1` `:sample-to 1` will do 27."}
    tile* @{:doc "(tile* size get-shape [:limit limit] [:oversample oversample] [:sample-from sample-from] [:sample-to sample-to])\n\nLike `tile`, but the shape is a result of invoking `get-shape` with one argument,\na GLSL variable referring to the current index in space. Unlike `tile`, `size` must\nbe a vector that determines the dimension of the index variable.\n\n```example\n(tile* [10 10] (fn [$i] \n  (circle 5 \n  | color (hsv (hash $i) 0.5 1))))\n```\n\nYou can use this to generate different shapes or colors at every sampled tile. The index\nwill be a vector with integral components that represents  being considered. So for\nexample, in 3D, the shape at the origin has an index of `[0 0 0]` and the shape above\nit has an index of `[0 1 0]`."}
    tile: @{:doc "(tile: shape $i & args)\n\nLike `tile*`, but its first argument should be a form that will\nbecome the body of the function. Basically, it's a way to create\na repeated shape where each instance of the shape varies, and it's\nwritten in a way that makes it conveniently fit into a pipeline:\n\n```example\n(circle 5 \n| color (hsv (hash $i) 0.5 1) \n| tile: $i [10 10])\n```"
            :macro true}
    torus @{:doc "(torus axis radius thickness)\n\nReturns a 3D shape, a torus around the provided `axis`.\n\n```example\n(torus z 100 (osc t 2 10 50))\n```"}
    trapezoid @{:doc "(trapezoid bottom-width top-width height [:r round])\n\nReturns a 2D shape.\n\n```example\n(trapezoid (osc t 3 50 100) (oss t 2 100 50) 100)\n```"}
    triangle-points @{:doc "(triangle-points a b c)\n\nTODOC"}
    trunc @{}
    uint @{}
    uneven-capsule @{:doc "(uneven-capsule bottom-radius top-radius height)\n\n```example\n(uneven-capsule 50 (osc t 3 20 60) (oss t 8 30 100))\n```"}
    union @{:doc "(union & shapes [:r r] [:rs rs] [:distance distance] [:color color])\n\nUnion two or more shapes together. Pass `:r` or `:rs` to produce a smooth union.\n\n`:r` and `:rs` combine color fields differently. `:rs` is a symmetric union, where\nthe color field is based on the nearest shape, regardless of the order that they're\nspecified. `:r` is an asymmetric union where the order matters, and later shapes will\nbe considered \"on top of\" previous shapes.\n\nThese produce identical colors on the surface, but different interior color fields.\nIt's easy to see the difference in 2D, while in 3D the difference only matters if\nyou're cutting into a shape, or transplanting the color field from one shape to another.\n\nYou can also pass `:distance` or `:color` to specify a different smoothing radius for\nthe separate fields. For example, to produce a smooth symmetric color union with a sharp\ndistance field, pass `(union :rs 10 :distance 0 ;shapes)`."}
    vec @{}
    vec2 @{}
    vec3 @{}
    vec4 @{}
    view @{:doc "(view subject)\n\nA shorthand for `(set subject _)` that fits nicely into pipe notation, e.g. `(ball 50 | view)`."
           :macro true}
    viewport @{:doc "You don't have to think about this value unless you're implementing a custom `main` function,\nwhich you probably aren't doing.\n\nThis represents the portion of the canvas currently being rendered. The `xy` components are the start\n(bottom left) and the `zw` coordinates are the size.\n\nNormally this will be equal to `[[0 0] resolution]`, but when rendering quad-view or a chunked render,\nit may have a different origin or resolution.\n\nYou can use `(gl_FragCoord.xy - viewport.xy)` in order to get the logical fragment position (the value\nexposed to a typical shader as `Frag-Coord`)."
               :value [:var "viewport" :vec4]}
    white @{:value [1 1 1]}
    with-lights @{:doc "(with-lights shape & lights)\n\nEvaluate `shape` with the `*lights*` dynamic variable set to the provided lights.\n\nThe argument order makes it easy to stick this in a pipeline. For example:\n\n```example\n(ball 100\n| blinn-phong [1 0 0]\n| with-lights\n  (light/point 0.5 [100 100 0])\n  (light/ambient 0.5))\n```"
                  :macro true}
    worley @{:doc "(worley point)\n\nWorley noise, also called cellular noise or voronoi noise.\nThe input `point` can be a `vec2` or a `vec3`.\n\n```example\n(ball 100 | color (worley (p.xy / 30) | vec3))\n```\n```example\n(ball 100 | color (worley (p / 30) | vec3))\n```\n\nReturns the nearest distance to points distributed randomly within the tiles of a square or cubic grid."}
    worley2 @{:doc "(worley2 point)\n\nLike `worley`, but returns the nearest distance in `x` and the second-nearest distance in `y`.\n\n```example\n(ball 100 | color [(worley2 (p.xy / 30)) 1])\n```\n```example\n(ball 100 | color [(worley2 (p / 30)) 1])\n```"}
    x @{:doc "`[1 0 0]`" :value [1 0 0]}
    xor @{}
    y @{:doc "`[0 1 0]`" :value [0 1 0]}
    yellow @{:value [hsv 0.16666666666666666 0.98 1]}
    z @{:doc "`[0 0 1]`" :value [0 0 1]}})

(use .)
(import ../shape)

(defmacro* test-shape [expr & args]
  ~(test (,shape/map ,((|sugar) expr) ,jlsl/show) ,;args))

(deftest "rotate"
  # just make sure valid use cases don't error
  (test-shape (rotate (rect 10) 10)
    {:fields {:distance [rotate-outer]}
     :tag <1>
     :type [<2> vec [<3> float] 2]})
  (test-shape (rotate (box 10) x 20)
    {:fields {:distance [rotate-outer]}
     :tag <1>
     :type [<2> vec [<3> float] 3]})
  (test-shape (rotate (box 10) [1 2 3] 20)
    {:fields {:distance [rotate-outer]}
     :tag <1>
     :type [<2> vec [<3> float] 3]})
  (test-shape (rotate (box 10) x 1 y 2)
    {:fields {:distance [rotate-outer]}
     :tag <1>
     :type [<2> vec [<3> float] 3]})
  (test-error (rotate (rect 10) x 10) "expected angle, got (1 0 0)")
  (test-error (rotate (box 10) 10 20) "rotation-around: no overload for arguments [:float :float]")
  (test-error (rotate (box 10) 10) "angle required")
  (test-error (rotate (box 10) x 10 [1 2] 3) "rotation-around: no overload for arguments [:vec2 :float]"))

(deftest "morph coefficient defaults to 0.5"
  (test-shape (morph (box 10) (ball 10))
    {:fields {:distance [mix [sdf-cube 10] [sdf-sphere 10] 0.5]}
     :tag <1>
     :type [<2> vec [<3> float] 3]}))

(deftest "morph coefficient can appear in any position"
  (test-shape (morph 0.25 (box 10) (ball 10))
    {:fields {:distance [mix [sdf-cube 10] [sdf-sphere 10] 0.25]}
     :tag <1>
     :type [<2> vec [<3> float] 3]})

  (test-shape (morph (box 10) 0.25 (ball 10))
    {:fields {:distance [mix [sdf-cube 10] [sdf-sphere 10] 0.25]}
     :tag <1>
     :type [<2> vec [<3> float] 3]})

  (test-shape (morph (box 10) (ball 10) 0.25)
    {:fields {:distance [mix [sdf-cube 10] [sdf-sphere 10] 0.25]}
     :tag <1>
     :type [<2> vec [<3> float] 3]})
  )

(deftest "morph will evaluate its coefficient argument multiple times if it has more than two shapes"
  # this is bad, but it's not easy to fix and no one will ever do this anyway
  (test-shape (morph (sin t) (box 10) (ball 10) (box 20))
    {:fields {:distance [mix
                         [mix
                          [sdf-cube 10]
                          [sdf-sphere 10]
                          [sin t]]
                         [sdf-cube 20]
                         [sin t]]}
     :tag <1>
     :type [<2> vec [<3> float] 3]}))

(deftest "morph with per-field coefficients"
  # this is bad, but it's not easy to fix and no one will ever do this anyway
  (test-shape (morph :distance 0.9 (box 10 | color [1 1 0]) (ball 10 | color [1 0 1]))
    {:fields {:color [mix [vec3 1 1 0] [vec3 1 0 1] 0.5]
              :distance [mix [sdf-cube 10] [sdf-sphere 10] 0.9]}
     :tag <1>
     :type [<2> vec [<3> float] 3]}))

(deftest "morph with nonsense coefficients"
  # eh it would be nice to error but i don't really care
  (test-shape (morph :foo 0.9 (box 10 | color [1 1 0]) (ball 10 | color [1 0 1]))
    {:fields {:color [mix [vec3 1 1 0] [vec3 1 0 1] 0.5]
              :distance [mix [sdf-cube 10] [sdf-sphere 10] 0.5]}
     :tag <1>
     :type [<2> vec [<3> float] 3]}))

(deftest "morph with a single shape"
  (test-shape (morph (box 10))
    {:fields {:distance [sdf-cube 10]}
     :tag <1>
     :type [<2> vec [<3> float] 3]}))

(deftest "invalid morphs"
  (test-error (morph) "no shapes to combine")
  (test-error (morph (box 10) :color) "no value for :color")
  (test-error (morph (box 10) :color [1 2]) "type mismatch: expected :float, got :vec2"))

(test (jlsl/show (+ p 1)) [+ p 1])
(test (jlsl/show (+ p [1 2 3])) [+ p [vec3 1 2 3]])

(test (jlsl/show (gl/let [r 10] (* r 2))) [let-outer])

(test-macro
  (gl/let [r (typecheck r jlsl/type/float)]
    (map-axes shape axes (fn [x] (sqrt (+ (* x x) (* r r))))))
  (do
    (def <1> (@coerce-expr (typecheck r jlsl/type/float)))
    (def r (@new "r" (@expr/type <1>)))
    (def <2> (do (map-axes shape axes (fn [x] (sqrt (+ (* x x) (* r r)))))))
    (if (@shape? <2>)
      (@map <2> (fn [<3>] (@with-expr @[[r <1>]] [] <3> "let")))
      (@with-expr @[[r <1>]] [] (@coerce-expr <2>) "let"))))

(test-macro (pivot (scale (rect 30) 0.75) [30 30])
  (as-macro @gl/let [<1> [30 30]] (@move (scale (@move (rect 30) (- <1>)) 0.75) <1>)))

(test-macro (pivot (rotate [1 2 3] z 0.75) [30 30])
  (as-macro @gl/let [<1> [30 30]] (@move (rotate (@move [1 2 3] (- <1>)) z 0.75) <1>)))

(test-macro (sugar
  (s + s | gl/let [s (sin t)] _))
  (gl/let [s (sin t)] (+ s s)))

(test (jlsl/show (ss 10))
  [smoothstep 0 1 10])
(test (jlsl/show (ss 10 [1 2]))
  [smoothstep 1 2 10])
(test (jlsl/show (ss 10 [2 1]))
  [- 1 [smoothstep 1 2 10]])
(test (jlsl/show (ss 10 [2 1] [100 120]))
  [+ [* [- 1 [smoothstep 1 2 10]] 20] 100])

