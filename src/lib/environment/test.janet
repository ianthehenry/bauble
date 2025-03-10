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
    *lights* @{:doc "The default lights used by the `shade` function.\nYou can manipulate this using `setdyn` or `with-dyns` like any other\ndynamic variable, but there is a dedicated `with-lights` function to\nset it in a way that fits nicely into a pipeline."
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
    Frag-Coord @{:doc "The center of the current pixel being rendered. Pixel centers are at `[0.5 0.5]`,\nso with no anti-aliasing this will have values like `[0.5 0.5]`, `[1.5 0.5]`, etc.\nIf you're using multisampled antialiasing, this will have off-centered values\nlike `[0.3333 0.3333]`."
                 :value [:var "Frag-Coord" :vec2]}
    Light @{:doc "(Light color direction brightness)\n\n"}
    OrthographicCamera @{:doc "(OrthographicCamera position direction up scale)\n\n"}
    P @{:doc "The global point in 3D space. This is the position of the current ray before any\ntransformations are applied to it."
        :value [:var "P" :vec3]}
    PerspectiveCamera @{:doc "(PerspectiveCamera position direction up fov)\n\n"}
    Q @{:doc "The global point in 2D space."
        :value [:var "Q" :vec2]}
    Ray @{:doc "(Ray origin direction)\n\n"}
    aa-grid-size @{:doc "The size of the grid used to sample a single pixel. The total samples per pixel will\nbe the square of this number. The default value is 1 (no anti-aliasing)."
                   :ref @[nil]}
    abs @{}
    acos @{}
    acosh @{}
    align @{:doc "(align target from to)\n\nAlign a shape or a vector to another vector. Both the `from` and `to` vectors must have unit length.\n\nThis function is useful for \"pointing\" one shape towards another. For example:\n\n```example\n(def pos\n  [(sin (t * 1.0) * 100)\n   (sin (t * 1.5) * 100)\n   (cos (t * 2.0) * 100)])\n(union\n  (cone y 10 100 | align y (normalize pos))\n  (box 10 | move pos))\n```\n\nIf `from` = `(- to)`, the result is undefined: there are infinitely many rotation matrices that reverse\na vector's direction."}
    alignment-matrix @{:doc "(alignment-matrix from to)\n\nReturn a 3D rotation matrix that aligns one normalized vector to another.\n\nBoth input vectors must have a unit length!\n\nIf `from` = `(- to)`, the result is undefined."}
    and @{}
    arc @{:doc "(arc radius angle thickness)\n\n```example\n(arc 100 (osc t 5 tau) (osc t 3 5 20))\n```"}
    asin @{}
    asinh @{}
    atan @{}
    atan2 @{:doc "(atan2 y x)\n\nReturns a value in the range `[-pi, pi]` representing the angle\nbetween the (2D) `+x` axis and the point `[x y]`.\n\nThis is an alternative to the built-in `atan`'s two argument\noverload that is defined when `x = 0`. You can also invoke this\nwith a single `vec2` whose coordinates will act as `x` and `y`.\n\nSee `atan2+` for an angle in the range `[0, tau)`."}
    atan2+ @{:doc "(atan2+ y x)\n\nLike `atan2`, but returns a value in the range `[0, tau)` instead of\n`[-pi, pi]`."}
    atanh @{}
    background-color @{:doc "A variable that determines the background color of the canvas.\n\nDefault is `graydient`. This can be a vec3 or a vec4:\n\n```example\n(ball 100)\n(set background-color transparent)\n```"
                       :ref @[[do]]}
    ball @{:doc "(ball size)\n\nReturns a 3D shape, which is either a sphere or an ellipsoid, depending on the type of `size`.\n\n```example\n(ball 100)\n```\n\n```example\n(ball [50 80 120])\n```\n\nEllipsoids do not have correct distance fields. Their distance field is only a bound, and\nit has strange isosurfaces that can make it combine with other shapes oddly:\n\n```example\n(ball [30 50 80] | slice y)\n```"}
    bezier @{:doc "(bezier shape start control end [:up up] [:from from] [:to to])\n\nReturns a 2D or 3D quadratic bezier curve, or extrudes a shape along that curve.\n\nA quadratic bezier curve is defined by three points: a start point, an end point, and a control point.\n\nIf you connect a line between the start and the control point, and another line between the control point\nand the end point, you will have two legs of a triangle. If you then move a point along each line,\ndraw a line between those two points, and then move a point along *that* line, you will get a quadratic\nbezier curve.\n\n```example\n(def start [-200 (osc (t + 20) 19.1 -200 200)])\n(def middle [0 (osc (t + 29) 20.2 -200 200)])\n(def end [200 (oss t 21.3 -200 200)])\n(def h (sin+ t))\n\n(def v1 (mix start middle h))\n(def v2 (mix middle end h))\n(union\n  (union :r 5\n    (line start middle 2)\n    (circle 3 | move v1)\n  | color red)\n  (union :r 5\n    (line middle end 2)\n    (circle 3 | move v2)\n  | color sky)\n  (bezier 2 start middle end | color white)\n  (union :r 5\n    (circle 5 | move (mix v1 v2 h))\n    (line v1 v2 2)\n  | color magenta))\n```\n\nMaybe better intuition is that you linearly interpolate between two pairs of points,\nthen linearly interpolate between that linear interpolation, you get a quadratic bezier\ncurve.\n\nLike `line`, bezier curves are defined in 2D or in 3D:\n\n```example\n(def start [-200 (osc (t + 20) 19.1 -200 200) (osc (t + 20) 21.5 -100 100)])\n(def middle [0 (osc (t + 29) 20.2 -200 200) (osc (t + 29) 19.2 -100 100)])\n(def end [200 (oss t 21.3 -200 200) (osc (t + 29) 18.5 -100 100)])\n(def h (sin+ t))\n\n(def v1 (mix start middle h))\n(def v2 (mix middle end h))\n(union\n  (union :r 5\n    (line start middle 2)\n    (ball 3 | move v1)\n  | color red)\n  (union :r 5\n    (line middle end 2)\n    (ball 3 | move v2)\n  | color sky)\n  (bezier 2 start middle end | color white)\n  (union :r 5\n    (ball 5 | move (mix v1 v2 h))\n    (line v1 v2 2)\n  | color magenta)\n| union (ground -210 | shade dark-gray))\n```\n\nThes simplest version of a bezier curve produces round lines:\n\n```example\n(bezier (osc t 3 1 10) [-100 0] [0 -100] [100 0] | color white)\n```\n\nBut you can also pass a shape instead of a float as the first argument,\nin which case you will *extrude* the shape along the curve, which you\ncan use to produce differently-shaped lines:\n\n```example\n(bezier (rect (osc t 3 1 10)) [-100 0] [0 -100] [100 0] | color white)\n```\n\nOr, in 3D, more interesting curves:\n\n```example\n(bezier (torus y 20 (osc t 3 1 10)) [-100 0 100] [0 -100 0] [100 0 -100])\n```\n\nYou can also vary the shape over the course of the extrusion, by passing a function\nas the first argument:\n\n```example\n(bezier (fn [$t] (mix 1 10 $t)) [-100 0] [0 -100] [100 0] | color white)\n```\n\nAlthough the `bezier:` helper gives you a slightly more convenient way to write\nthis that fits into a pipeline:\n\n```example\n(triangle [10 (osc $t 0.1 1 20)]\n| bezier: $t [-100 0] [0 -100] [100 0] | color white)\n```\n\nYou can also pass `:from` and `:to` to constrain the extrusion.\n\n```example\n(box 20 | shade red | subtract (ball 23 | shade green) | rotate z ($t * tau)\n| bezier: $t [-100 0 100] [0 -100 0] [100 0 -100] :to (osc t 3 0 1))\n```\n\nLike `elongate`, only a two-dimensional slice of a three-dimensional shape will be\nextruded along the curve. But if you vary the position of that shape as you extrude\nit, you can \"scan\" the entire shape, and produce a stretching effect instead:\n\n```example\n(gl/def to (osc t 3 0.1 1))\n(box 20 | shade red | subtract (ball 23 | shade green) | rotate z ($t * tau)\n| move z (mix -20 20 ($t / to))\n| bezier: $t [-100 0 100] [0 -100 0] [100 0 -100] :to to\n| slow 0.8)\n```\n\nThe relative curve position (`$t` in this case) is not clamped to `from`/`to`, which means\nthat if you're extruding a 3D shape, you might see unexpected results. For example, this\n2D extrusion is a perfect rainbow from red to red:\n\n```example\n(circle 5 | radial 6 16 | rotate ($t * tau)\n| shade (hsv $t 1 1)\n| bezier: $t [-100 0 100] [0 -100 0] [100 0 -100] :to (osc t 3 0 1))\n```\n\nBut this similar 3D extrusion is not:\n\n```example\n(octahedron 30 | radial z 6 16 | rotate z ($t * tau)\n| shade (hsv $t 1 1)\n| bezier: $t [-100 0 100] [0 -100 0] [100 0 -100] :to (osc t 3 0 1))\n```\n\nBecause the left edge of the shape has a negative `$t` value. You can explicitly clamp it:\n\n```example\n(gl/def to (osc t 3 0 1))\n(octahedron 30 | radial z 6 16 | rotate z ($t * tau)\n| shade (hsv $t 1 1)\n| gl/let [$t (clamp $t 0 to)] _\n| bezier: $t [-100 0 100] [0 -100 0] [100 0 -100] :to to)\n```\n\nIf that isn't what you want.\n\nThe shape will be oriented along the curve according to the vector `:up`, which determines\nwhat direction will become normal to the curve. The default is `+y`, but you can specify\nany normalized vector:\n\n```example\n(torus y 30 5\n| bezier [-100 0 100] [0 -100 0] [100 0 -100]\n  :up y\n  :from 0\n  :to (sin+ t))\n```\n```example\n(torus y 30 5\n| bezier [-100 0 100] [0 -100 0] [100 0 -100]\n  :up z\n  :from 0\n  :to (sin+ t))\n```\n```example\n(torus y 30 5\n| bezier [-100 0 100] [0 -100 0] [100 0 -100]\n  :up x\n  :from 0\n  :to (sin+ t))\n```"}
    bezier: @{:doc "(bezier: shape $t & args)\n\nLike `bezier`, but implicitly wraps its first argument in an anonymous function. See `bezier` for examples."
              :macro true}
    black @{:doc "  ```example\n  (set background-color black)\n  (ball 100 | shade black)\n  ```\n  "
            :value [0 0 0]}
    blinn-phong @{:doc "(blinn-phong light color [:s shininess] [:g glossiness])\n\nA Blinn-Phong shader, intended to be passed as an argument to `shade`. `:s` controls\nthe strength of specular highlights, and `:g` controls the glossiness.\n\n```example\n(ball 100 | shade :f blinn-phong [1 0 0] :s 1 :g (osc t 5 5 30))\n```"}
    blue @{:doc "  ```example\n  (set background-color blue)\n  (ball 100 | shade blue)\n  ```\n  "
           :value [hsv 0.66666666666666663 0.98 1]}
    bool @{}
    bound @{:doc "(bound shape bounding-shape threshold)\n\nWrap an expensive shape with a cheap bounding shape.\n\nThis operation evaluates the bounding shape, and if the distance to the bounding\nshape is less than `threshold`, it returns that distance. Otherwise it evaluates\nthe real shape.\n\nYou can use this to wrap a complicated, expensive shape in a cheaper bounding\nshape (spheres are best), so that you don't need to evaluate the expensive shape\nat every step of the raymarch. This is a very effective optimization if most\nrays don't need to enter the bounding shape, for example if you wrap a\nsmall shape in a large scene, but it doesn't really help.\n\nThis is hard to visualize because ideally it does not change the render,\nonly makes it faster, but you can see the effect it has on the raymarch\nby switching to debug convergence view (the magnet icon in the top right).\n\n```example\n(box 100 | bound (ball 180) 10)\n```\n\nIt's important that the bounding shape actually contain the inner shape, or\nyou'll get wild results that will hurt performance as rays fail to converge:\n\n```example\n(box 100 | bound (ball 100) 10)\n```\n\nThere's a tradeoff to make with the threshold between increased marching steps and\ntightening the bound, and a threshold too low may cause weird artifacts when rendering\nsoft shadows or ambient occlusion."}
    box @{:doc "(box size [:r round])\n\nReturns a 3D shape, a box with corners at `(- size)` and `size`. `size` will be coerced to a `vec3`.\n\nThink of `size` like the \"radius\" of the box: a box with `size.x = 50` will be `100` units wide.\n\n```example\n(box 100 :r (osc t 3 0 10))\n```\n\n```example\n(box [100 (osc t 3 50 100) (oss t 4 50 100)])\n```"}
    box-frame @{:doc "(box-frame size thickness [:r round])\n\nReturns a 3D shape, the outline of a box.\n\n```example\n(union\n  (box-frame 100 5 :r (osc t 3 5))\n  (box-frame [(osc t 4 30 100) (osc t 5 30 100) (oss t 6 30 100)] 1))\n```"}
    bump @{:doc "(bump shape by [amount])\n\nAlter the `normal` for a shape. You can use this along\nwith noise expressions to give the appearance of texture\nwithout the expense of evaluating the offset multiple\ntimes during the march.\n\nCompare, an actually-bumpy shape:\n\n```example\n(ball 100 | shade red | expand (perlin (p / 10)))\n```\n\nTo a shape with normals inspired by that bumpiness:\n\n```example\n(ball 100 | shade red | bump (perlin (p / 10)) 0.3)\n```\n\nThis is much cheaper than using `expand`, so if you're only trying to add\na little texture and don't need to change the shape, consider using `bump`\ninstead.\n\n(If you really do care about distorting the geometry, see `expound` for\na more efficient way to do that.)\n\nThe expression to `bump` will be evaluated using `calculate-normal`,\nso it should vary with `p`. This is a much cheaper way to add texture\nthan trying to sculpt it into the distance field. Orange peel:\n\n```example\n(ball 100 | shade (hsv 0.05 1 0.75) | bump (perlin+ p) 0.2)\n(set camera (camera/perspective [(cos (t / 2)) 0 (sin (t / 2)) * 200] | camera/zoom (osc t 7 1 2)))\n```"}
    calculate-gradient @{:doc "(calculate-gradient expr)\n\nEvaluates the given 2D distance expression four times, and returns an approximation\nof the expression's gradient."}
    calculate-normal @{:doc "(calculate-normal expr)\n\nEvaluates the given 3D distance expression four times, and returns an approximation\nof the expression's gradient."}
    calculate-occlusion @{}
    camera @{:doc "An expression for a `ray` that determines the position and direction of the camera."
             :ref @[nil]}
    camera/dolly @{:doc "(camera/dolly camera amount)\n\nMove the camera forward or backward.\n\n```example\n(morph (ball 50) (box 50) 2\n| union (circle 200 | extrude y 10 | move y -100)\n| shade (vec3 0.75))\n(set camera (camera/perspective [0 100 600] :fov 45\n| camera/dolly (sin t * 100)))\n```\n\nUseful for Hitchcocking:\n\n```example\n(morph (ball 50) (box 50) 2\n| union\n  (circle 200 | extrude y 10 | move y -100)\n  (box [100 200 50] | tile [300 0 300] :limit 4 | move [0 0 -1000])\n| shade (vec3 0.75))\n\n(set camera (camera/perspective [0 100 600] :fov 45\n| camera/dolly (sin+ t * -500)\n| camera/zoom (sin+ t + 1)\n))\n```"}
    camera/orthographic @{:doc "(camera/orthographic position [:target target] [:dir dir] [:roll roll] [:scale scale])\n\nReturns a camera with an orthographic projection and the given `:scale`\n(default 512). Other arguments are the same as `camera/perspective`.\n\n```example\n(morph (ball 50) (box 50) 2\n| union (circle 200 | extrude y 10 | move y -100)\n| shade (vec3 0.75))\n(def pos [(sin t * 200) (cos+ (t / 2) * 300) 500])\n(set camera (camera/orthographic pos))\n```\n\nAn orthographic camera shoots every ray in the same direction, but from a different\norigin. In effect it produces an image without any sense of depth, because objects\nfarther away from the camera don't get smaller. Compare the following scenes, with\na typical perspective camera:\n\n```example\n(box 50 | tile [150 0 150])\n(set camera (camera/perspective [1 1 1 | normalize * 512]))\n```\n\nAnd the same scene with an orthographic camera:\n\n```example\n(box 50 | tile [150 0 150])\n(set camera (camera/orthographic [1 1 1 | normalize * 512]))\n```"}
    camera/pan @{:doc "(camera/pan camera angle [:up up])\n\nRotate the camera left and right.\n\n```example\n(morph (ball 50) (box 50) 2\n| union (circle 200 | extrude y 10 | move y -100)\n| shade (vec3 0.75))\n(set camera (camera/perspective [0 100 600] :fov 45\n| camera/pan (sin t * 0.2)))\n```\n\nBy default this rotation is relative to the camera's current\norientation, so the image you see will always appear to be moving\nhorizontally during a pan. But you can provide an absolute\n`:up` vector to ignore the camera's roll. (I think the difference\nis easier to understand if you unroll the camera afterward.)\n\n  ```example\n(morph (ball 50) (box 50) 2\n| union (circle 200 | extrude y 10 | move y -100)\n| shade (vec3 0.75))\n(set camera (camera/perspective [0 100 600] :fov 45\n| camera/roll pi/4\n| camera/pan (sin t * 0.2)\n# | camera/roll -pi/4\n))\n```\n\n```example\n(morph (ball 50) (box 50) 2\n| union (circle 200 | extrude y 10 | move y -100)\n| shade (vec3 0.75))\n(set camera (camera/perspective [0 100 600] :fov 45\n| camera/roll pi/4\n| camera/pan (sin t * 0.2) :up y\n# | camera/roll -pi/4\n))\n```"}
    camera/perspective @{:doc "(camera/perspective position [:target target] [:dir dir] [:roll roll] [:fov fov])\n\nReturns a camera with a perspective projection located at `position` and\npointing towards the origin. You can have the camera face another point\nby passing `:target`, or set the orientation explicitly by passing a\nnormalized vector as `:dir` (you can't pass both).\n\nYou can change the field of view by passing `:fov` with a number of degrees. The default is `60`, and\nthe default orbiting free camera uses `45`.\n\n```example\n(morph (ball 50) (box 50) 2\n| union (circle 200 | extrude y 10 | move y -100)\n| shade (vec3 0.75))\n(def pos [(sin t * 200) (cos+ (t / 2) * 300) 500])\n(set camera (camera/perspective pos :fov 45))\n```"}
    camera/ray @{:doc "(camera/ray camera)\n\nReturns the perspective-adjusted ray from this camera for\nthe current `frag-coord`. You probably don't need to call\nthis."}
    camera/roll @{:doc "(camera/roll camera angle)\n\nRoll the camera around.\n\n```example\n(morph (ball 50) (box 50) 2\n| union (circle 200 | extrude y 10 | move y -100)\n| shade (vec3 0.75))\n(set camera (camera/perspective [0 100 600] :fov 45\n| camera/roll (sin t * 0.2)))\n```"}
    camera/tilt @{:doc "(camera/tilt camera angle [:up up])\n\nRotate the camera up and down.\n\n```example\n(morph (ball 50) (box 50) 2\n| union (circle 200 | extrude y 10 | move y -100)\n| shade (vec3 0.75))\n(set camera (camera/perspective [0 100 600] :fov 45\n| camera/tilt (sin t * 0.2)))\n```\n\nAs with `pan`, you can supply an absolute `:up` vector to use\ninstead of the camera's current roll.\n"}
    camera/zoom @{:doc "(camera/zoom camera amount)\n\nZoom the camera by changing its field of view (for a perspective camera)\nor scale (for an orthographic camera).\n\n```example\n(morph (ball 50) (box 50) 2\n| union (circle 200 | extrude y 10 | move y -100)\n| shade (vec3 0.75))\n(set camera (camera/perspective [0 100 600] :fov 45\n| camera/zoom (sin t * 0.2 + 1)))\n```"}
    camera? @{:doc "(camera? value)\n\nReturns `true` if `value` is a GLSL expression with type `PerspectiveCamera` or `OrthographicCamera`."}
    capsule @{:doc "(capsule axis length radius [top-radius])\n\nThere are two types of `capsule`s: symmetric capsules, which look\nlike pills, or axis-aligned lines:\n\n```example\n(capsule y 100 25)\n```\n\nAnd asymmetric capsules, which have a different radius at the\ntop and bottom:\n\n```example\n(capsule y 100 25 10)\n```"}
    capsule-2d @{:doc "(capsule-2d bottom-radius top-radius height)\n\n```example\n(capsule-2d 50 (osc t 3 20 60) (oss t 8 30 100))\n```"}
    cast-light-hard-shadow @{:doc "(cast-light-hard-shadow light-color light-position)\n\nTODOC"}
    cast-light-no-shadow @{:doc "(cast-light-no-shadow light-color light-position)\n\nTODOC"}
    cast-light-soft-shadow @{:doc "(cast-light-soft-shadow light-color light-position softness)\n\nTODOC"}
    ceil @{}
    circle @{:doc "(circle radius)\n\nReturns a 2D shape.\n\n```example\n(circle 100)\n```\n\nA circle is the most primitive shape, and it's very versatile. With no radius it's a\npoint in space, which can be useful for procedural patterns:\n\n```example\n(circle 0\n| move (hash2 $i * 50)\n| tile: $i [50 50] :oversample true :sample-from -1 :sample-to 1)\n```\n\n(See also `worley` for an optimized version of that voronoi pattern.)\n\nBy varying the radius dynamically, you can produce other interesting shapes:\n\n```example\n(circle (ss q.x -100 150 100 150)\n| color green)\n```\n\n```example\n(circle (sin+ (abs q.y - abs q.x / 10 + (t * 5)) * 20 + 100)\n| color sky)\n```\n\nAnd by projecting it into 3D, you can produce shapes like a disc:\n\n```example\n(circle 100 | extrude y | expand 25)\n```\n\nA torus:\n\n```example\n(circle 100 | shell | extrude y | expand 25)\n```\n\nA tube:\n\n```example\n(circle 100 | shell | extrude z 100 | expand 10)\n```\n\nAmong other shapes."}
    clamp @{}
    color @{:doc "(color shape color)\n\nSet a shape's color field. This is the primitive surfacing operation,\nboth in 2D:\n\n```example\n(circle 100 | color [1 0.5 0.5])\n```\n\nAnd in 3D:\n\n```example\n(box 100 :r 10 | color [1 0.5 0.5])\n```\n\nAlthough you will typically set a color field to a dynamic expression:\n\n```example\n(box 100 :r 10\n| color (hsv (atan2 p.xy / tau) 1 1\n  * dot normal [1 2 3 | normalize]))\n```\n\nYou can also pass another shape, in which case the color field will be copied to\nthe destination shape:\n\n```example\n(box 100 :r 10 | color\n  (union (box 100 | shade [0 1 0]) (ball 125 | shade [1 0 0])))\n```"}
    cone @{:doc "(cone axis radius height [:r round])\n\nReturns a 3D shape. The `height` is the extent in only a single direction.\n\n```example\n(cone y 50 (sin t * 150) :r (osc t 2 10))\n```\n\nIf you supply a rounding factor, the cone will be offset such that\nit always rests exactly on the zero plane normal to your axis. Is\nthat what you'd expect? I went back on forth on this. I think it's more\nintuitive but if you have thoughts I'd like to hear them."}
    cos @{}
    cos+ @{:doc "(cos+ x)\n\nLike `cos`, but returns a number in the range `0` to `1`."}
    cosh @{}
    cross @{}
    cross-matrix @{:doc "(cross-matrix vec)\n\nReturns the matrix such that `(* (cross-matrix vec1) vec2)` = `(cross vec1 vec2)`."}
    cube @{:doc "(cube size [:r round])\n\nThis is an alias for the `float` overload of `box`."}
    cut-disk @{:doc "(cut-disk radius bottom)\n\nReturns a 2D shape.\n\n```example\n(cut-disk 100 (sin t * 80))\n```"}
    cyan @{:doc "  ```example\n  (set background-color cyan)\n  (ball 100 | shade cyan)\n  ```\n  "
           :value [hsv 0.5 0.98 1]}
    cylinder @{:doc "(cylinder axis radius height [:r round])\n\nReturns a 3D shape, a cylinder oriented along the given `axis`.\n\n```example\n(cylinder y 50 100)\n```\n\nThe second argument is twice the length of the cylinder. Like many shapes,\nyou can round it with `:r`.\n\n```example\n(cylinder z 100 50 :r (osc t 3 0 20))\n```"}
    dark-gray @{:doc "  ```example\n  (set background-color dark-gray)\n  (ball 100 | shade dark-gray)\n  ```\n  "
                :value [0.1 0.1 0.1]}
    default-2d-color @{:doc "A variable that determines the default color to use when rendering a 2D shape with no color field.\n\nDefault is `isolines`."
                       :ref @[[isolines]]}
    default-3d-color @{:doc "A variable that determines the default color to use when rendering a 3D shape with no color field.\n\nDefault is `(mix normal+ [1 1 1] (fresnel 5))`."
                       :ref @[[mix
                               [* [+ normal 1] 0.5]
                               [vec3 1 1 1]
                               [fresnel 5]]]}
    defuniform @{:doc "(defuniform name initial-value)\n\nShort for `(def name (uniform initial-value \"name\"))`."
                 :macro true}
    degrees @{}
    depth @{:doc "The distance that the current ray has marched, equal to `(distance ray-origin P)`. Not defined in 2D."
            :value [:var "depth" :float]}
    dist @{:doc "(Color only!) The value of the global distance field at `P`. In 3D, this\nshould be a very small positive number, assuming the ray was able to\nconverge correctly. In 2D, this gives a more useful value."
           :value [:var "dist" :float]}
    distance @{}
    dot @{}
    double @{}
    ellipsoid @{:doc "(ellipsoid size)\n\nReturns a 3D shape. This is an alias for the `vec3` overload of `ball`."}
    elongate @{:doc "(elongate shape & args)\n\nStretch the center of a shape, leaving the sides untouched.\nThe arguments to `elongate` are similar to `move`: you\npass vectors or axis / magnitude pairs, and their sum\nwill be the total elongation.\n\n```example\n(cone y 50 100 | elongate [(osc t 3 50) 0 (osc t 6 100)])\n```\n\n```example\n(torus x 50 20 | elongate x (sin+ t * 50) [0 100 0])\n```\n\n```example\n(rhombus [100 (gl/if (< q.y 0) 100 50)] | elongate y (osc t 2 0 20))\n```"}
    equal @{}
    exp @{}
    exp2 @{}
    expand @{:doc "(expand shape by)\n\nExpands the provided shape, rounding corners in the process.\n\nThis is the same as subtracting `by` from the distance field. It's\nmore accurate to say that this \"moves between isosurfaces,\" so it\nmay not actually round anything if the provided shape is not an\nexact distance field.\n\nFor example, this produces a nicely expanded shape:\n\n```example\n(rect 90 | expand (sin+ t * 30))\n```\n\nBut this does something weird, because subtraction does not produce\nan exact distance field:\n\n```example\n(rect 90\n| subtract (rect 100 | move x 150)\n| expand (sin+ t * 30))\n```"}
    expound @{:doc "(expound shape by &opt magnitude threshold)\n\nThis is a combination of `bound` and `expand`, when you want to expand a shape by\nsome expensive expression (e.g. a noise function). Essentially it's the same as:\n\n```\n(bound\n  (expand shape (by * magnitude))\n  (expand shape magnitude)\n  threshold)\n```\n\nBut it produces slightly better code. Consider this:\n\n```example\n(ball 100\n| expand (perlin+ [(p / 50) t] * osc t 1 20 50))\n```\n\nThe rays around the edge of the canvas never approach the shape,\nbut they need to evaluate its distance expression repeatedly until\nthey give up. But 4D perlin noise is pretty expensive to compute,\nso we can speed up the render by only evaluating it when the\ncurrent ray is near the shape we're distorting:\n\n```example\n(ball 100\n| expound\n  (perlin+ [(p / 50) t])\n  (osc t 1 20 50))\n```\n\nIt's important that the signal you supply as `by` not exceed `1`,\nor the bounding shape will be inaccurate.\n\nBy default the `threshold` is equal to the `magnitude` of the\noffset, but you can provide a custom threshold to fine-tune the\nboundary behavior.\n\nIf you're only using procedural distortion to texture a shape, consider\nusing `bump` for an even larger speedup."}
    extrude @{:doc "(extrude shape axis &opt distance)\n\nExtrude a 2D shape into 3D along the given `axis`.\n\n`distance` defaults to `0` and determines the width, length, or height of the final shape.\nYou can also pass `inf` to get an infinite extrusion (which is slightly cheaper to compute)."}
    faceforward @{}
    fbm @{:doc "(fbm octaves noise point [period] [:f f] [:gain gain])\n\nRun the given noise function over the input multiple times,\nwith different periods, and sum the results with some decay.\n\nYou can use this to create interesting procedural patterns\nout of simple noise functions. For example, perlin noise is\na fairly smooth and \"low resoultion\" pattern:\n\n```example\n(color r2 (vec3 (perlin q 30 | remap+)))\n```\n\nBut by summing multiple instances of perlin noise\nsampled with different periods, we can create more detailed\npatterns:\n\n```example\n(color r2 (vec3 (fbm 4 perlin q 30 | remap+)))\n```\n\n```example\n(color r2 (vec3 (fbm 4 perlin q 30 :gain 0.8 | remap+)))\n```\n\nYou can use this to create many effects, like clouds or landscapes,\nalthough note that it is pretty expensive to use in a distance field.\nFor example, 2D perlin noise makes a pretty good landscape:\n\n```example\n(plane y\n| expound (fbm 7 perlin p.xz 100 | remap+) 50 10\n| intersect (ball [200 100 200])\n| slow 0.5\n| shade normal+ :g 20)\n```\n\n3D perlin noise is more expensive, but produces a detailed rocky effect:\n\n```example\n(plane y\n| expound (fbm 7 perlin p 50 | remap+) 50 10\n| intersect (ball [200 100 200])\n| slow 0.5\n| shade normal+ :g 20)\n```\n\nBy default the function will be evaluated with its input multiplied\nby two every time. You can change this by passing a different value\nas `:f`, or you can pass a function to provide a custom transformation:\n\n```example\n(color r2 (vec3 (fbm 4\n  (fn [q] (sin q.x + cos q.y /))\n  :f (fn [q] (rotate (q * 2) pi/4 (t / 20)))\n  q 20 | remap+)))\n```"}
    float @{}
    floor @{}
    fma @{}
    fract @{}
    frag-coord @{:doc "The logical position of the current fragment being rendered, in the approximate\nrange `-1` to `1`, with `[0 0]` as the center of the screen. Note though that\nwe always shade pixel centers, so we never actual render `-1` or `1`, just\nnearby subpixel approximations depending on the antialiasing level.\n\nThis is equal to `(Frag-Coord - (resolution * 0.5) / max resolution * 2)`."
                 :value [:var "frag-coord" :vec2]}
    fresnel @{:doc "(fresnel [exponent])\n\nReturns an approximate fresnel intensity. `exponent` defaults to `5`.\n\n```example\n(ball 100\n| shade [1 0.5 0.5]\n| tint [1 1 1] (fresnel (osc t 5 0.5 5)))\n```"}
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
    gl/if @{:doc "(gl/if condition then else)\n\nA GLSL ternary conditional expression.\n\n```example\n(ball 100 | color \n  (gl/if (< normal.y 0) \n    [1 0 0] \n    [1 1 0]))\n```"}
    gl/iife @{:doc "(gl/iife & body)\n\nLike `gl/do`, except that you can explicitly return early.\n\n```example\n(ball 100 | color\n  (gl/iife \"optional-label\"\n    (var c [1 0 1])\n    (if (< normal.y 0)\n      (return c))\n    (for (var i 0:u) (< i 10:u) (++ i)\n      (+= c.g (p.x / 100 / 10)))\n    c))\n```"
              :macro true}
    gl/let @{:doc "(gl/let bindings & body)\n\nLike `let`, but creates GLSL bindings instead of a Janet bindings. You can use this\nto reference an expression multiple times while only evaluating it once in the resulting\nshader.\n\nFor example:\n\n```\n(let [s (sin t)]\n  (+ s s))\n```\n\nProduces GLSL code like this:\n\n```\nsin(t) + sin(t)\n```\n\nBecause `s` refers to the GLSL *expression* `(sin t)`.\n\nMeanwhile:\n\n```\n(gl/let [s (sin t)]\n  (+ s s))\n```\n\nProduces GLSL code like this:\n\n```\nfloat let(float s) {\n  return s + s;\n}\n\nlet(sin(t))\n```\n\nOr something equivalent. Note that the variable is hoisted into an immediately-invoked function\nbecause it's the only way to introduce a new identifier in a GLSL expression context.\n\nYou can also use Bauble's underscore notation to fit this into a pipeline:\n\n```\n(s + s | gl/let [s (sin t)] _)\n```\n\nIf the body of the `gl/let` returns a shape, the bound variable will be available in all of its\nfields. If you want to refer to variables or expressions that are only available in some fields,\npass a keyword as the first argument:\n\n```example\n(gl/let :color [banding (dist * 10)]\n  (box 100 | shade [1 banding 0]))\n```"
             :macro true}
    gl/with @{:doc "(gl/with bindings & body)\n\nLike `gl/let`, but instead of creating a new binding, it alters the value of an existing\nvariable. You can use this to give new values to dynamic variables. For example:\n\n```example\n# implement your own (move)\n(gl/with [p (- p [0 (sin t * 50) 0])] (ball 100))\n```\n\nYou can also use Bauble's underscore notation to fit this into a pipeline:\n\n```example\n(ball 100 | gl/with [p (- p [0 (sin t * 50) 0])] _)\n```\n\nYou can -- if you really want -- use this to alter `P` or `Q` to not refer to the point in\nglobal space, or use it to pretend that the camera `ray` is actually coming at a different angle.\n\nThe variables you change in `gl/with` will, by default, apply to all of the fields of a shape.\nYou can pass a keyword as the first argument to only change a particular field. This allows you\nto refer to variables that only exist in color expressions:\n\n```example\n(gl/with :color [normal (normal + (perlin p * 0.1))]\n  (ball 100 | shade [1 0 0]))\n```"
              :macro true}
    gradient @{:doc "(Color only!) An approximation of the 2D distance field gradient at `Q`."
               :value [:var "gradient" :vec2]}
    gray @{:doc "  ```example\n  (set background-color gray)\n  (ball 100 | shade gray)\n  ```\n  "
           :value [0.5 0.5 0.5]}
    graydient @{:doc "The default background color, a gray gradient."
                :value [do]}
    green @{:doc "  ```example\n  (set background-color green)\n  (ball 100 | shade green)\n  ```\n  "
            :value [hsv 0.33333333333333331 0.98 1]}
    ground @{:doc "(ground [offset])\n\nReturns a 3D plane that only exists while the camera is above it.\n\nThis is useful for quickly debugging shadows while still being able\nto see the underside of your scene, although note that taking the plane\naway will affect ambient occlusion, so you're not *really* seeing the\nunderside."}
    hash @{:doc "(hash & args)\n\nReturn a pseudorandom float. The input can be a float or vector. With multiple arguments,\nthis will return the hash of the sum.\n\nThis should return consistent results across GPUs, unlike high-frequency sine functions."}
    hash2 @{:doc "(hash2 & args)\n\nReturn a pseudorandom `vec2`. The input can be a float or vector. With multiple arguments,\nthis will return the hash of the sum.\n\nThis should return consistent results across GPUs, unlike high-frequency sine functions."}
    hash3 @{:doc "(hash3 & args)\n\nReturn a pseudorandom `vec3`. The input can be a float or vector. With multiple arguments,\nthis will return the hash of the sum.\n\nThis should return consistent results across GPUs, unlike high-frequency sine functions."}
    hash4 @{:doc "(hash4 & args)\n\nReturn a pseudorandom `vec4`. The input can be a float or vector. With multiple arguments,\nthis will return the hash of the sum.\n\nThis should return consistent results across GPUs, unlike high-frequency sine functions."}
    hexagon @{:doc "(hexagon radius [:r round])\n\n```example\n(hexagon 100 :r (osc t 3 20))\n```"}
    hexagram @{:doc "(hexagram radius [:r round])\n\n```example\n(hexagram 100 :r (osc t 3 20))\n```"}
    hoist @{:doc "(hoist expr &opt name)\n\nReturn a hoisted version of the expression. See the documentation for `gl/def`\nfor an explanation of hoisting."}
    hot-pink @{:doc "  ```example\n  (set background-color hot-pink)\n  (ball 100 | shade hot-pink)\n  ```\n  "
               :value [hsv 0.91666666666666663 0.98 1]}
    hsl @{:doc "(hsl hue saturation lightness)\n\nReturns a color."}
    hsv @{:doc "(hsv hue saturation value)\n\nReturns a color."}
    in @{:doc "(in & args)\n\n"}
    inf @{:doc "The number representing positive infinity"
          :value 9e999}
    int @{}
    intersect @{:doc "(intersect & shapes [:r r] [:s s] [:distance distance] [:color color])\n\nIntersect two or more shapes. The named arguments produce a smooth intersection;\nsee `union` for a thorough description.\n\n```example\n(intersect\n  (ball 100 | move x -50 | shade red)\n  (ball 100 | move x +50 | shade sky))\n```\n\nNote that although it doesn't matter when doing a sharp intersection,\nyou probably want to use `:s` to smooth over `:r`, or else the latter\nshape's color field will \"take over\" the earlier shape. Compare:\n\n```example\n(intersect :r 30\n  (ball 100 | move x -50 | shade red)\n  (ball 100 | move x +50 | shade sky))\n```\n```example\n(intersect :s 30\n  (ball 100 | move x -50 | shade red)\n  (ball 100 | move x +50 | shade sky))\n```\n\nThis effect makes sense if you think about the shapes in 2D:\n\n```example\n(intersect :r 30\n  (circle 100 | move x -50 | color red)\n  (circle 100 | move x +50 | color sky))\n```\n\nThe second shape was on top of the first shape, so the first\nshape's color field is only visible where it fades into the\nshape of the first. But with a symmetric intersection:\n\n```example\n(intersect :s 30\n  (circle 100 | move x -50 | color red)\n  (circle 100 | move x +50 | color sky))\n```\n\nThis doesn't happen."}
    inverse @{}
    inversesqrt @{}
    isolines @{:doc "A color that represents the visualization of the 2D gradient. This is\nthe default color used when rendering a 2D shape with no color field."
               :value [isolines]}
    length @{}
    light-gray @{:doc "  ```example\n  (set background-color light-gray)\n  (ball 100 | shade light-gray)\n  ```\n  "
                 :value [0.75 0.75 0.75]}
    light/ambient @{:doc "(light/ambient color [offset] [:brightness brightness] [:hoist hoist])\n\nShorthand for `(light/point color (P + offset))`.\n\nWith no offset, the ambient light will be completely directionless, so it won't\ncontribute to specular highlights. By offsetting by a multiple of the surface\nnormal, or by the surface normal plus some constant, you can create an ambient\nlight with specular highlights, which provides some depth in areas of your scene\nthat are in full shadow."}
    light/directional @{:doc "(light/directional color dir dist [:shadow softness] [:brightness brightness] [:hoist hoist])\n\nA light that hits every point at the same angle.\n\nShorthand for `(light/point color (P - (dir * dist)))`."}
    light/map @{:doc "(light/map light f)\n\n`f` takes and returns a `Light` expression."}
    light/map-brightness @{:doc "(light/map-brightness light f)\n\n`f` takes and returns a `:float` expression."}
    light/map-color @{:doc "(light/map-color light f)\n\n`f` takes and returns a `vec3` expression."}
    light/point @{:doc "(light/point color position [:shadow softness] [:brightness brightness] [:hoist hoist])\n\nReturns a new light, which can be used as an input to some shading\nfunctions.\n\nAlthough this is called a point light, the location of the \"point\" can vary\nwith a dynamic expression. A light that casts no shadows and is located at\n`P` (no matter where `P` is) is an ambient light. A light that is always\nlocated at a fixed offset from `P` is a directional light.\n\nBy default lights don't cast shadows, but you can change that by passing a\n`:shadow` argument. `0` will cast hard shadows, and any other expression\nwill cast a soft shadow (it should be a number roughly in the range `0` to\n`1`).\n\nShadow casting affects the `brightness` of the light. You can also specify a\nbaseline `:brightness` explicitly, which defaults to `1`.\n\nShadow casting always occurs in the global coordinate space, so you should\nposition lights relative to `P`, not `p`.\n\nBy default light calculations are hoisted (see `gl/def` for more info). This\nis an optimization that's helpful if you have a light that casts shadows\nthat applies to multiple shaded surfaces that have been combined with a\nsmooth `union` or `morph` or other shape combinator. Instead of computing\nshadows twice and mixing them together, the shadow calculation will be\ncomputed once at the top level of the shader. Note though that this will\nprevent you from referring to variables that don't exist at the top\nlevel -- e.g. anything defined with `gl/let`, or the index argument of\n`tiled` shape. If you want to make a light that dynamically varies, pass\n`:hoist false`."}
    light? @{:doc "(light? value)\n\nReturns `true` if `value` is a GLSL expression with type `Light`."}
    lime @{:doc "  ```example\n  (set background-color lime)\n  (ball 100 | shade lime)\n  ```\n  "
           :value [hsv 0.25 0.98 1]}
    line @{:doc "(line from to from-radius [to-radius])\n\nReturns a line between two points.\n\n```example\n(line\n  [-100 (sin t * 100) (cos t * 100)]\n  [100 (cos t * 100) (sin t * 100)]\n  10\n| union (box-frame 100 1))\n```\n\nYou can supply two radii to taper the line over its length:\n\n```example\n(line\n  [-100 (sin t * 100) (cos t * 100)]\n  [100 (cos t * 100) (sin t * 100)]\n  (oss t 3 50) (osc t 5 50)\n| union (box-frame 100 1))\n```\n\nYou can also give 2D points for a line in 2D:\n\n```example\n(line\n  [-100 (cos t * 100)]\n  [100 (sin t * 100)]\n  (osc t 3 50) (osc t 5 50))\n```"}
    log @{}
    log2 @{}
    magenta @{:doc "  ```example\n  (set background-color magenta)\n  (ball 100 | shade magenta)\n  ```\n  "
              :value [hsv 0.83333333333333337 0.98 1]}
    map-color @{:doc "(map-color shape f)\n\nApply a function `f` to the shape's color field. `f` should take and return a\n`vec3` expression.\n\nThe returned shape has the same dimensions as the input.\n\nThis differs from `shape/map-color` in that the expression is wrapped in `gl/let`,\nso you can refer to it multiple times."}
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
    morph @{:doc "(morph shape1 amount shape2 [:distance amount] [:color amount])\n\nMorph linearly interpolates between two shapes.\n\n```example\n(morph (sin+ t)\n  (ball 100 | shade sky)\n  (box 100 | shade red))\n```\n\nConcretely this means that it returns a new shape whose individual fields\nare linear interpolations of the fields on the input shapes.\n\nWith an anonymous `amount` coefficient, both the distance and color fields\nwill be interpolated with the same value. But you can also specify per-field\noverrides:\n\n```example\n(morph (sin+ t) :color (cos+ t)\n  (ball 100 | shade sky)\n  (box 100 | shade red))\n```"}
    move @{:doc "(move subject & args)\n\nTranslate a shape. You can pass a vector offset:\n\n```example\n(move (box 50) [0 (sin t * 100) 0])\n```\n\nOr a vector and a scalar:\n\n```example\n(move (box 50) y (sin t * 100))\n```\n\nWhich is the same as `(move (box 50) (y * 100))`.\n\nIf you provide multiple vector-scalar pairs, their sum is the final offset:\n\n```example\n(move (box 50)\n  x (sin t * 100)\n  y (cos t * 100)\n  -z (sin t * 100))\n```\n\n`move` can take a shape, a vector, or a camera.\n\nIf you vary the amount of movement by the current position in space,\nyou can distort shapes in various ways:\n\n```example\n(box 100 | move x (sin (p.y / 100 * pi) * 30))\n```\n\n```example\n(cylinder y 100 10\n| move y (atan p.x p.z * 10 | sin * (length p | ss 10 100 0 10)))\n```\n\n```example\n(box [100 10 100] | move y (p.xz / 20 | pow 2 | sum) | slow 0.5)\n```"}
    nearest-distance @{:doc "(nearest-distance)\n\nThis is the forward declaration of the function that will become the eventual\ndistance field for the shape we're rendering. This is used in the main raymarcher,\nas well as the shadow calculations. You can refer to this function to sample the\ncurrent distance field at the current value of `p` or `q`, for example to create\na custom ambient occlusion value."}
    normal @{:doc "(Color only!) A normalized vector that approximates the 3D distance field\ngradient at `P` (in other words, the surface normal for shading)."
             :value [:var "normal" :vec3]}
    normal+ @{:doc "A color that represents the visualization of the 3D normal. This is\nthe default color used when rendering a 3D shape with no color field."
              :value [* [+ normal 1] 0.5]}
    normalize @{}
    not @{}
    not-equal @{}
    not= @{}
    occlusion @{:doc "(occlusion [:steps step-count] [:dist dist] [:dir dir] [:hoist hoist])\n\nApproximate ambient occlusion by sampling the distance field at `:steps` positions\n(default 8) linearly spaced from 0 to `:dist` (default `20`). The result will range\nfrom 1 (completely unoccluded) to 0 (fully occluded).\n\nBy default the occlusion samples will be taken along the surface normal of the point\nbeing shaded, but you can pass a custom `:dir` expression to change that. You can use\nthis to e.g. add jitter to the sample direction, which can help to improve the\nquality.\n\nOcclusion is somewhat expensive to calculate, so by default the result will\nbe hoisted, so that it's only calculated once per iteration (without you having to\nexplicitly `gl/def` the result). However, this means that the occlusion calculation\nwon't take into account local normal adjustments, so you might want to pass\n`:hoist false`."}
    octagon @{:doc "(octagon radius [:r round])\n\n```example\n(octagon 100 :r (osc t 3 20))\n```"}
    octahedron @{:doc "(octahedron radius [:r round])\n\nReturns a 3D shape.\n\n```example\n(octahedron 100 :r (sin+ t * 20) | rotate x t y t z t)\n```"}
    ok/hcl @{:doc "(ok/hcl hue chroma lightness)\n\nThis is a way to generate colors in the Oklab color space.\n\nOklab colors maintain \"perceptual brightness\" better than `hsv` or `hsl`:\n\n```example\n(union \n  (rect [200 50] | color (hsv    (q.x / 200 | remap+) 1 1) | move y 51)\n  (rect [200 50] | color (ok/hcl (q.x / 200 | remap+) 0.5 0.5) | move y -51))\n```\n\nNote that there is no yellow in that rainbow, because yellow is a bright color. If we increase\nthe lightness above 0.5, we notice that pure blue disappears:\n\n```example\n(union \n  (rect [200 50] | color (hsv    (q.x / 200 | remap+) 1 1) | move y 51)\n  (rect [200 50] | color (ok/hcl (q.x / 200 | remap+) 0.5 1) | move y -51))\n```\n\nBecause pure blue is a dark color.\n\n`chroma` is analogous to \"saturation,\" and should approximately range from 0 to 0.5.\n\n```example\n(union \n  (rect [200 50] | color (hsv    (q.x / 200 | remap+) (q.y / 50 | remap+) 1) | move y 101)\n  (rect [200 50] | color (ok/hcl (q.x / 200 | remap+) (q.y / 50 | remap+) 1) | move y 0)\n  (rect [200 50] | color (ok/hcl (q.x / 200 | remap+) (q.y / 50 | remap+) 0.5) | move y -101))\n```\n\n`lightness` should approximately range from 0 to 1, but is not properly defined at all hues or chromas.\nFor example, if we try to make a high-chroma yellow too dark, it slips into being green instead:\n\n```example\n(union \n  (rect [200 50] | color (hsv    (q.x / 200 | remap+) 1 (q.y / 50 | remap+)) | move y 151)\n  (rect [200 50] | color (ok/hcl (q.x / 200 | remap+) 0.25 (q.y / 50 | remap+)) | move y 50)\n  (rect [200 50] | color (ok/hcl (q.x / 200 | remap+) 0.5 (q.y / 50 | remap+)) | move y -51)\n  (rect [200 50] | color (ok/hcl (q.x / 200 | remap+) 0.75 (q.y / 50 | remap+)) | move y -151))\n```"}
    ok/mix @{:doc "(ok/mix from to by)\n\nLinearly interpolate between two RGB colors using the Oklab color space. This is the\nsame as converting them to the Oklab color space, mixing them, and then converting\nback to RGB, but it's more efficient.\n\n```example\n(union \n  (rect [200 50] | color (ok/mix red blue (q.x / 200 | remap+)) | move y 50)\n  (rect [200 50] | color (mix    red blue (q.x / 200 | remap+)) | move y -50))\n```"}
    ok/of-rgb @{:doc "(ok/of-rgb rgb)\n\nConvert an Oklab color to a linear RGB color. You can use this,\nalong with `ok/to-rgb`, to perform color blending in the Oklab\ncolor space.\n\nIn these examples, Oklab is on the left, and linear RGB mixing is\non the right:\n\n```example\n(union\n  (morph (osc t 10 | ss 0.01 0.99)\n    (ball 50 | shade yellow | map-color ok/of-rgb)\n    (box 50 | shade blue | map-color ok/of-rgb)\n  | map-color ok/to-rgb\n  | move [-60 0 60])\n  (morph (osc t 10 | ss 0.01 0.99)\n    (ball 50 | shade yellow)\n    (box 50 | shade blue)\n  | move [60 0 -60]))\n```\n\n```example\n(union\n(union :r (osc t 5 0 30)\n  (box 50 | shade red | map-color ok/of-rgb)\n  (ball 40 | shade blue | map-color ok/of-rgb | move y 50)\n| map-color ok/to-rgb\n| move [-60 0 60])\n(union :r (osc t 5 0 30)\n  (box 50 | shade red)\n  (ball 40 | shade blue | move y 50)\n| move [60 0 -60]))\n```"}
    ok/to-rgb @{:doc "(ok/to-rgb ok)\n\nConvert a linear RGB color to the Oklab color space. See `ok/of-rgb` for examples."}
    or @{}
    orange @{:doc "  ```example\n  (set background-color orange)\n  (ball 100 | shade orange)\n  ```\n  "
             :value [hsv 0.083333333333333329 0.98 1]}
    oriented-rect @{:doc "(oriented-rect start end width)\n\nTODOC"}
    osc @{:doc "(osc &opt period lo hi)\n\nReturns a number that oscillates with the given period. There are several overloads:\n\n```\n# 0 to 1 to 0 every second\n(osc t)\n\n# 0 to 1 to 0 every 10 seconds\n(osc t 10)\n\n# 0 to 100 to 0 every 10 seconds\n(osc t 10 100)\n\n# 50 to 100 to 50 every 10 seconds\n(osc t 10 50 100)\n```"}
    oss @{:doc "(oss &opt period lo hi)\n\nLike `osc`, but uses a sine wave instead of a cosine wave,\nso the output begins halfway between `lo` and `hi`."}
    outer-product @{}
    p @{:doc "The local point in 3D space. This is the position of the current ray, with\nany transformations applied to it."
        :value [:var "p" :vec3]}
    parallelogram @{:doc "(parallelogram size skew)\n\nReturns a 2D shape. `size.x` is the width of the top and bottom edges, and `size.y`\nis the height of the parellogram.\n\n```example\n(parallelogram [80 100] (sin t * 100))\n```\n\n`skew` is how far the pallorelogram leans in the `x` direction, so the total\nwidth of the prellogram is `(size.x + skew) * 2`. A `skew` of `0` gives the\nsame shape as `rect`."}
    pentagon @{:doc "(pentagon radius [:r round])\n\n```example\n(pentagon 100 :r (osc t 3 20))\n```"}
    perlin @{:doc "(perlin point &opt period)\n\nReturns perlin noise ranging from `-1` to `1`. The input `point` can be a vector of any dimension.\n\nUse `perlin+` to return noise in the range `0` to `1`."}
    perlin+ @{:doc "(perlin+ point &opt period)\n\nPerlin noise in the range `0` to `1`.\n\n```example\n(ball 100 | color (perlin+ (p.xy / 10) | vec3))\n```\n```example\n(ball 100 | color (perlin+ (p / 10) | vec3))\n```\n```example\n(ball 100 | color (perlin+ [(p / 10) t] | vec3))\n```"}
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
    purple @{:doc "  ```example\n  (set background-color purple)\n  (ball 100 | shade purple)\n  ```\n  "
             :value [hsv 0.75 0.98 1]}
    q @{:doc "The local point in 2D space. This is the position being shaded, with any\ntransformations applied."
        :value [:var "q" :vec2]}
    quad-circle @{:doc "(quad-circle radius)\n\nReturns a 2D shape, an approximation of a circle made out of quadratic bezier curves.\n\n```example\n(quad-circle 100)\n```\n\nIt's like a circle, but quaddier."}
    quantize @{:doc "(quantize value count)\n\nRounds a value to the nearest multiple of `count`."}
    r2 @{:doc "A 2D shape with zero distance everywhere."
         :value {:fields {:distance [<1>
                                     literal
                                     [<2> primitive [<3> float]]
                                     0]}
                 :tag <4>
                 :type [<2> vec [<3> float] 2]}}
    r3 @{:doc "A 2D shape with zero distance everywhere."
         :value {:fields {:distance [<1>
                                     literal
                                     [<2> primitive [<3> float]]
                                     0]}
                 :tag <4>
                 :type [<2> vec [<3> float] 3]}}
    radial @{:doc "(radial shape [axis] count [offset] [:oversample oversample] [:sample-from sample-from] [:sample-to sample-to])\n\nRepeat an angular slice of space `count` times around the given axis.\n\n```example\n(torus x 100 1 | radial y 24)\n```\n\nWith an offset argument, you can translate the shape away from the origin first:\n\n```example\n(torus x 100 1 | radial y 24 (osc t 5 0 120))\n```\n\nIf you're repeating a shape that is not symmetric, you can use `:oversample true` to evaluate\nmultiple instances at each pass, essentially considering the distance not only to this\nslice, but also to neighboring slices. Compare these two distance fields:\n\n```example\n(triangle [50 100] | radial 12 100)\n```\n```example\n(triangle [50 100] | radial 12 100 :oversample true)\n```\n\nThe default oversampling is `:sample-from 0` `:sample-to 1`, which means looking at one adjacent\nslice, asymmetrically based on the location of the point (so when evaluating a point near\nthe right edge of a slice, it will look at the slice to the right, but not the slice\nto the left). By passing `:sample-from -1`, you can also look at the \"far\" slice.\nBy passing `:sample-from 0 :sample-to 2`, you can look at two slices in the direction of\nthe nearest slice.\n\nThis can be useful when raymarching a 3D space where each slice produces a different shape, or\nwhere the shape you're marching doesn't fit into a single slice. For example:\n\n```example\n(cone y 25 100 :r 1\n| radial z 12 100 :oversample true :sample-from -1)\n```"}
    radial* @{:doc "(radial* [axis] count [offset] get-shape [:oversample oversample] [:sample-from sample-from] [:sample-to sample-to])\n\nLike `radial`, but the shape is a result of invoking `get-shape` with one argument,\na GLSL variable referring to the current slice of space.\n\n```example\n(radial* z 12 100 (fn [$i]\n  (ball 50\n  | color (hsv (hash $i) 0.5 1))))\n```\n\nYou can use this to generate different shapes or colors at every sampled slice of space.\nThe index will be a `float` with integral components that represents the current slice\nbeing considered.\n\nSee also `radial:`, which is a more convenient macro version of this function."}
    radial: @{:doc "(radial: shape $i & args)\n\nLike `radial*`, but its first argument should be a form that will\nbecome the body of the function. Basically, it's a way to create\na repeated shape where each instance of the shape varies, and it's\nwritten in a way that makes it conveniently fit into a pipeline:\n\n```example\n(ball 50\n| color (hsv (hash $i) 0.5 1)\n| radial: $i z 12 100)\n```"
              :macro true}
    radians @{}
    ray @{:doc "The current ray being used to march and shade the current fragment. This always represents\nthe ray from the camera, even when raymarching for shadow casting.\n\nA ray has two components: an `origin` and a `dir`ection. `origin` is a point in the \nglobal coordinate space, and you can intuitively think of it as \"the location of the camera\"\nwhen you're using the default perspective camera (orthographic cameras shoot rays from different\norigins).\n\nThe direction is always normalized."
          :value [:var "ray" Ray]}
    ray? @{:doc "(ray? value)\n\nReturns `true` if `value` is a GLSL expression with type `Ray`."}
    rect @{:doc "(rect size [:r radius])\n\nReturns a 2D shape, a rectangle with corners at `(- size)` and `size`. `size` will be coerced to a `vec2`.\n\nThink of `size` like the \"radius\" of the rect: a rect with `size.x = 50` will be `100` units wide.\n\n`radii` can be a single radius or a `vec4` of `[top-left` `top-right` `bottom-right` `bottom-left]`.\n\n```example\n(union\n  (rect 50 | move [-100 100])\n  (rect 50 :r 10 | move [100 100])\n  (rect 50 :r [0 10 20 30] | move [-100 -100])\n  (rect 50 :r [0 30 0 30] | move [100 -100]))\n```"}
    red @{:doc "  ```example\n  (set background-color red)\n  (ball 100 | shade red)\n  ```\n  "
          :value [hsv 0 0.98 1]}
    reflect @{}
    refract @{}
    remap+ @{:doc "(remap+ x)\n\nLinearly transform a number in the range `[-1 1]` to `[0 1]`."}
    remap- @{:doc "(remap- x)\n\nLinearly transform a number in the range `[0 1]` to `[-1 1]`. The inverse of `remap+`."}
    resolution @{:doc "The size, in physical pixels, of the canvas being rendered. In quad view, this\nwill be smaller than the physical size of the canvas."
                 :value [:var "resolution" :vec2]}
    revolve @{:doc "(revolve shape axis &opt offset)\n\nRevolve a 2D shape around the given `axis` to return a 3D shape.\n\n```example\n(revolve (triangle 100) y)\n```\n\nThis lets you create shapes that look like they were turned on a lathe:\n\n```example\n(union :r 10\n  (circle 40 | move y 80)\n  (rect [(ss q.y -90 -52 60 10) 100])\n  (rect [(ss q.y -70 57 58 20 * ss q.y -80 -5) 80] | move y -38)\n  (rect :r 5 [20 5] | move y 40 | rotate -0.48)\n  #| view\n  | revolve y)\n```\n\nYou can optionally supply an `offset` to move the shape away from the\norigin first (the default is `0`).\n\n```example\n(revolve (triangle 50) y 50)\n```\n\nYou can use this to create different types of toroidal shapes:\n\n```example\n(revolve (star 30 50 | rotate t) y 100)\n```"}
    rhombus @{:doc "(rhombus size [:r round])\n\nReturns a 2D shape. It rhombs with a kite.\n\n```example\n(rhombus [100 (osc t 3 50 150)])\n```"}
    ring @{:doc "(ring radius angle thickness)\n\n```example\n(ring 100 (osc t 5 tau) (osc t 3 5 20))\n```"}
    rotate @{:doc "(rotate subject & args)\n\nRotate a shape or a vector. Positive angles are counter-clockwise rotations.\n\nIn 3D, the arguments should be pairs of `axis angle`. For example:\n\n```example\n(rotate (box 100) x t y (sin t))\n```\n\nAll `axis` arguments must be unit vectors. There are built-in axis variables `x`/`+y`/`-z`\nfor the cardinal directions, and these produce optimized rotation matrices. But you can\nrotate around an arbitrary axis:\n\n```example\n(rotate (box 100) [1 1 1 | normalize] t)\n```\n\nThe order of the arguments is significant, as rotations are not commutative.\n\nThe first argument to `rotate` can be a shape, vector, or camera.\n\nIn 2D, the arguments should just be angles; no axis is allowed.\n\nYou can use `rotate` to make lots of cool effects. By varying the angle\nof rotation, you can create twists:\n\n```example\n(box [50 100 50]\n| rotate y (p.y / 100 * (cos+ t)))\n```\n\nTwirls:\n\n```example\n(box [100 50 100]\n| rotate y (length p.xz / 50 * (cos+ t)))\n```\n\nAnd bends:\n\n```example\n(box [50 100 100]\n| rotate y (p.z / 100 * (cos+ t)))\n```\n\nOr any number of other cool effects!\n\n```example\n(box [50 100 50]\n| rotate y (sin (p.y / 10) * sin t * 0.2))\n```"}
    rotation-around @{:doc "(rotation-around axis angle)\n\nA rotation matrix about an arbitrary axis. More expensive to compute than the axis-aligned rotation matrices."}
    rotation-matrix @{:doc "(rotation-matrix & args)\n\nReturn a rotation matrix. Takes the same arguments as `rotate`, minus the initial thing to rotate."}
    rotation-x @{:doc "(rotation-x angle)\n\nA rotation matrix about the X axis."}
    rotation-y @{:doc "(rotation-y angle)\n\nA rotation matrix about the Y axis."}
    rotation-z @{:doc "(rotation-z angle)\n\nA rotation matrix about the Z axis."}
    round @{}
    round-even @{}
    scale @{:doc "(scale shape & args)\n\nScale a shape. If the scale factor is a float, this will produce an exact\ndistance field.\n\n```example\n(rect 50 | scale 2)\n```\n\nIf the scale factor is a vector, space will be distorted by the smallest\ncomponent of that vector, and produce an approximate distance field:\n\n```example\n(rect 50 | scale [2 1])\n```\n\nWith an even number of arguments, `scale` expects `axis amount` pairs.\nUnlike `rotate`, it won't work with arbitrary axes -- you must give it\na cardinal axis.\n\n```example\n(rect 50 | scale x 0.5 y 2)\n```"}
    shade @{:doc "(shade shape & args [:f f] :& kargs)\n\n`shade` colors a shape with a map-reduce over the current lights.\nIt's a higher-order operation that takes a shading function (by\ndefault `blinn-phong`) -- and calls it once for every light in `*lights*`.\n\n```example\n(ball 100 | shade [1 0.25 0.5])\n```\n\nAll arguments to `shade` will be passed to the shading function,\nand only evaluated once, no matter how many lights there are. See the\ndocumentation for `blinn-phong` for a description of the arguments\nit takes.\n\nIf you define a custom shading function, its first argument should be a light:\n\n```example\n(ball 100 | shade [1 0.25 0.5] :f (fn [light color]\n  (dot normal light.direction * light.brightness\n  | quantize 5 * light.color * color\n)))\n```\n\nA light is a struct with three fields: `color`, `direction`,\nand `brightness`. `brightness` is roughly \"shadow intensity,\"\nand in the default lights it includes an ambient occlusion\ncomponent. This shader hardens the edges of shadows with step,\nand uses it to apply a custom shadow color:\n\n```example\n(torus x 10 5 | rotate z t | radial y 20 100 | union (ground -120)\n| shade [0.5 1.0 1.0] :f (fn [light color]\n  (dot normal light.direction * light.color * (mix [0.5 0 0] color (step 0.5 light.brightness))\n  )))\n```\n\nWhy does a light give you a `direction` instead of a `position`? `direction` is a little\nmore robust because you don't have to remember to special-case the zero vector (ambient\nlights), and theoretically if you want to do anything position-dependent you should reflect\nthat in the `color` or `brightness`. But maybe it should give you both. I'm torn now."}
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
    simplex @{:doc "(simplex point &opt period)\n\nReturns simplex noise ranging from `-1` to `1`. The input `point` can be a vector of any dimension.\n\nUse `simplex+` to return noise in the range `0` to `1`."}
    simplex+ @{:doc "(simplex+ point &opt period)\n\nsimplex noise in the range `0` to `1`.\n\n```example\n(ball 100 | color (simplex+ (p.xy / 10) | vec3))\n```\n```example\n(ball 100 | color (simplex+ (p / 10) | vec3))\n```\n```example\n(ball 100 | color (simplex+ [(p / 10) t] | vec3))\n```"}
    sin @{}
    sin+ @{:doc "(sin+ x)\n\nLike `sin`, but returns a number in the range `0` to `1`."}
    sinh @{}
    sky @{:doc "  ```example\n  (set background-color sky)\n  (ball 100 | shade sky)\n  ```\n  "
          :value [hsv 0.58333333333333337 0.98 1]}
    slice @{:doc "(slice shape axis &opt position)\n\nTake a 2D slice of a 3D shape at a given `position` along the supplied `axis`.\n\n`position` defaults to `0`."}
    sliced @{:doc "(sliced shape axis &opt position)\n\nTake a 2D slice of a 3D shape at a given `position` along the supplied `axis`,\nand then project it back into 3D space at the same spot.\n\nThis is useful for quickly looking inside shapes:\n\n```example\n(union\n  (box 80 | shade red)\n  (ball 100 | shade green)\n# try commenting out this line:\n| sliced y (sin t * 100)\n)\n```"}
    slow @{:doc "(slow shape amount)\n\nScales the shape's distance field, causing the raymarcher to converge more slowly.\nThis is useful for raymarching distance fields that vary based on `p` -- shapes\nthat don't actually provide an accurate distance field unless you are very close\nto their surfaces. Compare the following examples, with and without `slow`:\n\n```example\n(box 100\n| rotate y (p.y / 30)\n| rotate x t)\n```\n\n```example\n(box 100\n| rotate y (p.y / 30)\n| rotate x t\n| slow 0.5)\n```\n\nNote however that `slow` will also affect the behavior of anything that depends on a shape's\ndistance field, such as smooth boolean operations, morphs, soft shadows, and so on. A future\nversion of Bauble may mitigate these effects, but it is the way that it is right now.\n\n```example\n# slowing the distance field introduces asymmetry\n# into the smooth union\n(union :r 50\n  (ball 100 | move x 75)\n  (ball 100 | move x -75 | slow (osc t 4 0.25 1)))\n```"}
    smoothstep @{}
    sphere @{:doc "(sphere radius)\n\nReturns a 3D shape. This is an alias for the float overload of `ball`."}
    sqrt @{}
    ss @{:doc "(ss x [from-start] [from-end] [to-start] [to-end])\n\nThis is a wrapper around `smoothstep` with a different argument order, which also\nallows the input edges to occur in descending order. It smoothly interpolates\nfrom some input range into some output range.\n\n```example\n(box [100 (ss p.z 100 -100 0 100) 100])\n```\n\nThere are several overloads. You can pass one argument:\n\n```example\n# (ss x) = (smoothstep 0 1 x)\n(union\n  (rect 50 | move y (sin t * 100) x -100)\n  (rect 50 | move y (ss (sin t) * 100) x 100))\n```\n\nThree arguments (which is basically just `smoothstep`, except that you can reverse\nthe edge order):\n\n```example\n# (ss x from-start from-end) =\n#   (if (< from-start from-end)\n#     (smoothstep from-start from-end x)\n#     (1 - smoothstep from-end from-start x))\n(union\n  (rect 50 | move y (sin t * 100) x -100)\n  (rect 50 | move y (ss (sin t) 1 0.5 * 100) x 100))\n```\n\nOr five arguments:\n\n```example\n# (ss x from [to-start to-end]) =\n#   (ss x from * (- to-end to-start) + to-start)\n(union\n  (rect 50 | move y (sin t * 100) x -100)\n  (rect 50 | move y (ss (sin t) 0.9 1 -100 100) x 100))\n```"}
    star @{:doc "(star outer-radius inner-radius [:r round])\n\n```example\n(star 100 70 :r (osc t 3 20))\n```"}
    step @{}
    subject @{:doc "A variable that determines what Bauble will render.\n\nYou can set this variable explicitly to change your focus, or use\nthe `view` macro to change your focus. If you don't set a subject,\nBauble will render the last shape in your script."
              :ref @[nil]}
    subtract @{:doc "(subtract & shapes [:r r] [:s s] [:distance distance] [:color color])\n\nSubtract one or more shapes from a source shape. The named arguments\nhere produce a smooth subtraction, and are similar to the arguments to `union`.\n\n```example\n(subtract\n  (ball 100 | move x -50 | shade red)\n  (ball 100 | move x +50 | shade sky))\n```\n\nLike `union` and `intersect`, you can perform a smooth subtraction with `:r` or `:s`:\n\n```example\n(subtract :r 20\n  (ball 100 | move x -50 | shade red)\n  (ball 100 | move x +50 | shade sky))\n```\n\n```example\n(subtract :s 20\n  (ball 100 | move x -50 | shade red)\n  (ball 100 | move x +50 | shade sky))\n```\n\nSee the docs for `union` and `intersect` for a full explanation of these arguments\nand the difference between them."}
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
    teal @{:doc "  ```example\n  (set background-color teal)\n  (ball 100 | shade teal)\n  ```\n  "
           :value [hsv 0.41666666666666669 0.98 1]}
    tile @{:doc "(tile shape size [:limit limit] [:oversample oversample] [:sample-from sample-from] [:sample-to sample-to])\n\nRepeat the region of space `size` units around the origin. Pass `:limit` to constrain\nthe number of repetitions. See `tile:` or `tile*` if you want to produce a shape that\nvaries as it repeats.\n\nTo repeat space only along some axes, pass `0`. For example, to only tile in the `y` direction:\n\n```example\n(tile (ball 50) [0 100 0])\n```\n\nIf you're repeating a shape that is not symmetric, you can use `:oversample true` to evaluate\nmultiple instances at each pass, essentially considering the distance not only to this\ntile, but also to neighboring tiles. Compare these two distance fields:\n\n```example\n(rect 30 | rotate 0.3 | tile [80 80] :oversample false)\n```\n```example\n(rect 30 | rotate 0.3 | tile [80 80] :oversample true)\n```\n\nThe default oversampling is `:sample-from 0` `:sample-to 1`, which means looking at one adjacent\ntile, asymmetrically based on the location of the point (so when evaluating a point near\nthe right edge of a tile, it will look at the adjacent tile to the right, but not the tile\nto the left). By passing `:sample-from -1`, you can also look at the tile to the left.\nBy passing `:sample-from 0 :sample-to [2 1 1]`, it will look at two tiles to the right in the\n`x` direction, and one tile up/down/in/out.\n\nThis can be useful when raymarching a 3D space where each tile is quite different, but note\nthat it's very costly to increase these values. If you're tiling a 3D shape in all directions,\nthe default `:oversample` parameters will do 8 distance field evaluations;\n`:sample-from -1` `:sample-to 1` will do 27."}
    tile* @{:doc "(tile* size get-shape [:limit limit] [:oversample oversample] [:sample-from sample-from] [:sample-to sample-to])\n\nLike `tile`, but the shape is a result of invoking `get-shape` with one argument,\na GLSL variable referring to the current index in space. Unlike `tile`, `size` must\nbe a vector that determines the dimension of the index variable.\n\n```example\n(tile* [10 10] (fn [$i] \n  (circle 5 \n  | color (hsv (hash $i) 0.5 1))))\n```\n\nYou can use this to generate different shapes or colors at every sampled tile. The\nindex will be a vector with integral components that represents the current tile\nbeing evaluated. So in 3D, the shape at the origin has an index of `[0 0 0]` and\nthe shape above it has an index of `[0 1 0]`.\n\nSee also `tile:`, which is a more convenient macro version of this function."}
    tile: @{:doc "(tile: shape $i & args)\n\nLike `tile*`, but its first argument should be a form that will\nbecome the body of the function. Basically, it's a way to create\na repeated shape where each instance of the shape varies, and it's\nwritten in a way that makes it conveniently fit into a pipeline:\n\n```example\n(circle 5 \n| color (hsv (hash $i) 0.5 1) \n| tile: $i [10 10])\n```"
            :macro true}
    tint @{:doc "(tint shape color &opt amount)\n\nAdd a color to a shape's color field.\n\n```example\n(ball 100 | shade normal+ | tint [1 0 0] (sin+ t))\n```"}
    torus @{:doc "(torus axis radius thickness)\n\nReturns a 3D shape, a torus around the provided `axis`.\n\n```example\n(torus z 100 (osc t 3 10 50))\n```"}
    transparent @{:doc "\nThis is a `vec4`, not a `vec3`, so you\ncan basically only use it as a background color.\n\n```example\n(set background-color transparent)\n```"
                  :value [vec4 0]}
    transpose @{}
    trapezoid @{:doc "(trapezoid bottom-width top-width height [:r round])\n\nReturns a 2D shape.\n\n```example\n(trapezoid (osc t 3 50 100) (oss t 2 100 50) 100)\n```"}
    triangle @{:doc "(triangle & args)\n\nUsually returns a 2D shape, with various overloads:\n\n```example\n(triangle 100)\n```\n\n```example\n(triangle [50 100])\n```\n\n```example\n(triangle [-50 100] [100 10] [-10 -100])\n```\n\nBut it can also return a 3D shape:\n\n```example\n(triangle\n  [(osc t 4 -100 100) -100 (oss t 5 -100 100)]\n  [100 (osc t 6 -100 100) 100]\n  [-100 (oss t 7 -100 100) (osc t 8 -100 100)]\n| union (box-frame 100 1))\n```"}
    trunc @{}
    uint @{}
    uniform @{:doc "(uniform initial-value &opt name)\n\nCreate a uniform with an initial value.\n\nA uniform is like an input to a shader, and you can use uniforms to\ncreate dynamic shaders that you can control from outside of Bauble.\nIf you use Bauble's \"Export to HTML Embed\" function, you can put\nyour Baubles on a web page you control, and then set its uniforms\nfrom JavaScript based on whatever inputs you want.\n\nThese correspond to literal GLSL uniforms, so you don't have to use\nBauble's JavaScript player at all -- you can export a GLSL shader with\ncustom uniforms and set them by hand, if you want to.\n\nYou probably want to give your uniforms names, so that you can set them, and\n`(defuniform)` is a convenient wrapper for doing this. But you can also create\nanonymous uniforms. It's kind of a weird thing to do, but you can edit the\ninitial value of an anonymous uniform without needing to recompile the\nshader every time it changes, which can be helpful for refining values in\ncomplex scenes that take a long time to compile. One day Bauble might\nautomatically create anonymous uniforms whenever you use mouse editing,\nbut it can't do that yet. Also sometimes it recompiles the shader anyway because\nthe shader output is not fully deterministic; my bad; one day this will work right."}
    union @{:doc "(union & shapes [:r r] [:s s] [:distance distance] [:color color])\n\nUnion two or more shapes together. Pass `:r` or `:s` to produce a smooth union.\n\n```example\n(union\n  (ball 100 | shade red | move x -50)\n  (ball 100 | shade sky | move x 50))\n```\n\nThere are two ways that `union` (and other boolean operations) can combine color fields.\nThe default is to put later shapes \"on top of\" earlier shapes:\n\n```example\n(union\n  (circle 100 | move x -50 | color red)\n  (circle 100 | move x +50 | color sky))\n```\n\nAnd you can perform a smoothed version of this operation with `:r`:\n\n```example\n(union :r 20\n  (circle 100 | move x -50 | color red)\n  (circle 100 | move x +50 | color sky))\n```\n\nThe other way to combine color fields is to simply pick the nearest\ncolor. This produces a symmetric color field where the order of arguments\ndoesn't matter:\n\n```example\n(union :s 20\n  (circle 100 | move x -50 | color red)\n  (circle 100 | move x +50 | color sky))\n```\n\n(You can pass `:s 0` if you want a sharp symmetric color union.)\n\nIn 3D, the difference is harder to see, because they both produce\nthe same color field at the shape's surface:\n\n```example\n(union\n  (union :r 20\n    (ball 100 | move x -50 | shade red)\n    (ball 100 | move x +50 | shade sky)\n  | move y 100)\n  (union :s 20\n    (ball 100 | move x -50 | shade red)\n    (ball 100 | move x +50 | shade sky)\n  | move y -100))\n```\n\nBut just as in 2D, they produce different colors inside the shapes:\n\n```example\n(union\n  (union :r 20\n    (ball 100 | move x -50 | shade red)\n    (ball 100 | move x +50 | shade sky)\n  | move y 100)\n  (union :s 20\n    (ball 100 | move x -50 | shade red)\n    (ball 100 | move x +50 | shade sky)\n  | move y -100)\n| sliced z (sin t * 50))\n```\n\nThis is more relevant when using `subtract` or `intersect`, which will\ntypically prefer the `:s` behavior.\n\nYou can also pass `:distance` or `:color` to specify a different smoothing radius for\nthe separate fields. For example, you can produce a smooth symmetric color union with a sharp\ndistance field:\n\n```example\n(union :s 30 :distance 0\n  (ball 100 | move x -50 | shade red)\n  (ball 100 | move x +50 | shade sky))\n```\n\nOr a smooth distance field with a sharp transition in color:\n\n```example\n(union :r 30 :color 0\n  (ball 100 | move x -50 | shade red)\n  (ball 100 | move x +50 | shade sky))\n```\n\nOr any combination like that."}
    union-color @{:doc "(union-color & shapes [:r r] [:s s])\n\n`union-color` is like `union`, but it only affects color fields: it returns a shape with the same\ndistance field as its first argument.\n\nYou can use it to \"paint\" or \"stamp\" shapes, in 2D or 3D. For example:\n\n```example\n(star 100 50 | color sky | union-color (circle 60 | color orange))\n```\n\n```example\n(ball 100\n| shade sky\n# change this to a union\n| union-color (star 50 30 | color red | extrude z inf | radial y 5 | rotate y t))\n```"}
    vec @{}
    vec2 @{}
    vec3 @{}
    vec4 @{}
    view @{:doc "(view subject)\n\nA shorthand for `(set subject _)` that fits nicely into pipe notation, e.g. `(ball 50 | view)`."
           :macro true}
    viewport @{:doc "You don't have to think about this value unless you're implementing a custom `main` function,\nwhich you probably aren't doing.\n\nThis represents the portion of the canvas currently being rendered. The `xy` components are the start\n(bottom left) and the `zw` coordinates are the size.\n\nNormally this will be equal to `[[0 0] resolution]`, but when rendering quad-view or a chunked render,\nit may have a different origin or resolution.\n\nYou can use `(gl-frag-coord.xy - viewport.xy)` in order to get the logical fragment position (the value\nexposed to a typical shader as `Frag-Coord`)."
               :value [:var "viewport" :vec4]}
    white @{:doc "  ```example\n  (set background-color white)\n  (ball 100 | shade white)\n  ```\n  "
            :value [1 1 1]}
    with-lights @{:doc "(with-lights shape & lights)\n\nEvaluate `shape` with the `*lights*` dynamic variable set to the provided lights.\n\nThe argument order makes it easy to stick this in a pipeline. For example:\n\n```example\n(ball 100\n| shade [1 0 0]\n| with-lights\n  (light/point 0.5 [100 100 0])\n  (light/ambient 0.5))\n```"
                  :macro true}
    worley @{:doc "(worley point &opt period)\n\nWorley noise, also called cellular noise or voronoi noise.\nThe input `point` can be a `vec2` or a `vec3`.\n\n```example\n(ball 100 | color (worley (p.xy / 30) | vec3))\n```\n```example\n(ball 100 | color (worley (p / 30) | vec3))\n```\n\nReturns the nearest distance to points distributed randomly within the tiles of a square or cubic grid."}
    worley2 @{:doc "(worley2 point &opt period)\n\nLike `worley`, but returns the nearest distance in `x` and the second-nearest distance in `y`.\n\n```example\n(ball 100 | color [(worley2 (p.xy / 30)) 1])\n```\n```example\n(ball 100 | color [(worley2 (p / 30)) 1])\n```"}
    x @{:doc "`[1 0 0]`" :value [1 0 0]}
    xor @{}
    y @{:doc "`[0 1 0]`" :value [0 1 0]}
    yellow @{:doc "  ```example\n  (set background-color yellow)\n  (ball 100 | shade yellow)\n  ```\n  "
             :value [hsv 0.16666666666666666 0.98 1]}
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
(test (jlsl/show (ss 10 1 2))
  [smoothstep 1 2 10])
(test (jlsl/show (ss 10 2 1))
  [- 1 [smoothstep 1 2 10]])
(test (jlsl/show (ss 10 2 1 100 120))
  [+ [* [- 1 [smoothstep 1 2 10]] 20] 100])

