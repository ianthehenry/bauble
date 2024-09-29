(use ./import)
(import ../expression-hoister)

(def- VERY_LARGE_DISTANCE 1e6)

(defn color
  ````
  Set a shape's color field. This is the primitive surfacing operation,
  both in 2D:

  ```example
  (circle 100 | color [1 0.5 0.5])
  ```

  And in 3D:

  ```example
  (box 100 :r 10 | color [1 0.5 0.5])
  ```

  Although you will typically set a color field to a dynamic expression:

  ```example
  (box 100 :r 10
  | color (hsv (atan2 p.xy / tau) 1 1
    * dot normal [1 2 3 | normalize]))
  ```

  You can also pass another shape, in which case the color field will be copied to
  the destination shape:

  ```example
  (box 100 :r 10 | color
    (union (box 100 | shade [0 1 0]) (ball 125 | shade [1 0 0])))
  ```
  ````
  [shape color]
  (def color
    (if (shape? color)
      (shape/color color)
      (typecheck color jlsl/type/vec3)))
  (shape/with shape :color color))

(defn tint
  ````
  Add a color to a shape's color field.

  ```example
  (ball 100 | shade normal+ | tint [1 0 0] (sin+ t))
  ```
  ````
  [shape color &opt amount]
  (default amount 1)
  (def amount (typecheck amount jlsl/type/float))
  (def color
    (if (shape? color)
      (shape/color color)
      (typecheck color jlsl/type/vec3)))
  (assert (not (nil? (shape/color shape))) "cannot tint a shape with no color field")
  (map-color shape (fn [old-color]
    (+ old-color (* color amount)))))

(defdyn *lights* ```
The default lights used by the `shade` function.
You can manipulate this using `setdyn` or `with-dyns` like any other
dynamic variable, but there is a dedicated `with-lights` function to
set it in a way that fits nicely into a pipeline.
```)

(thunk ~(setdyn ,expression-hoister/*hoisted-vars* (table/weak-keys 8)))

# we need to make sure that the distance-function thunk has been
# registered before the thunks we register here
(require "./forward-declarations")

# TODO: this should be somewhere else and called something else
(def- MINIMUM_HIT_DISTANCE 0.01)
(def- MAX_LIGHT_STEPS 256:u)

(defn light/map
  ```
  `f` takes and returns a `Light` expression.
  ```
  [light f]
  (gl/let [light light] (f light)))

(defn light/map-brightness
  ```
  `f` takes and returns a `:float` expression.
  ```
  [light f]
  (sugar (jlsl/do
    (var light light)
    (set light.brightness (f light.brightness))
    light)))

(defn light/map-color
  ```
  `f` takes and returns a `vec3` expression.
  ```
  [light f]
  (sugar (jlsl/do
    (var light light)
    (set light.color (f light.color))
    light)))

(defhelper- :vec3 normalize-safe [:vec3 v]
  (return (if (= v (vec3 0)) v (normalize v))))

# So kind of a flaw here is that we can't make this a private definition,
# because we need it to be part of the environment so that it can be
# referenced by light/point below. We could attach some extra metadata to
# purge this from the user environment... or we could just embrace making
# it part of the API? I'm not sure.
(defhelper Light cast-light-no-shadow [:vec3 light-color :vec3 light-position]
  ```
  TODOC
  ```
  (return (Light light-color (normalize-safe (light-position - P)) 1)))

# TODO: I think that this should actually be a function that takes the distance
# expression to march through. This would let us create a light that casts
# shadows from a custom source, which would be cool and fun.

(thunk ~(as-macro ,defhelper- Light cast-light-hard-shadow [:vec3 light-color :vec3 light-position]
  ```
  TODOC
  ```
  (if (= light-position P)
    (return (Light light-color (vec3 0) 1)))
  # because it can be so hard to converge exactly
  # to a point on a shape's surface, we actually
  # march the light to a point very near the surface.
  # this produces much better results when shading
  # smooth surfaces like spheres
  (var to-light (normalize (light-position - P)))
  # don't bother marching if the light won't contribute anything
  # TODO: i feel like you're more likely to do this with falloff
  # after the fact
  (if (= light-color (vec3 0))
    (return (Light light-color to-light 0)))
  # If you're looking at a surface facing away from the light,
  # there's no need to march all the way to it. I mean,
  # theoretically you could have an infinitely-thin surface or
  # invalid normals or something, but for the most part this is just
  # a nice optimization. Another way to achieve this is to do the
  # march in the reverse direction, from the surface to the light,
  # stopping as soon as you hit the volume behind you. But I think
  # it's more clear to march from the light to the surface.
  (if (< (dot to-light normal) 0)
    (return (Light light-color to-light 0)))
  (var target (,MINIMUM_HIT_DISTANCE * normal + P))
  (var light-distance (length (target - light-position)))
  (var ray-dir (target - light-position / light-distance))
  (var depth 0)
  (for (var i 0:u) (< i ,MAX_LIGHT_STEPS) (++ i)
    (var nearest (with [P (light-position + (ray-dir * depth)) p P] (nearest-distance)))
    (if (< nearest ,MINIMUM_HIT_DISTANCE) (break))
    (+= depth nearest))
  (if (>= depth light-distance)
    (return (Light light-color to-light 1))
    (return (Light light-color to-light 0)))))

(thunk ~(as-macro ,defhelper- Light cast-light-soft-shadow [:vec3 light-color :vec3 light-position :float softness]
  ```
  TODOC
  ```
  (if (= softness 0)
    (return (cast-light-hard-shadow light-color light-position)))
  (if (= light-position P)
    (return (Light light-color (vec3 0) 1)))
  (var to-light (normalize (light-position - P)))
  (if (= light-color (vec3 0))
    (return (Light light-color to-light 0)))
  # TODO: i feel like you're more likely to do this with falloff
  # after the fact
  (if (< (dot to-light normal) 0)
    (return (Light light-color to-light 0)))
  (var target (,MINIMUM_HIT_DISTANCE * normal + P))
  (var light-distance (length (target - light-position)))
  (var ray-dir (target - light-position / light-distance))
  (var brightness 1)
  (var sharpness (softness * softness /))
  (var last-nearest ,VERY_LARGE_DISTANCE)
  (var depth 0)
  (for (var i 0:u) (< i ,MAX_LIGHT_STEPS) (++ i)
    (var nearest (with [P (light-position + (ray-dir * depth)) p P] (nearest-distance)))
    (if (< nearest ,MINIMUM_HIT_DISTANCE) (break))
    (var intersect-offset (nearest * nearest / (2 * last-nearest)))
    (var intersect-distance (sqrt (nearest * nearest - (intersect-offset * intersect-offset))))
    (set brightness (min brightness (sharpness * intersect-distance / (max 0 (light-distance - depth - intersect-offset)))))
    (+= depth nearest)
    (set last-nearest nearest))

  (if (>= depth light-distance)
    (return (Light light-color to-light brightness))
    (return (Light light-color to-light 0)))))

(thunk ~(as-macro ,defnamed light/point [color position :?shadow:softness :?brightness :?hoist]
  ```
  Returns a new light, which can be used as an input to some shading
  functions.

  Although this is called a point light, the location of the "point" can vary
  with a dynamic expression. A light that casts no shadows and is located at
  `P` (no matter where `P` is) is an ambient light. A light that is always
  located at a fixed offset from `P` is a directional light.

  By default lights don't cast shadows, but you can change that by passing a
  `:shadow` argument. `0` will cast hard shadows, and any other expression
  will cast a soft shadow (it should be a number roughly in the range `0` to
  `1`).

  Shadow casting affects the `brightness` of the light. You can also specify a
  baseline `:brightness` explicitly, which defaults to `1`.

  Shadow casting always occurs in the global coordinate space, so you should
  position lights relative to `P`, not `p`.

  By default light calculations are hoisted (see `gl/def` for more info). This
  is an optimization that's helpful if you have a light that casts shadows
  that applies to multiple shaded surfaces that have been combined with a
  smooth `union` or `morph` or other shape combinator. Instead of computing
  shadows twice and mixing them together, the shadow calculation will be
  computed once at the top level of the shader. Note though that this will
  prevent you from referring to variables that don't exist at the top
  level -- e.g. anything defined with `gl/let`, or the index argument of
  `tiled` shape. If you want to make a light that dynamically varies, pass
  `:hoist false`.
  ```
  (def color (,coerce-expr-to-type ',jlsl/type/vec3 vec3 color))
  (def position (,typecheck position ',jlsl/type/vec3))
  (default hoist true)
  (def <expr> (case softness
    nil (cast-light-no-shadow color position)
    # the soft shadow calculation below has to handle this, because
    # shadow might be a dynamic expression that is only sometimes
    # zero. but in the case that all lights have known constant zeroes,
    # there's no need to compile and include the soft shadow function
    # at all. which... will never happen but whatever
    0 (cast-light-hard-shadow color position)
    (cast-light-soft-shadow color position (,typecheck softness ',jlsl/type/float))))
  (def <expr> (if brightness
    (light/map-brightness <expr> (fn [b] (* b (,typecheck brightness ',jlsl/type/float))))
    <expr>))
  (if hoist (,expression-hoister/hoist "light" <expr>) <expr>)))

(thunk ~(as-macro ,defnamed light/ambient [color ?offset :?brightness :?hoist]
  ```
  Shorthand for `(light/point color (P + offset))`.

  With no offset, the ambient light will be completely directionless, so it won't
  contribute to specular highlights. By offsetting by a multiple of the surface
  normal, or by the surface normal plus some constant, you can create an ambient
  light with specular highlights, which provides some depth in areas of your scene
  that are in full shadow.
  ```
  (def offset (,typecheck? offset ',jlsl/type/vec3))
  (light/point color (if offset (+ P offset) P)
    :hoist hoist
    :brightness brightness)))

(thunk ~(as-macro ,defnamed light/directional [color dir dist :?shadow:softness :?brightness :?hoist]
  ```
  A light that hits every point at the same angle.

  Shorthand for `(light/point color (P - (dir * dist)))`.
  ```
  (def dir (,typecheck dir ',jlsl/type/vec3))
  (def dist (,typecheck dist ',jlsl/type/float))
  (light/point color (- P (* dir dist))
    :shadow softness
    :hoist hoist
    :brightness brightness)))

# TODO: probably we should take a direction?
(defn- make-calculate-occlusion [nearest-distance]
  (defhelper :float calculate-occlusion [:uint step-count :float max-distance :vec3 dir]
    (var step-size (max-distance / float step-count))
    (var baseline (nearest-distance))
    (var occlusion 0)
    (var step (dir * step-size))
    (for (var i 1:u) (<= i step-count) (++ i)
      (var expected-distance (float i * step-size + baseline))
      # TODO: do I want the max 0 there?
      (var actual-distance (with [P (float i * step + P) p P] ((nearest-distance) | max 0)))
      (+= occlusion (actual-distance / expected-distance)))
    (return (occlusion / float step-count | clamp 0 1)))
  calculate-occlusion)

(thunk ~(def calculate-occlusion (,make-calculate-occlusion nearest-distance)))
(thunk ~(as-macro ,defnamed occlusion [:?steps:step-count :?dist :?dir :?hoist]
  ```
  Approximate ambient occlusion by sampling the distance field at `:steps` positions
  (default 8) linearly spaced from 0 to `:dist` (default `10`). The result will range
  from 1 (completely unoccluded) to 0 (fully occluded).

  By default the occlusion samples will be taken along the surface normal of the point
  being shaded, but you can pass a custom `:dir` expression to change that. You can use
  this to e.g. add jitter to the sample direction, which can help to improve the
  quality.

  Occlusion is somewhat expensive to calculate, so by default the result will
  be hoisted, so that it's only calculated once per iteration (without you having to
  explicitly `gl/def` the result). However, this means that the occlusion calculation
  won't take into account local normal adjustments, so you might want to pass
  `:hoist false`.
  ```
  (default step-count 8)
  (default dir normal)
  (default dist 10)
  (default hoist true)
  (def step-count (if (number? step-count) (int/u64 step-count) step-count))
  (def <expr> (calculate-occlusion step-count dist dir))
  (if hoist (,expression-hoister/hoist "occlusion" <expr>) <expr>)))

(sugar (thunk ~(setdyn ,*lights*
  (let [default-occlusion (mix 0.1 1 (occlusion))]
    @[(light/directional (vec3 1.2) [-2 -2 -1 | normalize] 512 :shadow 0.25)
      (light/ambient (vec3 0.1) :brightness default-occlusion)
      (light/ambient (vec3 0.1) (normal * 0.1) :brightness default-occlusion)]))))

(defhelper- :vec3 blinn-phong [Light light :vec3 color :float shininess :float glossiness]
  (if (= light.direction (vec3 0))
    (return (* color light.color light.brightness)))
  (var halfway-dir (light.direction - ray.direction | normalize))
  (var specular-strength (shininess * pow (max (dot normal halfway-dir) 0) (glossiness * glossiness)))
  (var diffuse (max 0 (dot normal light.direction)))
  (return (light.color * light.brightness * specular-strength + (* color diffuse light.color light.brightness))))
(def- blinn-phong- blinn-phong)

(defnamed blinn-phong [light color :?s:shininess :?g:glossiness]
  ````
  A Blinn-Phong shader, intended to be passed as an argument to `shade`. `:s` controls
  the strength of specular highlights, and `:g` controls the glossiness.

  ```example
  (ball 100 | shade :f blinn-phong [1 0 0] :s 1 :g (osc t 5 5 30))
  ```
  ````
  (default shininess 0.25)
  (default glossiness 10)
  (blinn-phong- light color shininess glossiness))

(defnamed shade [shape :?f &args :&kargs]
  ````
  `shade` colors a shape with a map-reduce over the current lights.
  It's a higher-order operation that takes a shading function (by
  default `blinn-phong`) -- and calls it once for every light in `*lights*`.

  ```example
  (ball 100 | shade [1 0.25 0.5])
  ```

  All arguments to `shade` will be passed to the shading function,
  and only evaluated once, no matter how many lights there are. See the
  documentation for `blinn-phong` for a description of the arguments
  it takes.

  If you define a custom shading function, its first argument should be a light:

  ```example
  (ball 100 | shade [1 0.25 0.5] :f (fn [light color]
    (dot normal light.direction * light.brightness
    | quantize 5 * light.color * color
  )))
  ```

  A light is a struct with three fields: `color`, `direction`,
  and `brightness`. `brightness` is roughly "shadow intensity,"
  and in the default lights it includes an ambient occlusion
  component. This shader hardens the edges of shadows with step,
  and uses it to apply a custom shadow color:

  ```example
  (torus x 10 5 | rotate z t | radial y 20 100 | union (ground -120)
  | shade [0.5 1.0 1.0] :f (fn [light color]
    (dot normal light.direction * light.color * (mix [0.5 0 0] color (step 0.5 light.brightness))
    )))
  ```

  Why does a light give you a `direction` instead of a `position`? `direction` is a little
  more robust because you don't have to remember to special-case the zero vector (ambient
  lights), and theoretically if you want to do anything position-dependent you should reflect
  that in the `color` or `brightness`. But maybe it should give you both. I'm torn now.
  ````
  (default f blinn-phong)
  (def bindings @[])
  (defn save [value]
    (if (jlsl/expr? value)
      (let [v (jlsl/variable/new "temp" (jlsl/expr/type value))]
        (array/push bindings [v value])
        v)
      value))

  (def args (map save args))
  (def kargs (mapcat (fn [[k v]] [k (save v)]) (pairs kargs)))

  (shape/with shape :color
    (jlsl/with-expr bindings []
      (jlsl/do "shade"
        (var result (vec3 0))
        ,;(seq [light :in (dyn *lights*)]
          (jlsl/statement (+= result ,(f light ;args ;kargs))))
        result)
      "shade")))

(defmacro with-lights
  ````
  Evaluate `shape` with the `*lights*` dynamic variable set to the provided lights.

  The argument order makes it easy to stick this in a pipeline. For example:

  ```example
  (ball 100
  | shade [1 0 0]
  | with-lights
    (light/point 0.5 [100 100 0])
    (light/ambient 0.5))
  ```
  ````
  [shape & lights]
  ~(as-macro ,with-dyns [:lights (,tuple ,;lights)] ,shape))

(defhelper :vec3 hsv [:float hue :float saturation :float value]
  ```
  Returns a color.
  ```
  (var c (hue * 6 + [0 4 2] | mod 6 - 3 | abs))
  (return (value * (mix (vec3 1) (c - 1 | clamp 0 1) saturation))))

(defhelper :vec3 hsl [:float hue :float saturation :float lightness]
  ```
  Returns a color.
  ```
  (var c (hue * 6 + [0 4 2] | mod 6 - 3 | abs))
  (return (* saturation (c - 1 | clamp 0 1 - 0.5) (1 - abs (2 * lightness - 1)) + lightness)))

(defhelper- :float fresnel [:float exponent]
  (return (1 + dot normal ray.direction | pow exponent)))
(def- fresnel- fresnel)

(defnamed fresnel [?exponent]
  ````
  Returns an approximate fresnel intensity. `exponent` defaults to `5`.

  ```example
  (ball 100
  | shade [1 0.5 0.5]
  | tint [1 1 1] (fresnel (osc t 5 0.5 5)))
  ```
  ````
  (default exponent 5)
  (fresnel- (typecheck exponent jlsl/type/float)))

(def normal+
  ```
  A color that represents the visualization of the 3D normal. This is
  the default color used when rendering a 3D shape with no color field.
  ```
  (remap+ normal))

(def isolines
  ```
  A color that represents the visualization of the 2D gradient. This is
  the default color used when rendering a 2D shape with no color field.
  ```
  (sugar (jlsl/do "isolines"
    (def line-every 12)
    (def shadow-thickness 0.5)
    (def boundary-thickness 2)

    (var color (remap+ gradient))

    (var inside (step dist 0))
    (var isoline (smoothstep (1 - shadow-thickness) 1 (abs dist / line-every | fract)))
    (var boundary-line (1 - (smoothstep 0 (boundary-thickness * 0.5) (abs dist))))

    (mix
      (pow [color inside] (mix 1 2 isoline) | clamp 0 1)
      (vec3 1)
      boundary-line))))

# TODO: make this a dynamic var, or possibly
# make it vary based on depth
(def- NORMAL_OFFSET 0.005)

# TODO: this should take a shape, not a distance
(defn calculate-gradient
  ```
  Evaluates the given 2D distance expression four times, and returns an approximation
  of the expression's gradient.
  ```
  [expr]
  (def step (vec2 NORMAL_OFFSET 0))
  (sugar (jlsl/do (normalize [
    (with [q (q + step.xy)] expr - with [q (q - step.xy)] expr)
    (with [q (q + step.yx)] expr - with [q (q - step.yx)] expr)
    ]))))

# TODO: this should take a shape, not a distance
(defn calculate-normal
  ```
  Evaluates the given 3D distance expression four times, and returns an approximation
  of the expression's gradient.
  ```
  [expr]
  (def s [1 -1])
  (sugar (jlsl/do (normalize (+
    (s.xyy * with [p (s.xyy * NORMAL_OFFSET + p)] expr)
    (s.yyx * with [p (s.yyx * NORMAL_OFFSET + p)] expr)
    (s.yxy * with [p (s.yxy * NORMAL_OFFSET + p)] expr)
    (s.xxx * with [p (s.xxx * NORMAL_OFFSET + p)] expr)
    )))))

(def graydient
  ```
  The default background color, a gray gradient.
  ```
  (sugar (jlsl/do
    (def light (pow ([69 72 79] / 255) (vec3 2.2)))
    (def dark (pow ([40 42 46] / 255) (vec3 2.2)))
    (vec3 (mix dark light (Frag-Coord.x + Frag-Coord.y / (resolution.x + resolution.y)))))))

(defnamed bump [shape by ?amount]
  ````
  Alter the `normal` for a shape. You can use this along
  with noise expressions to give the appearance of texture
  without the expense of evaluating the offset multiple
  times during the march.

  Compare, an actually-bumpy shape:

  ```example
  (ball 100 | shade red | expand (perlin (p / 10)))
  ```

  To a shape with normals inspired by that bumpiness:

  ```example
  (ball 100 | shade red | bump (perlin (p / 10)) 0.3)
  ```

  This is much cheaper than using `expand`, so if you're only trying to add
  a little texture and don't need to change the shape, consider using `bump`
  instead.

  (If you really do care about distorting the geometry, see `expound` for
  a more efficient way to do that.)

  The expression to `bump` will be evaluated using `calculate-normal`,
  so it should vary with `p`. This is a much cheaper way to add texture
  than trying to sculpt it into the distance field. Orange peel:

  ```example
  (ball 100 | shade (hsv 0.05 1 0.75) | bump (perlin+ p) 0.2)
  (set camera (camera/perspective [(cos (t / 2)) 0 (sin (t / 2)) * 200] | camera/zoom (osc t 7 1 2)))
  ```
  ````
  (default amount 1)
  (def by (if (shape? by) (shape/distance by) (typecheck by jlsl/type/float)))
  (def amount (typecheck amount jlsl/type/float))
  (assert (shape/color shape) "you have to set a color field before you can bump it up")
  (sugar (gl/with :color [normal (normal - (calculate-normal by * amount * by) | normalize)]
    shape)))
