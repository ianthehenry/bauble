(use ./import)
(import ../expression-hoister)

(defdyn *lights* "The default lights used by surfacing functions like `blinn-phong`. You can manipulate this using `setdyn` or `with-dyns` like any other dynamic variable, but there is a dedicated `with-lights` function to set it in a way that fits nicely into a pipeline.")

(thunk ~(setdyn ,expression-hoister/*hoisted-vars* (table/weak-keys 8)))

# we need to make sure that the distance-function thunk has been
# registered before the thunks we register here
(require "./forward-declarations")

# TODO: this should be somewhere else and called something else
(def- MINIMUM_HIT_DISTANCE 0.01)
(def- MAX_LIGHT_STEPS 256:u)

(jlsl/jlsl/defstruct LightIncidence
  :vec3 color
  :vec3 direction)

(defhelper- :vec3 normalize-safe [:vec3 v]
  (return (if (= v (vec3 0)) v (normalize v))))

# So kind of a flaw here is that we can't make this a private definition,
# because we need it to be part of the environment so that it can be
# referenced by light/point below. We could attach some extra metadata to
# purge this from the user environment... or we could just embrace making
# it part of the API? I'm not sure.
(defhelper LightIncidence cast-light-no-shadow [:vec3 light-color :vec3 light-position]
  ```
  TODOC
  ```
  (return (LightIncidence light-color (normalize-safe (light-position - P)))))

# TODO: I think that this should actually be a function that takes the distance
# expression to march through. This would let us create a light that casts
# shadows from a custom source, which would be cool and fun.

(thunk ~(as-macro ,defhelper- LightIncidence cast-light-hard-shadow [:vec3 light-color :vec3 light-position]
  ```
  TODOC
  ```
  (if (= light-position P)
    (return (LightIncidence light-color (vec3 0))))
  # because it can be so hard to converge exactly
  # to a point on a shape's surface, we actually
  # march the light to a point very near the surface.
  # this produces much better results when shading
  # smooth surfaces like spheres
  (var to-light (normalize (light-position - P)))
  # don't bother marching if the light won't contribute anything
  (if (= light-color (vec3 0))
    (return (LightIncidence light-color to-light)))
  # If you're looking at a surface facing away from the light,
  # there's no need to march all the way to it. I mean,
  # theoretically you could have an infinitely-thin surface or
  # invalid normals or something, but for the most part this is just
  # a nice optimization. Another way to achieve this is to do the
  # march in the reverse direction, from the surface to the light,
  # stopping as soon as you hit the volume behind you. But I think
  # it's more clear to march from the light to the surface.
  (if (< (dot to-light normal) 0)
    (return (LightIncidence (vec3 0) to-light)))
  (var target (,MINIMUM_HIT_DISTANCE * normal + P))
  (var light-distance (length (target - light-position)))
  (var ray-dir (target - light-position / light-distance))
  (var depth 0)
  (for (var i 0:u) (< i ,MAX_LIGHT_STEPS) (++ i)
    (var nearest (with [P (light-position + (ray-dir * depth)) p P] (nearest-distance)))
    (if (< nearest ,MINIMUM_HIT_DISTANCE) (break))
    (+= depth nearest))
  (if (>= depth light-distance)
    (return (LightIncidence light-color to-light))
    (return (LightIncidence (vec3 0) to-light)))))

(thunk ~(as-macro ,defhelper- LightIncidence cast-light-soft-shadow [:vec3 light-color :vec3 light-position :float softness]
  ```
  TODOC
  ```
  (if (= softness 0)
    (return (cast-light-hard-shadow light-color light-position)))
  (if (= light-position P)
    (return (LightIncidence light-color (vec3 0))))
  (var to-light (normalize (light-position - P)))
  (if (= light-color (vec3 0))
    (return (LightIncidence light-color to-light)))
  (if (< (dot to-light normal) 0)
    (return (LightIncidence (vec3 0) to-light)))
  (var target (,MINIMUM_HIT_DISTANCE * normal + P))
  (var light-distance (length (target - light-position)))
  (var ray-dir (target - light-position / light-distance))
  (var in-light 1)
  (var sharpness (softness * softness /))
  (var last-nearest 1e10)
  (var depth 0)
  (for (var i 0:u) (< i ,MAX_LIGHT_STEPS) (++ i)
    (var nearest (with [P (light-position + (ray-dir * depth)) p P] (nearest-distance)))
    (if (< nearest ,MINIMUM_HIT_DISTANCE) (break))
    (var intersect-offset (nearest * nearest / (2 * last-nearest)))
    (var intersect-distance (sqrt (nearest * nearest - (intersect-offset * intersect-offset))))
    (set in-light (min in-light (sharpness * intersect-distance / (max 0 (light-distance - depth - intersect-offset)))))
    (+= depth nearest)
    (set last-nearest nearest))

  (if (>= depth light-distance)
    (return (LightIncidence (in-light * light-color) to-light))
    (return (LightIncidence (vec3 0) to-light)))))

(def- light-tag (gensym))

# TODO: should we really have a named argument like this? maybe just a note that you probably
# should hoist your lights? or should hoist be the default? i don't know.
(thunk ~(as-macro ,defnamed light/point [color position :?shadow:softness :?hoist]
  ```
  Returns a new light, which can be used as an input to some shading functions.

  Although this is called a point light, the location of the "point" can vary
  with a dynamic expression. A light that casts no shadows and is located at `P`
  (no matter where `P` is) is an ambient light. A light that is always located at
  a fixed offset from `P` is a directional light.

  By default lights don't cast shadows, but you can change that by passing a
  `:shadow` argument. `0` will cast hard shadows, and any other expression will
  cast a soft shadow (`0.25` is a reasonable default softness).

  Shadow casting always occurs in the global coordinate space, so you should position
  lights relative to `P`, not `p`.

  If you pass `:hoist true`, then the light computation will be hoisted to the top
  of the color field and only calculated once. This is an optimization that's useful
  if you have a light that casts shadows that applies to multiple shaded surfaces that
  have been combined with a smooth `union` or `morph` or other shape combinator. Instead
  of computing shadows twice and mixing them together, the shadow calculation will be
  computed once at the top level of the shader. Note though that this will prevent you
  from referring to variables that don't exist at the top level -- e.g. anything defined
  with `gl/let`, or the index argument of `tiled` shape.
  ```
  (def color (,coerce-expr-to-type ',jlsl/type/vec3 vec3 color))
  (def position (,typecheck position ',jlsl/type/vec3))
  (def <expr> (case softness
    nil (cast-light-no-shadow color position)
    # the soft shadow calculation below has to handle this, because
    # shadow might be a dynamic expression that is only sometimes
    # zero. but in the case that all lights have known constant zeroes,
    # there's no need to compile and include the soft shadow function
    # at all. which... will never happen but whatever
    0 (cast-light-hard-shadow color position)
    (cast-light-soft-shadow color position (,typecheck softness ',jlsl/type/float))))
  {:tag ',light-tag
   :expression (if hoist (,expression-hoister/hoist "light" <expr>) <expr>)}))

(thunk ~(as-macro ,defnamed light/ambient [color ?offset :?hoist]
  ```
  Shorthand for `(light/point color (P + offset))`.

  With no offset, the ambient light will be completely directionless, so it won't
  contribute to specular highlights. By offsetting by a multiple of the surface
  normal, or by the surface normal plus some constant, you can create an ambient
  light with specular highlights, which provides some depth in areas of your scene
  that are in full shadow.
  ```
  (light/point color (if offset (+ P (,typecheck offset ',jlsl/type/vec3)) P) :hoist hoist)))

(thunk ~(as-macro ,defnamed light/directional [color dir dist :?shadow:softness :?hoist]
  ```
  A light that hits every point at the same angle.

  Shorthand for `(light/point color (P - (dir * dist)))`.
  ```
  (def dir (,typecheck dir ',jlsl/type/vec3))
  (def dist (,typecheck dist ',jlsl/type/float))
  (light/point color (- P (* dir dist)) :shadow softness :hoist hoist)))

(defn light?
  "Returns true if its argument is a light."
  [t]
  (and (struct? t) (= (t :tag) light-tag)))

(thunk ~(setdyn ,*lights*
  @[(light/directional (vec3 0.95) (normalize [-2 -2 -1]) 512 :shadow 0.25 :hoist true)
    (light/ambient (vec3 0.03) :hoist true)
    (light/ambient (vec3 0.02) (* normal 0.1) :hoist true)]))

(defhelper- :vec3 blinn-phong1 [:vec3 color :float shininess :float glossiness :vec3 light-color :vec3 light-dir]
  (if (= light-dir (vec3 0))
    (return (* color light-color)))
  (var halfway-dir (light-dir - ray-dir | normalize))
  (var specular-strength (shininess * pow (max (dot normal halfway-dir) 0) (glossiness * glossiness)))
  (var diffuse (max 0 (dot normal light-dir)))
  (return (light-color * specular-strength + (* color diffuse light-color))))

(defn- blinn-phong-color-expression [color shininess glossiness lights]
  (jlsl/do "blinn-phong"
    (var result (vec3 0))
    ,;(seq [light :in lights]
      (def light-incidence (light :expression))
      # TODO hmmmmm okay wait a minute. can't we use the position of the light
      # to just determine the incidence angle? why do we need to forward it? can
      # the incidence just be a color and nothing else? unless we like... we don't
      # distort this, right?
      (jlsl/statement
        (+= result (blinn-phong1 color shininess glossiness
          (. light-incidence color)
          (. light-incidence direction)))))
    result))

(defnamed blinn-phong [shape color :?s:shininess :?g:glossiness]
  ```
  TODOC
  ```
  (default shininess 0.25)
  (default glossiness 5)
  (shape/with shape :color (blinn-phong-color-expression color shininess glossiness (dyn :lights))))

(defn recolor
  "Replaces the color field on `dest-shape` with the color field on `source-shape`. Does not affect the distance field."
  [dest-shape source-shape]
  (shape/transplant dest-shape :color source-shape))

(defmacro with-lights
  ````
  Evaluate `shape` with the `*lights*` dynamic variable set to the provided lights.

  The argument order makes it easy to stick this in a pipeline. For example:

  ```
  (sphere 50 | blinn-phong [1 0 0] | with-lights light1 light2)
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

(defhelper- :float fresnel-intensity [:float exponent]
  (return (1 + dot normal ray-dir | pow exponent)))

(defnamed fresnel [subject :?color :?exponent]
  ```
  Tint a shape with an approximation of Fresnel reflectivity.

  `:color` defaults to `[1 1 1]`; `:exponent` defaults to `5`.
  ```
  (default color [1 1 1])
  (default exponent 5)
  (shape/map-color subject |(+ $ (* color (fresnel-intensity exponent)))))

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
    (def line-every 10)
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
