(use ./import)

# we need to make sure that the distance-function thunk has been
# registered before the thunks we register here
(require "./forward-declarations")

(deftransform color [shape color]
  "Set the color field of a shape."
  (typecheck color jlsl/type/vec3)
  (shape/replace shape :color color))

# TODO: this should be somewhere else and called something else
(def- MINIMUM_HIT_DISTANCE 0.01)
(def- MAX_LIGHT_STEPS :256)

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

# TODO: we should be able to check the dot product with the normal here and
# short-circuit the marching for anything facing away from the light
(thunk ~(as-macro ,defhelper- LightIncidence cast-light-hard-shadow [:vec3 light-color :vec3 light-position]
  ```
  TODOC
  ```
  (if (= light-position P)
    (return (LightIncidence light-color (vec3 0))))
  (var target (,MINIMUM_HIT_DISTANCE * normal + P))
  (var target-distance (distance light-position target))
  (var to-light (light-position - target / target-distance))
  # don't bother marching if the light won't contribute anything
  (if (= light-color (vec3 0))
    (return (LightIncidence light-color to-light)))
  (var progress 0)
  (for (var i :0) (< i ,MAX_LIGHT_STEPS) (++ i)
    (var distance (with [P (light-position - (to-light * progress)) p P] (nearest-distance)))
    (if (< distance ,MINIMUM_HIT_DISTANCE)
      (if (>= (progress + distance) (target-distance - ,MINIMUM_HIT_DISTANCE))
        (return (LightIncidence light-color to-light))
        (return (LightIncidence (vec3 0) to-light))))
    (+= progress distance))
  (return (LightIncidence (vec3 0) to-light))))

(thunk ~(as-macro ,defhelper- LightIncidence cast-light-soft-shadow [:vec3 light-color :vec3 light-position :float softness]
  ```
  TODOC
  ```
  (if (= softness 0)
    (return (cast-light-hard-shadow light-color light-position)))
  (if (= light-position P)
    (return (LightIncidence light-color (vec3 0))))
  # TODO: we do this adjustment because if you land inside a shape you want to try to get out of it.
  (var target (,MINIMUM_HIT_DISTANCE * normal + P))
  (var target-distance (distance light-position target))
  (if (= target-distance 0)
    (return (LightIncidence light-color (vec3 0))))
  (var to-light (light-position - target / target-distance))
  # don't bother marching if the light won't contribute anything
  (if (= light-color (vec3 0))
    (return (LightIncidence light-color to-light)))
  (var in-light 1)
  (var sharpness (softness * softness /))
  (var last-distance 1e10)
  (var progress 0)
  (for (var i :0) (< i ,MAX_LIGHT_STEPS) (++ i)
    (var distance (with [P (light-position - (to-light * progress)) p P] (nearest-distance)))
    (if (< distance ,MINIMUM_HIT_DISTANCE)
      (if (>= (progress + distance) (target-distance - ,MINIMUM_HIT_DISTANCE))
        (return (LightIncidence (in-light * light-color) to-light))
        (return (LightIncidence (vec3 0) to-light))))

    (if (< distance last-distance) (do
      (var intersect-offset (distance * distance / (2 * last-distance)))
      (var intersect-distance (sqrt (distance * distance - (intersect-offset * intersect-offset))))
      (set in-light (min in-light (sharpness * intersect-distance / (max 0 (target-distance - progress - intersect-offset)))))))
    (+= progress distance)
    (set last-distance distance))
  (return (LightIncidence (vec3 0) to-light))))

(def- light-tag (gensym))

(thunk ~(defn light/point
  ```
  Returns a new light, which can be used as an input to some shading functions.

  Although this is called a point light, the location of the "point" can vary
  with a dynamic expression. A light that casts no shadows and is located at `P`
  (no matter where `P` is) is an ambient light. A light that is always located at
  a fixed offset from `P` is a directional light.

  `shadow` can be `nil` to disable casting shadows, or a number that controls the
  softness of the shadows that the light casts. `0` means that shadows will have
  hard edges. The default value is `0.25`.

  Lighting always occurs in the global coordinate space, so you should position lights
  relative to `P`, not `p`. They will be the same at the time that lights are computed,
  so it doesn't *actually* matter, but it's just more accurate to use `P`, and there's
  a chance that a future version of Bauble will support computing lights in local
  coordinate spaces, where `p` might vary.
  ```
  [color position & shadow]
  (assert (<= (@length shadow) 1) "too many arguments")
  (def shadow (if (empty? shadow) 0.25 (shadow 0)))
  {:tag ',light-tag
   :variable (,jlsl/variable/new "light" (,jlsl/type/coerce LightIncidence))
   :expression
     (case shadow
      nil (cast-light-no-shadow color position)
      # the soft shadow calculation below has to handle this, because
      # shadow might be a dynamic expression that is only sometimes
      # zero. but in the case that all lights have known constant zeroes,
      # there's no need to compile and include the soft shadow function
      # at all. which... will never happen but whatever
      0 (cast-light-hard-shadow color position)
      (cast-light-soft-shadow color position shadow))}))

(thunk ~(defn light/ambient
  ```
  Shorthand for `(light/point color P nil)`.
  ```
  [color]
  (light/point color P nil)))

(thunk ~(defn light/directional
  ```
  A light that hits every point at the same angle.

  Shorthand for `(light/point color (P - (dir * distance)) shadow)`.
  ```
  [color dir distance & shadow]
  # TODO: these coercions aren't necessary if we overload * to work on tuples
  (light/point color (- P (* (,jlsl/coerce-expr dir) (,jlsl/coerce-expr distance))) ;shadow)))

(defn light?
  "Returns true if its argument is a light."
  [t]
  (and (struct? t) (= (t :tag) light-tag)))

# TODO: should this just be a dynamic variable? why isn't this a dynamic variable?
(thunk ~(var lights "The default lights used by surfacing functions like `blinn-phong`.\n\nAlthough `lights` is an ordinary lexical variable, it behaves like a dynamic variable. Why isn't it a dynamic variable? Good question."
  @[(light/directional [1 1 1] (normalize [-2 -2 -1]) 512)]))

(defhelper- :vec3 blinn-phong1 [:vec3 color :float shine :float gloss :vec3 light-color :vec3 light-dir]
  # TODO: ray-dir should just be available as a dynamic variable
  (if (= light-dir (vec3 0))
    (return (* color light-color)))
  (var view-dir (camera-origin - P | normalize))
  (var halfway-dir (light-dir + view-dir | normalize))
  (var specular-strength (shine * pow (max (dot normal halfway-dir) 0) (gloss * gloss)))
  (var diffuse (max 0 (dot normal light-dir)))
  (return (light-color * specular-strength + (* color diffuse light-color))))

(defn- blinn-phong-color-expression [color shine gloss lights]
  (jlsl/do "blinn-phong"
    (var result (vec3 0))
    ,;(seq [light :in lights]
      (def incidence-variable (light :variable))
      # TODO hmmmmm okay wait a minute. can't we use the position of the light
      # to just determine the incidence angle? why do we need to forward it? can
      # the incidence just be a color and nothing else? unless we like... we don't
      # distort this, right?
      (jlsl/statement
        (+= result (blinn-phong1 color shine gloss
          (. incidence-variable color)
          (. incidence-variable direction)))))
    result))

(defn- hoisties [lights]
  (tabseq [{:variable variable :expression expression} :in lights]
    variable expression))

(defn blinn-phong [shape color shine gloss &opt lights]
  ```
  TODOC
  ```
  (default lights (get-var (curenv) 'lights))
  (assertf (indexed? lights) "%q should be a list" lights)
  (each light lights
    (assertf (light? light) "%q is not a light" light))
  (shape/replace shape :color
    (blinn-phong-color-expression color shine gloss lights)
    (hoisties lights)))

(defn resurface [dest-shape source-shape]
  (shape/transplant :color source-shape dest-shape))
