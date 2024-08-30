(use ./import)
(use ../../jlsl/adt)

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

(defhelper- LightIncidence cast-light-no-shadow [:vec3 light-color :vec3 light-position]
  (var target (MINIMUM_HIT_DISTANCE * normal + p))
  (var target-distance (distance light-position target))
  # TODO: we could eliminate ambient lights altogether if we just
  # defined them as point lights at location `P`?
  #(if (= target-distance 0)
  #  (return (LightIncidence (vec3 0) light-color)))
  (var to-light (light-position - target / target-distance))
  (return (LightIncidence light-color to-light)))

(thunk ~(as-macro ,defhelper- LightIncidence cast-light-soft-shadow [:vec3 light-color :vec3 light-position :float softness]
  ```
  TODOC
  ```
  # TODO: handle softness of 0 correctly
  #if (softness == 0.0) {
  #  cast-light-hard-shadow(light-color light-position)
  #}
  (var target (,MINIMUM_HIT_DISTANCE * normal + p))
  (var target-distance (distance light-position target))
  (if (= target-distance 0)
    (return (LightIncidence light-color (vec3 0))))
  (var to-light (light-position - target / target-distance))
  # if a light with falloff doesn't reach this point,
  # there's no need to do the full raymarch
  (if (= light-color (vec3 0))
    (return (LightIncidence light-color to-light)))
  (var in-light 1)
  (var sharpness (softness * softness /))
  (var last-distance 1e10)
  (var progress 0)
  (for (var i :0) (< i ,MAX_LIGHT_STEPS) (++ i)
    (var distance (with [p (light-position - (to-light * progress))] (nearest-distance)))
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

# TODO: should have some information about shadows
(defadt- light
  (ambient color)
  (point color-expression position-expression incidence-variable expr))

(thunk ~(defn light/new [color position]
  (def variable (,jlsl/variable/new "light" (,jlsl/type/coerce LightIncidence)))
  (,light/point color position variable (cast-light-soft-shadow color position 1))))

(defhelper- :vec3 blinn-phong1 [:vec3 color :float shine :float gloss :vec3 light-color :vec3 light-dir]
  # TODO: ray-dir should just be available as a dynamic variable
  (var view-dir (camera-origin - P | normalize))
  (var halfway-dir (light-dir + view-dir | normalize))
  (var specular-strength (shine * pow (max (dot normal halfway-dir) 0) (gloss * gloss)))
  (var diffuse (max 0 (dot normal light-dir)))
  (return (light-color * specular-strength + (* color diffuse light-color))))

(defn- blinn-phong-color-expression [color shine gloss lights]
  (jlsl/do "blinn-phong"
    (var result (vec3 0))
    ,;(seq [light :in lights]
      # TODO hmmmmm okay wait a minute. can't we use the position of the light
      # to just determine the incidence angle? why do we need to forward it? can
      # the incidence just be a color and nothing else? unless we like... we don't
      # distort this, right?
      (light/match light
        (point _ _ incidence-variable _) (jlsl/statement
          (+= result (blinn-phong1 color shine gloss
            (. incidence-variable color)
            (. incidence-variable direction))))
        (ambient _) (error "oh no")))
    result))

(defn- hoisties [lights]
  (var result @{})
  (each light lights
    (light/match light
      (ambient _) nil
      (point _ _ incidence-variable expr)
        (put result incidence-variable expr)))
  result)

(defn blinn-phong [shape color shine gloss lights]
  ```
  TODOC
  ```
  (shape/replace shape :color
    (blinn-phong-color-expression color shine gloss lights)
    (hoisties lights)))

(defn resurface [dest-shape source-shape]
  (shape/transplant :color source-shape dest-shape))
