# A collection of fields.
#
# All of the fields must share the same dimension.
#
# These might be 3D distance fields, 2D distance fields,
# 3D color fields, 2D color fields, etc.
#
# You could have something that is just a color field, or
# a combination of a distance and color field.
#
# Ultimately a "field" is just a JLSL expression.

(import ../jlsl/type :as jlsl)

(def- tag (gensym))

(defn new [type & kvs]
  (struct
    :type type
    :tag tag
    ;kvs))

(defn distance-2d [expr]
  {:type jlsl/type/vec2
   :tag tag
   :distance expr})

(defn distance-3d [expr]
  {:type jlsl/type/vec3
   :tag tag
   :distance expr})

(defn type [t] (t :type))

(defn- map-if [x f] (if x (f x)))

(defn is? [x] (and (struct? x) (= (x :tag) tag)))

(defn map [t f &opt type]
  {:type (or type (t :type))
   :tag tag
   :distance (map-if (t :distance) f)
   :color (map-if (t :color) f)})

(defn with [t & new-kvs] (struct ;(kvs t) ;new-kvs))

(defn map-distance [t f]
  {:type (t :type)
   :tag tag
   :distance (map-if (t :distance) f)
   :color (t :color)})
