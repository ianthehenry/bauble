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

(def fields @{
  :type jlsl/type/vec3
  :fields
  :distance
  :color
  :normal
  })
