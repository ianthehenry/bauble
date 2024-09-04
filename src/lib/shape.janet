# A shape is a collection of two things:
#
# 1. A dimension -- a shape is either 2D or 3D.
# 2. A set of fields -- these should probably be JLSL expressions, although the
#    module is technically agnostic to this. These might be distance fields,
#    color fields, or arbitrary user-defined fields. Although user-defined fields
#    aren't really well-supported yet.

(import ../jlsl/type :as jlsl)
(use judge)
(use ./util)

(def- tag (gensym))

(defn new
  ````
  Returns a new shape with the given type and fields.

  ```
  # red circle with radius 10
  (shape/new jlsl/type/vec2
    :distance (length q - 10)
    :color [1 0 0])
  ```
  ````
  [type & fields]
  (struct
    :type type
    :tag tag
    :fields (struct ;fields)))

(defn- shape/2d
  ```
  Returns a new 2D shape with the given distance field.
  ```
  [distance]
  {:type jlsl/type/vec2
   :tag tag
   :fields {:distance (typecheck distance jlsl/type/float)}})

(defn- shape/3d
  ```
  Returns a new 3D shape with the given distance field.
  ```
  [distance]
  {:type jlsl/type/vec3
   :tag tag
   :fields {:distance (typecheck distance jlsl/type/float)}})

(export shape/2d)
(export shape/3d)

(defn type
  ```
  Returns the dimension of a shape, as a JLSL type equal to the dimension of a point
  in the shape -- either `vec2` or `vec3`.
  ```
  [shape] (shape :type))

(defn- map-opt [f x] (if x (f x)))
(defn- map-struct [f t] (table/to-struct (tabseq [[k v] :pairs t] k (f v))))

(test (map-struct |(* $ 2) {:foo 1 :bar 2}) {:bar 4 :foo 2})

(defn is?
  "Returns `true` if its argument is a shape."
  [x]
  (and (struct? x) (= (x :tag) tag)))

(defn merge
  ```
  Merge multiple shapes together. `shapes` should be a list of shapes that all
  have the same dimension.

  `f` will be called with an array of all of the fields from each shape, and
  should return a struct with the fields for the new shape.

  `merge` returns a new shape with the same dimension as its inputs.
  ```
  [shapes f]
  (def type (get-unique type shapes (fn [types]
    (if (empty? types)
      (error "no shapes to combine")
      (error "cannot combine shapes with different dimensions")))))
  {:type type
   :tag tag
   :fields (f (map |(in $ :fields) shapes))})

(defn map
  ```
  Alter the fields on a shape, optionally changing its dimension in the process.
  ```
  [shape f &opt type]
  {:type (or type (shape :type))
   :tag tag
   :fields (map-struct f (shape :fields))})

(defn with
  ```
  Replace arbitrary fields on a shape.

  You probably don't want to use this. Theoretically shapes
  in Bauble are collections of arbitrary fields, but in practice
  `:color` and `:distance` are the only fields that are really
  supported in a meaningful way.

  But you could associate other fields with shapes, and use that to
  model, for example, analytic normals. But none of the existing
  infrastructure will understand you if you do this.
  ```
  [shape & new-kvs]
  {:type (shape :type)
   :tag tag
   :fields (struct ;(kvs (shape :fields)) ;new-kvs)})

(defn map-field
  ```
  Map a single field on a shape. If the field does not exist, this does nothing.
  ```
  [shape field f] (with shape field (map-opt f (in (in shape :fields) field))))

(defn map-distance
  ```
  Shorthand for `(shape/map-field shape :distance f)`.
  ```
  [shape f]
  (map-field shape :distance f))

(defn map-color
  ```
  Shorthand for `(shape/map-field shape :color f)`.
  ```
  [shape f]
  (map-field shape :color f))

(defn get-field
  ```
  Look up a single field on a shape. If the field does not exist, this will return `nil`.
  ```
  [shape field]
  (in (in shape :fields) field))

(defn transplant
  ```
  Shorthand for `(shape/with dest-shape field (shape/get-field source-shape field))`.
  ```
  [dest-shape field source-shape]
  (with dest-shape field (get-field source-shape field)))

(deftest "shapes"
  (def t (test (new :dimension :distance 1)
           {:fields {:distance 1}
            :tag <1>
            :type :dimension}))
  (def t (test (with t :color 2)
           {:fields {:color 2 :distance 1}
            :tag <1>
            :type :dimension}))
  (def t (test (map t |(* $ 2))
           {:fields {:color 4 :distance 2}
            :tag <1>
            :type :dimension}))
  (def t (test (map-distance t |(* $ 2))
           {:fields {:color 4 :distance 4}
            :tag <1>
            :type :dimension}))
  (def t (test (map-color t |(* $ 2))
           {:fields {:color 8 :distance 4}
            :tag <1>
            :type :dimension}))
  )
