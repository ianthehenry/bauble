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

(defn new [type & kvs]
  (struct
    :type type
    :tag tag
    :fields (struct ;kvs)))

(defn distance-2d [expr]
  {:type jlsl/type/vec2
   :tag tag
   :fields {:distance expr}})

(defn distance-3d [expr]
  {:type jlsl/type/vec3
   :tag tag
   :fields {:distance expr}})

(defn type [t] (t :type))

(defn- map-opt [f x] (if x (f x)))
(defn- map-struct [f t] (table/to-struct (tabseq [[k v] :pairs t] k (f v))))

(test (map-struct |(* $ 2) {:foo 1 :bar 2}) {:bar 4 :foo 2})

(defn is? [x] (and (struct? x) (= (x :tag) tag)))

(defn merge [ts f]
  (def type (get-unique type ts (fn [types]
    (if (empty? types)
      (error "no shapes to combine")
      (error "cannot combine shapes with different dimensions")))))
  {:type type
   :tag tag
   :fields (f (map |(in $ :fields) ts))})

(defn map [t f &opt type]
  {:type (or type (t :type))
   :tag tag
   :fields (map-struct f (t :fields))})

(defn with [t & new-kvs]
  {:type (t :type)
   :tag tag
   :fields (struct ;(kvs (t :fields)) ;new-kvs)})

(defn- map-key [t f key] (with t key (map-opt f (in (in t :fields) key))))

(defn map-distance [t f] (map-key t f :distance))
(defn map-color [t f] (map-key t f :color))

(defn get-field [t k] (in (in t :fields) k))

(defn transplant [field from to]
  (with to field (get-field from field)))

(deftest "shapes"
  (def t (test (distance-2d 1)
           {:fields {:distance 1}
            :tag <1>
            :type [<2> vec [<3> float] 2]}))
  (def t (test (with t :color 2)
           {:fields {:color 2 :distance 1}
            :tag <1>
            :type [<2> vec [<3> float] 2]}))
  (def t (test (map t |(* $ 2))
           {:fields {:color 4 :distance 2}
            :tag <1>
            :type [<2> vec [<3> float] 2]}))
  (def t (test (map-distance t |(* $ 2))
           {:fields {:color 4 :distance 4}
            :tag <1>
            :type [<2> vec [<3> float] 2]}))
  (def t (test (map-color t |(* $ 2))
           {:fields {:color 8 :distance 4}
            :tag <1>
            :type [<2> vec [<3> float] 2]}))
  )
