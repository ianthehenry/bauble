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
(use judge)
(use ./util)

(def- tag (gensym))

(defn new [type & kvs]
  (struct
    :type type
    :tag tag
    ;kvs))

(defn distance-2d [expr]
  {:type jlsl/type/vec2
   :tag tag
   :hoisted {}
   :fields {:distance expr}})

(defn distance-3d [expr]
  {:type jlsl/type/vec3
   :tag tag
   :hoisted {}
   :fields {:distance expr}})

(defn type [t] (t :type))

(defn- map-opt [f x] (if x (f x)))
(defn- map-struct [f t] (table/to-struct (tabseq [[k v] :pairs t] k (f v))))

(test (map-struct |(* $ 2) {:foo 1 :bar 2}) {:bar 4 :foo 2})

(defn is? [x] (and (struct? x) (= (x :tag) tag)))

(defn- same-value [k v1 v2]
  (assertf (= v1 v2) "something has gone horribly wrong: a hoisted variable %q has taken on distinct values:\n%q\n%q" k v1 v2)
  v1)

(defn- merge-hoisted-vars [k v1 v2] (merge-structs same-value [v1 v2]))

(defn merge [ts f]
  (def type (get-unique type ts (fn [types]
    (if (empty? types)
      (error "no shapes to combine")
      (error "cannot combine shapes with different dimensions")))))
  {:type type
   :tag tag
   :hoisted (merge-structs merge-hoisted-vars (map |(in $ :hoisted) ts))
   :fields (f (map |(in $ :fields) ts))})

(defn map [t f &opt type]
  {:type (or type (t :type))
   :tag tag
   # TODO: do we want to alter these? i honestly do not know.
   :hoisted (t :hoisted)
   :fields (map-struct f (t :fields))})

(defn with [t & new-kvs]
  {:type (t :type)
   :tag tag
   :hoisted (t :hoisted)
   :fields (struct ;(kvs (t :fields)) ;new-kvs)})

(defn hoist-all [t k dict]
  (def {:hoisted hoisted :fields fields :type type} t)
  {:type type
   :tag tag
   :hoisted (struct ;(kvs hoisted) k (merge-structs merge-hoisted-vars [(in (in t :hoisted) k {}) dict]))
   :fields fields})

(defn- map-key [t f key] (with t key (map-opt f (in (in t :fields) key))))

(defn map-distance [t f] (map-key t f :distance))
(defn map-color [t f] (map-key t f :color))

(defn get-field [t k] (in (in t :fields) k))
(defn get-hoisted-vars [t k] (in (in t :hoisted) k))

(deftest "shapes"
  (def t (test (distance-2d 1)
           {:fields {:distance 1}
            :hoisted {}
            :tag <1>
            :type [<2> vec [<3> float] 2]}))
  (def t (test (with t :color 2)
           {:fields {:color 2 :distance 1}
            :hoisted {}
            :tag <1>
            :type [<2> vec [<3> float] 2]}))
  (def t (test (map t |(* $ 2))
           {:fields {:color 4 :distance 2}
            :hoisted {}
            :tag <1>
            :type [<2> vec [<3> float] 2]}))
  (def t (test (map-distance t |(* $ 2))
           {:fields {:color 4 :distance 4}
            :hoisted {}
            :tag <1>
            :type [<2> vec [<3> float] 2]}))
  (def t (test (map-color t |(* $ 2))
           {:fields {:color 8 :distance 4}
            :hoisted {}
            :tag <1>
            :type [<2> vec [<3> float] 2]}))
  (def t (test (hoist-all t :color {'foo "value"})
           {:fields {:color 8 :distance 4}
            :hoisted {:color {foo "value"}}
            :tag <1>
            :type [<2> vec [<3> float] 2]}))
  (def t (test (hoist-all t :distance {'foo "value"})
           {:fields {:color 8 :distance 4}
            :hoisted {:color {foo "value"}
                      :distance {foo "value"}}
            :tag <1>
            :type [<2> vec [<3> float] 2]}))
  (def t (test (hoist-all t :color {'bar "value"})
           {:fields {:color 8 :distance 4}
            :hoisted {:color {bar "value" foo "value"}
                      :distance {foo "value"}}
            :tag <1>
            :type [<2> vec [<3> float] 2]}))
  )
