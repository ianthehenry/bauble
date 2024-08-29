# A shape is a collection of three things:
#
# 1. A dimension -- a shape is either 2D or 3D.
# 2. A set of fields -- these should probably be JLSL expressions, although the
#    module is technically agnostic to this. These might be distance fields,
#    color fields, or arbitrary user-defined fields. Although user-defined fields
#    aren't really well-supported yet.
# 3. A set of "hoisted variables." These are confusing, and I will try to explain
#    them in detail below:
#
# The idea is that we want to define some expensive expressions -- basically,
# our lighting calculations -- that might be used by multiple different shapes.
# You might have a sphere shaded with a certain light, and a box shaded with the
# same light, and if you're rendering their smooth union you don't want to
# cast that light ray twice in the region where both color fields contribute to
# the final result.
#
# So... we could just not care about this, if we had the simplest form of common
# subexpression elimination. If the JLSL compiler had the ability to perform that
# optimization pass, then we could just let both fields compute the same light and
# trust that it would be hoisted up automatically and we wouldn't have to think about
# it. (This is pretty hard, though, since in a smooth union we conditionally evaluate
# color fields only when they contribute anything to the final result. And you don't
# want to cast the light if *neither* shape contributes anything to the final color
# field, as that would change the behavior of the program... although that is
# equivalent to what we're doing here and it would still be nicer to have it as an
# implicit pass.)
#
# But... we don't. So instead we have to mark specific expressions as explicitly
# expensive, put them in variables, and then hoist those variables up to the very
# root of the color expression.
#
# And when you combine multiple shapes -- say with `union` or `smooth-union` -- we
# combine their hoisted variables.
#
# Now, the *right* thing to do would be to only hoist them up to the lowest common
# ancestor. But that would require, like, being smart, and doing smart things, and
# building a shape graph, and deferring computation of the "fields with their
# hoisted variables" until the entire shape was realized.
#
# This would be a reasonable thing to do in the future, but right now we just hoist
# everything up as far as we can. Because *in practice* we only use this for light
# calculations, and *in practice* you pretty much always use a global set of lights
# for the whole scene, this doesn't make a difference. But it *could*, and it's a
# reasonable avenue to explore in the future, especially if we want to do something
# like adding lights as explicit elements in a scene graph.
#
# A future alternative representation that I think would be better -- although a
# little more complicated -- would be to treat e.g. color fields as *thunks*
# that will be realized with the current set of lights, which would allow us to
# share lights across different parts of the scene graph (instead of the current
# dynamic variable approach).

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
