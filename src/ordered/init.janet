(use judge)
(use module)
(use ../util)

(def- core/put put)
(def- core/in in)
(def- core/has-key? has-key?)

(defmodule table
  (defn put [[ks vs indexes] k v]
    (assert (not= k nil) "nil cannot act as a key")
    (assert (not= v nil) "nil cannot act as a value")
    (assertf (not (core/has-key? indexes k)) "can't insert duplicate key %q" k)
    (core/put indexes k (length ks))
    (array/push ks k)
    (array/push vs v))

  (defn in [[ks vs indexes] k &opt dflt]
    (if-let [index (core/in indexes k)]
      (core/in vs index)
      dflt))

  (defn has-key? [[_ _ indexes] k]
    (core/has-key? indexes k))

  (defn pairs [[ks vs _]] (map |[$0 $1] ks vs))
  (defn kvs [[ks vs _]] (mapcat |[$0 $1] ks vs))
  (defn keys [[ks vs _]] ks)
  (defn values [[ks vs _]] vs)

  (defn new [& kvs]
    (def t [@[] @[] @{}])
    (each [k v] (partition 2 kvs)
      (put t k v))
    t))

(deftest "ordered-table"
  (def t (table/new))
  (table/put t 1 1)
  (table/put t 2 2)
  (table/put t 4 4)
  (table/put t 3 3)
  (table/put t 7 7)
  (table/put t 5 5)
  (table/put t 6 6)
  (table/put t 8 8)
  (test t
    [@[1 2 4 3 7 5 6 8]
     @[1 2 4 3 7 5 6 8]
     @{1 0 2 1 3 3 4 2 5 5 6 6 7 4 8 7}])
  (test (table/in t 7) 7)
  (test (table/in t 20 "default") "default")
  (test (table/keys t) @[1 2 4 3 7 5 6 8])
  (test (table/values t) @[1 2 4 3 7 5 6 8])
  (test (table/kvs t) @[1 1 2 2 4 4 3 3 7 7 5 5 6 6 8 8]))

(defmodule set
  (defn new [] [@[] @{}])
  (defn put [[els mask] v]
    (assert (not= v nil) "set cannot contain nil")
    (when (in mask v) (break))
    (array/push els v)
    (core/put mask v true))
  (defn in [[els mask] v]
    (core/in mask v false))
  (defn values [[els _]] els))

(deftest "ordered-set"
  (def t (set/new))
  (set/put t 1)
  (set/put t 2)
  (set/put t 4)
  (set/put t 3)
  (set/put t 7)
  (set/put t 5)
  (set/put t 6)
  (set/put t 8)
  (set/put t 1)
  (test t
    [@[1 2 4 3 7 5 6 8]
     @{1 true
       2 true
       3 true
       4 true
       5 true
       6 true
       7 true
       8 true}])
  (test (set/in t 7) true)
  (test (set/in t 20) false)
  (test (set/values t) @[1 2 4 3 7 5 6 8]))
