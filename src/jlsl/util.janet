(use judge)
(use module)
(import pat)

(defn unprefix [sym]
  (symbol/slice (drop-until |(= $ (chr "/")) sym) 1))

(test (unprefix 'foo/bar) bar)

(defmacro export [sym]
  ~(put (curenv) ',(unprefix sym) (table/setproto @{:private false} (dyn ',sym))))

(test-macro (export foo/bar)
  (put (curenv) (quote bar) (table/setproto @{:private false} (dyn (quote foo/bar)))))

(defmacro assertf [x & args]
  ~(as-macro ,assert ,x (,string/format ,;args)))

(defn get-unique [f ind on-error]
  (pat/match (distinct (map f ind))
    [unique] unique
    (on-error)))

(defn >> [f g] |(g (f ;$&)))
(defn << [f g] |(f (g ;$&)))

(defn ptuple? [x] (and (tuple? x) (= (tuple/type x) :parens)))
(defn btuple? [x] (and (tuple? x) (= (tuple/type x) :brackets)))

(test (btuple? [1 2 3]) false)
(test (btuple? '[1 2 3]) true)
(test (ptuple? [1 2 3]) true)
(test (ptuple? '[1 2 3]) false)

(defn contents= [a b]
  (and
    (= (length a) (length b))
    (all = a b)))

(defn find-last [pred ind &opt dflt]
  (var result dflt)
  (loop [i :down-to [(dec (length ind)) 0]]
    (def x (in ind i))
    (when (pred x)
      (set result x)
      (break)))
  result)

(test (find-last odd? [1 2 3 4]) 3)
(test (find-last odd? [2 4 6]) nil)
(test (find-last odd? [2 4 6] :none) :none)

# This is a bit like postwalk, except that it works
# with cyclic data structures.
#
# The callback is invoked with three arguments: the value
# being visited, a boolean indicated whether the node is
# currently being visited, and a stack of all values currently
# being visited (not including the current value).
#
# (The visited? boolean in the callback is redundant with the
# values on the stack.)
#
# A node will never be visited more than once, which has implications
# for nodes which are value-equal to other nodes. Duplicate nodes will
# never be visited again.
(def- core/walk walk)
(defn visit [structure f &opt walk]
  (default walk core/walk)
  (def visiting @{})
  (def visited @{})
  (def stack @[])

  (defn recur [node]
    (when (in visited node) (break nil))
    (def visiting? (truthy? (in visiting node)))
    (put visiting node true)
    (unless visiting?
      (array/push stack node)
      (walk recur node)
      (array/pop stack))
    (f node visiting? stack)
    (put visited node true)
    (put visiting node nil))
  (recur structure)
  nil)

(test-stdout (do (postwalk (fn [x] (pp x) x) [1 1 1]) nil) `
  1
  1
  1
  (1 1 1)
`)

(test-stdout (visit [1 1 1] (fn [x _ _] (pp x))) `
  1
  (1 1 1)
`)

# useful for invoking macros as regular functions
(defn call [f & args]
  (f ;args))

(defmacro get-or-put [t k v]
  (with-syms [$t $k $v]
    ~(let [,$t ,t ,$k ,k]
      (if (,has-key? ,$t ,$k)
        (,in ,$t ,$k)
        (let [,$v ,v]
          (,put ,$t ,$k ,$v)
          ,$v)))))

(test-macro (get-or-put @{} 1 2)
  (let [<1> @{} <2> 1]
    (if (@has-key? <1> <2>)
      (@in <1> <2>)
      (let [<3> 2]
        (@put <1> <2> <3>)
        <3>))))

(defmodule ref
  (defn new [& args]
    (case (length args)
      0 @[]
      1 @[(in args 0)]
      (error "too many arguments to ref/new")))
  (defn get [t &opt dflt] (if (empty? t) dflt (in t 0)))
  (defn set [t x] (put t 0 x))
  (defmacro get-or-put [t v]
    (with-syms [$t $v]
      ~(let [,$t ,t]
        (if (,empty? ,$t)
          (let [,$v ,v]
            (,put ,$t 0 ,$v)
            ,$v)
          (,in ,$t 0))))))

(test-macro (ref/get-or-put x y)
  (let [<1> x]
    (if (@empty? <1>)
      (let [<2> y]
        (@put <1> 0 <2>)
        <2>)
      (@in <1> 0))))

(defn zip [& xs]
  (assert (> (length xs) 0) "nothing to zip")
  (def len (length (in xs 0)))
  (assertf (all |(= len (length $)) xs) "zip length mismatch")
  (apply map tuple xs))

(defn tmap [& args]
  (tuple/slice (map ;args)))

(defn const [x] (fn [&] x))

(defmodule bimap
  (defn new [&opt proto]
    (if proto
      [(table/setproto @{} (in proto 0))
       (table/setproto @{} (in proto 1))]
       [@{} @{}]))
  (def- core/in in)
  (def- core/put put)
  (def- core/has-key? has-key?)
  (defn in [[forward _] key] (core/in forward key))
  (defn out [[_ backward] value] (core/in backward value))
  (defn put [[forward backward] k v]
    (core/put forward k v)
    (core/put backward v k)
    v)
  (defn has-key? [[forward _] k] (core/has-key? forward k))
  (defn has-value? [[_ backward] v] (core/has-key? backward v)))

(def- core/put put)
(def- core/in in)

(defmodule ordered-table
  (defn put [[ks vs indexes] k v]
    (assert (not= k nil) "nil cannot act as a key")
    (assert (not= v nil) "nil cannot act as a value")
    (assertf (not (has-key? indexes k)) "can't insert duplicate key %q" k)
    (core/put indexes k (length ks))
    (array/push ks k)
    (array/push vs v))
  (defn in [[ks vs indexes] k &opt dflt]
    (if-let [index (core/in indexes k)]
      (core/in vs index)
      dflt))

  (defn kvs [[ks vs _]] (mapcat |[$0 $1] ks vs))
  (defn keys [[ks vs _]] ks)
  (defn values [[ks vs _]] vs)

  (defn new [& kvs]
    (def t [@[] @[] @{}])
    (each [k v] (partition 2 kvs)
      (put t k v))
    t))

(deftest "ordered-table"
  (def t (ordered-table/new))
  (ordered-table/put t 1 1)
  (ordered-table/put t 2 2)
  (ordered-table/put t 4 4)
  (ordered-table/put t 3 3)
  (ordered-table/put t 7 7)
  (ordered-table/put t 5 5)
  (ordered-table/put t 6 6)
  (ordered-table/put t 8 8)
  (test t
    [@[1 2 4 3 7 5 6 8]
     @[1 2 4 3 7 5 6 8]
     @{1 0 2 1 3 3 4 2 5 5 6 6 7 4 8 7}])
  (test (ordered-table/in t 7) 7)
  (test (ordered-table/in t 20 "default") "default")
  (test (ordered-table/keys t) @[1 2 4 3 7 5 6 8])
  (test (ordered-table/values t) @[1 2 4 3 7 5 6 8])
  (test (ordered-table/kvs t) @[1 1 2 2 4 4 3 3 7 7 5 5 6 6 8 8]))

(defmodule ordered-set
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
  (def t (ordered-set/new))
  (ordered-set/put t 1)
  (ordered-set/put t 2)
  (ordered-set/put t 4)
  (ordered-set/put t 3)
  (ordered-set/put t 7)
  (ordered-set/put t 5)
  (ordered-set/put t 6)
  (ordered-set/put t 8)
  (ordered-set/put t 1)
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
  (test (ordered-set/in t 7) true)
  (test (ordered-set/in t 20) false)
  (test (ordered-set/values t) @[1 2 4 3 7 5 6 8]))

(def- core/dyn dyn)
(defn dyn [dynvar & dflt]
  (assert (<= (length dflt) 1) "too many arguments")
  (or (core/dyn dynvar)
    (if (empty? dflt)
      (errorf "dynamic variable %q is not set" dynvar)
      (in dflt 0))))
