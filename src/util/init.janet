(use judge)
(use module)
(import pat)

(defmacro assertf [x & args]
  ~(as-macro ,assert ,x (,string/format ,;args)))

(defn get-unique [f ind on-error]
  (pat/match (distinct (map f ind))
    [unique] unique
    values (on-error values)))

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

(def- core/dyn dyn)
(defn dyn [dynvar & dflt]
  (assert (<= (length dflt) 1) "too many arguments")
  (or (core/dyn dynvar)
    (if (empty? dflt)
      (errorf "dynamic variable %q is not set" dynvar)
      (in dflt 0))))

# like defn, but docstring is required, it appears in the right place, and
# it doesn't implicitly include the arguments
(defmacro deffn [name args docstring & body]
  (assert (string? docstring) "docstring required")
  ~(def ,name ,docstring (fn ,name ,args ,;body)))

(defn- true-false [f list]
  (def {true trues false falses} (group-by |(truthy? (f $)) list))
  [(or trues @[]) (or falses @[])])

(defn defnamed-aux [def-flavor name params docstring & body]
  (assert (string? docstring) "docstring required")
  (def [named-params positional-params] (true-false keyword? params))
  (var named-params-ordered (seq [name :in (or named-params [])]
    (match (string/split ":" name)
      [short long] [(keyword (string/triml short "?")) (symbol long) (not (string/has-prefix? "?" short))]
      [short] (let [trimmed (string/triml short "?")]
        [(keyword trimmed) (symbol trimmed) (not (string/has-prefix? "?" short))])
      (errorf "invalid param %q" name))))
  (def named-variadic-index (find-index (fn [[k _]] (string/has-prefix? "&" k)) named-params-ordered))
  (def variadic-named-params-symbol
    (if named-variadic-index
      (symbol (string/triml (get-in named-params-ordered [named-variadic-index 1]) "&"))))
  (when named-variadic-index
    (array/remove named-params-ordered named-variadic-index))

  # even though this is a triple, from-pairs only looks at the first two arguments
  (def named-params (from-pairs named-params-ordered))
  (def required-named-params (tabseq [[k _ required?] :in named-params-ordered :when required?] k true))

  (defn parse-params [args]
    (var key nil)
    (var variadic-named-args (if named-variadic-index @{}))
    (def positional-args @[])
    (def named-args @{})
    (each arg args
      (if (keyword? arg)
        (if (nil? key)
          (set key arg)
          (errorf "%s: no value for %q" name key))
        (if (nil? key)
          (array/push positional-args arg)
          (do
            (if (has-key? named-params key)
              (if (has-key? named-args key)
                (errorf "%s: duplicate named argument %q" name key)
                (put named-args key arg))
              (if variadic-named-args
                (if (has-key? variadic-named-args key)
                  (errorf "%s: duplicate named argument %q" name key)
                  (put variadic-named-args key arg))
                (errorf "%s: unknown named argument %q" name key)))
            (set key nil)))))
    (assert (nil? key) (errorf "%s: no value for %q" name key))
    (eachk param-name required-named-params
      (unless (has-key? named-args param-name)
        (errorf "%s: missing named argument %q" name param-name)))
    (cond
      (< (length positional-args) (length positional-params))
        (errorf "%s: not enough arguments, missing %s"
          name (string/join (slice positional-params (length positional-args)) " "))
      (> (length positional-args) (length positional-params))
        (errorf "%s: too many arguments"name))

    [named-args ;(if variadic-named-args [variadic-named-args] []) ;positional-args])

  (def signature
    (string "(" name " "
    (string/join positional-params " ")
    ;(map (fn [[k v required?]]
      (if required?
        (string/format " %q %q" k v)
        (string/format " [%q %q]" k v)))
      named-params-ordered)
    (if variadic-named-params-symbol (string/format " :& %s]" variadic-named-params-symbol))
    ")"))

  (with-syms [$key $args]
    ~(,def-flavor ,name ,(string signature "\n\n" docstring) (fn ,name [& ,$args]
      (def [,named-params
            ,;(if variadic-named-params-symbol [variadic-named-params-symbol] [])
            ,;positional-params] (,parse-params ,$args))
      ,;body))))

(def defnamed :macro (partial defnamed-aux 'def))
(def defnamed- :macro (partial defnamed-aux 'def-))

(test-macro (defnamed foo [x] "docstring" (+ x 1))
  (def foo "(foo x)\n\ndocstring" (fn foo [& <1>] (def [@{} x] (@parse-params <1>)) (+ x 1))))

(test-macro (defnamed foo [x :y] "docstring" (+ x 1))
  (def foo "(foo x :y y)\n\ndocstring" (fn foo [& <1>] (def [@{:y y} x] (@parse-params <1>)) (+ x 1))))

(test-macro (defnamed foo [x :y:long] "docstring" (+ x 1))
  (def foo "(foo x :y long)\n\ndocstring" (fn foo [& <1>] (def [@{:y long} x] (@parse-params <1>)) (+ x 1))))

(test-macro (defnamed foo [x :y:long] "docstring" (+ x 1))
  (def foo "(foo x :y long)\n\ndocstring" (fn foo [& <1>] (def [@{:y long} x] (@parse-params <1>)) (+ x 1))))

(test-macro (defnamed foo [x :&fields] "docstring" (+ x 1))
  (def foo "(foo x :& fields])\n\ndocstring" (fn foo [& <1>] (def [@{} fields x] (@parse-params <1>)) (+ x 1))))

(test-macro (defnamed foo [x :?y:something] "docstring" (+ x 1))
  (def foo "(foo x [:y something])\n\ndocstring" (fn foo [& <1>] (def [@{:y something} x] (@parse-params <1>)) (+ x 1))))

(deftest "basic defnamed"
  (defnamed foo [x] "" (+ x 1))
  (test (foo 1) 2)

  (defnamed foo [:x] "" (+ x 1))
  (test (foo :x 1) 2)

  (defnamed foo [:x y] "" (- x y))
  (test (foo 1 :x 2) 1)
  (test (foo 2 :x 1) -1))

(deftest "duplicate param names"
  (defnamed foo [:x y x] "" x)
  (test (foo :x 1 2 3) 3))

(deftest "optional named params"
  (defnamed foo [:?x y] "" (+ (or x 0) y))
  (test (foo :x 1 2) 3)
  (test (foo 2) 2))

(deftest "no way to distinguish explicit nil from missing argument"
  (defnamed foo [:?x] "" x)
  (test (foo) nil)
  (test (foo :x nil) nil))

(deftest "variadic named arguments"
  (defnamed foo [:x :&extras] "" [x extras])
  (test (foo :x 1 :y 2) [1 @{:y 2}])
  (test-error (foo :x 1 :x 2) "foo: duplicate named argument :x")
  (test-error (foo :x 0 :y 1 :y 2) "foo: duplicate named argument :y"))

(deftest "optional and variadic named arguments"
  (defnamed foo [:?x :&extras] "" [x extras])
  (test (foo :x 1 :y 2) [1 @{:y 2}])
  (test (foo :y 2) [nil @{:y 2}])
  (test-error (foo :x 1 :x 2) "foo: duplicate named argument :x")
  (test-error (foo :x 0 :y 1 :y 2) "foo: duplicate named argument :y"))

(deftest "illegal"
  (defnamed foo [x] "" (+ x 1))
  (test-error (foo) "foo: not enough arguments, missing x")
  (test-error (foo 1 2) "foo: too many arguments")
  (test-error (foo :x 1) "foo: unknown named argument :x")

  (defnamed foo [x y z] "" (+ x 1))
  (test-error (foo 1) "foo: not enough arguments, missing y z")

  (defnamed foo [:x] "" (+ x 1))
  (test-error (foo) "foo: missing named argument :x")
  (test-error (foo 1) "foo: missing named argument :x")
  (test-error (foo :x 1 2) "foo: too many arguments")
  (test-error (foo :x 1 :x 2) "foo: duplicate named argument :x")
  (test-error (foo :x 1 :y 2) "foo: unknown named argument :y")
  (test-error (foo :y 2) "foo: unknown named argument :y"))
