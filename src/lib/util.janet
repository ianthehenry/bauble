(use judge)
(import ../util :prefix "" :export true)
(import ../jlsl)

(defn find-index [pred? ast &opt start-index end-index]
  (default start-index 0)
  (default end-index (length ast))
  (var result nil)
  (for i start-index end-index
    (when (pred? (in ast i))
      (set result i)
      (break)))
  result)
(test (find-index odd? [1 2 3]) 0)
(test (find-index odd? [1 2 3] 0) 0)
(test (find-index odd? [1 2 3] 0 3) 0)
(test (find-index odd? [1 2 3] 1) 2)
(test (find-index odd? [2 2 2 2 2 3] 1 3) nil)

(defn string/find-last [str c]
  (var i (dec (length str)))
  (var result nil)
  (while (>= i 0)
    (when (= (in str i) c)
      (set result i)
      (break))
    (-- i))
  result)

(defn merge-structs [f structs]
  (def result @{})
  (each struct structs
    (eachp [k v] struct
      (put result k
        (if-let [old-v (in result k)]
          (f k old-v v)
          v))))
  (table/to-struct result))

(test (merge-structs |(+ $1 $2) [{:foo 1 :bar 2} {:bar 2 :baz 3}]) {:bar 4 :baz 3 :foo 1})

(defn typecheck [expr expected]
  (def expr (jlsl/coerce-expr expr))
  (def actual (jlsl/expr/type expr))
  (def expected (jlsl/type/coerce expected))
  (assertf (= actual expected)
    "type mismatch: expected %q, got %q"
    (jlsl/show-type expected)
    (jlsl/show-type actual))
  expr)

(defn typecheck? [expr expected] (if (nil? expr) expr (typecheck expr expected)))
