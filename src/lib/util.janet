(use judge)

(defn get-env [env sym]
  (-> env
    (in sym)
    (in :value)))

(defn get-var [env sym]
  (-> env
    (in sym)
    (in :ref)
    (in 0)))

(defn set-var [env sym value]
  (-> env
    (in sym)
    (in :ref)
    (put 0 value))
  value)

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

(defn ptuple? [x]
  (and (tuple? x) (= (tuple/type x) :parens)))

(defn get-unique [f ind on-error]
  (def distincts (distinct (map f ind)))
  (case (length distincts)
    1 (in distincts 0)
    (on-error distincts)))

(defmacro assertf [x & args]
  ~(as-macro ,assert ,x (,string/format ,;args)))

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

