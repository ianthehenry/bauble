(defn- invocation? [form]
  (and (= (type form) :tuple) (= (tuple/type form) :parens)))

(defn- maybe-invoke [x]
  (if (function? x) (x) x))

# split that performs no allocation if
# it never encounters anything to split
(defn- split-map [xs prefix f]
  (var result nil)
  (var start 0)
  (var saved nil)
  (for i 0 (length xs)
    (when-let [split-point (f (xs i))]
      (def up-to-now (slice xs start i))
      (if (nil? result)
        (if (= (length up-to-now) 1)
          (set result @[prefix (up-to-now 0)])
          (set result @[prefix up-to-now]))
        (array/push result [saved ;up-to-now]))
      (set start (+ i 1))
      (set saved split-point)))
  (if (nil? result)
    xs
    (tuple/slice (array/push result [saved ;(slice xs start)]))))

(defn- rewrite-pipe [invocation]
  (split-map invocation '-> (fn [form]
    (if (and (invocation? form)
             (= (length form) 2)
             (= (form 0) 'short-fn)
             (= (type (form 1)) :symbol))
      (form 1)
      nil))))

(defn- resolve-form [form]
  (if (invocation? form)
    (rewrite-pipe form)
    form))

(defmacro pipe [& forms]
  ~(do
    ,;(map (fn [form] (prewalk resolve-form form)) forms)))
