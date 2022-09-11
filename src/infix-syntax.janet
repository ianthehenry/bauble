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
  (var invoke-saved-args nil)
  (for i 0 (length xs)
    (when-let [[split-point invoke-args] (f (xs i) i)]
      (def up-to-now (slice xs start i))
      (if (nil? result)
        (if (= (length up-to-now) 1)
          (set result @[prefix (up-to-now 0)])
          (set result @[prefix up-to-now]))
        (if (and invoke-saved-args (> (length up-to-now) 1))
          (array/push result [saved up-to-now])
          (array/push result [saved ;up-to-now])))
      (set start (+ i 1))
      (set saved split-point)
      (set invoke-saved-args invoke-args)))
  (if (nil? result)
    xs
    (do
      (let [rest (slice xs start)]
        (if (and invoke-saved-args (> (length rest) 1))
          (array/push result [saved rest])
          (array/push result [saved ;rest])))
      (tuple/slice result))))

(defn- binop? [x]
  (case x
    '+ true
    '- true
    '/ true
    '* true
    false))

(defn- rewrite-pipe [invocation]
  (split-map invocation '-> (fn [form i]
    (cond
      (and (invocation? form)
           (= (length form) 2)
           (= (form 0) 'short-fn)
           (= (type (form 1)) :symbol))
        [(form 1) false]
      (and (> i 0) (binop? form)) [form true]
      nil))))

(defn- resolve-form [form]
  (if (invocation? form)
    (rewrite-pipe form)
    form))

(defn expand [form]
  (prewalk resolve-form form))

(defmacro pipe-syntax [& forms]
  ~(do ,;(map expand forms)))
