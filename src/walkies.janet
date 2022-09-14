# copied from boot.janet and modified to preserve sourcemaps

(defn- with-sourcemap [source dest]
  (tuple/setmap dest ;(tuple/sourcemap source))
  dest)

(defn- walk-ind [f form]
  (def ret @[])
  (each x form (array/push ret (f x)))
  ret)

(defn- walk-dict [f form]
  (def ret @{})
  (loop [k :keys form]
    (put ret (f k) (f (in form k))))
  ret)

(defn walk
  ``Iterate over the values in ast and apply `f`
  to them. Collect the results in a data structure. If ast is not a
  table, struct, array, or tuple,
  returns form.``
  [f form]
  (case (type form)
    :table (walk-dict f form)
    :struct (table/to-struct (walk-dict f form))
    :array (walk-ind f form)
    :tuple (let [x (walk-ind f form)]
             (with-sourcemap form
               (if (= :parens (tuple/type form))
                 (tuple/slice x)
                 (tuple/brackets ;x))))
    form))

(defn postwalk
  ``Do a post-order traversal of a data structure and call `(f x)`
  on every visitation.``
  [f form]
  (f (walk (fn [x] (postwalk f x)) form)))

(defn prewalk
  "Similar to `postwalk`, but do pre-order traversal."
  [f form]
  (walk (fn [x] (prewalk f x)) (f form)))
