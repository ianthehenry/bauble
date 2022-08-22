# TODO: maybe better to compile this to a function called `.`,
# and let that be a function that takes care of the quoting
# *when necessary*? This feels like a bit of a weird hack.
(defn- expand-dots [sym]
  (if (string/find "." sym)
    (let [components (string/split "." sym)]
      (if (some empty? components)
        sym
        (let [[head & props] components]
          (reduce |~(. ,$0 ,(keyword $1)) (symbol head) props))))
    sym))

(defmacro resolve-dots [form]
  (prewalk |(if (symbol? $) (expand-dots $) $) form))
