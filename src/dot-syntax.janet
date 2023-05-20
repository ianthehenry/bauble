(defn- expand-dots [sym]
  (if (string/find "." sym)
    (let [components (string/split "." sym)]
      (if (some empty? components)
        sym
        (let [[head & props] components]
          (reduce |~(. ,$0 ,(keyword $1)) (symbol head) props))))
    sym))

(defn expand [form]
  (prewalk |(if (symbol? $) (expand-dots $) $) form))

(defmacro dot-syntax [& forms]
  ~(do ,;(map expand forms)))
