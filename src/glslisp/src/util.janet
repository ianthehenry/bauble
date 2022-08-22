(defn not- [f & args]
  (not (f ;args)))

(defn second [x] (x 1))

(defn in? [x v]
  (def l (length v))
  (var found false)
  (var i 0)
  (while (and (< i l) (not found))
    (if (= (v i) x)
      (set found true)
      (++ i)))
  found)

(defn- application-? [expr]
  (and (not- empty? expr) (symbol? (first expr))))

(defn application? [expr]
  (and (indexed? expr) (application-? expr)))

(defn vec? [expr]
  (and (indexed? expr) (not- application-? expr)))

(defmacro assertf [x & args]
  ~(assert ,x (string/format ;args)))
