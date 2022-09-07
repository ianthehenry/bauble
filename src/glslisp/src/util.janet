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

(defn simple-vec? [expr]
  (and (indexed? expr) (all number? expr)))

(defmacro assertf [x & args]
  ~(assert ,x (string/format ,;args)))

(defn table/remove [t k]
  (let [r (t k)]
    (set (t k) nil)
    r))

(defn get-or-insert [t k f]
  (or (t k) (set (t k) (f))))

(defn get-or-error [t k]
  (or (t k) (errorf "%p not found" k)))

(defn bool [x] (if x true false))

(defn group-bool [f list]
  (def t (group-by |(bool (f $)) list))
  [(or (t true) []) (or (t false) [])])

(defn same-length? [a b]
  (= (length a) (length b)))

(defn zip-with2 [f a b]
  (def len (length a))
  (def result (array/new len))
  (for i 0 len
    (set (result i) (f (a i) (b i))))
  result)

(defn zip-with3 [f a b c]
  (def len (length a))
  (def result (array/new len))
  (for i 0 len
    (set (result i) (f (a i) (b i) (c i))))
  result)
