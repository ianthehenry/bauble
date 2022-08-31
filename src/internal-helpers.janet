(defn vec3/+= [target other]
  (+= (target 0) (other 0))
  (+= (target 1) (other 1))
  (+= (target 2) (other 2)))

(defn vec3/*= [target other]
  (*= (target 0) (other 0))
  (*= (target 1) (other 1))
  (*= (target 2) (other 2)))

(defn vec3/same? [[a b c]]
  (and (= a b) (= b c)))

(defn idiv [a b]
  (math/floor (/ a b)))

(defn map3 [vec3 f]
  [(f (vec3 0)) (f (vec3 1)) (f (vec3 2))])

(def vec3/unit [1 1 1])
(def vec3/zero [0 0 0])

(defn to-vec3 [x]
  (if (number? x)
    [x x x]
    (do
      (assert (and (indexed? x) (= (length x) 3)) "type mismatch: need vec3")
      x)))

(def mat3/identity [1 0 0 0 1 0 0 0 1])
