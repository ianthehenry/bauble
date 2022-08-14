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
  (if (number? x) [x x x] x))

(defn rotate-x-matrix [angle]
  (let [c (math/cos angle)
        s (math/sin angle)]
    [1 0 0
     0 c (- s)
     0 s c]))

(defn rotate-y-matrix [angle]
  (let [c (math/cos angle)
        s (math/sin angle)]
    [c 0 s
     0 1 0
     (- s) 0 c]))

(defn rotate-z-matrix [angle]
  (let [c (math/cos angle)
        s (math/sin angle)]
    [c (- s) 0
     s c 0
     0 0 1]))

(defn mat3/make-identity []
  @[1 0 0 0 1 0 0 0 1])

# this could be C but whatever
(defn mat3/multiply! [a b]
  (def [a11 a12 a13 a21 a22 a23 a31 a32 a33] a)
  (def [b11 b12 b13 b21 b22 b23 b31 b32 b33] b)
  (set (a 0) (+ (* a11 b11) (* a12 b21) (* a13 b31)))
  (set (a 1) (+ (* a11 b12) (* a12 b22) (* a13 b32)))
  (set (a 2) (+ (* a11 b13) (* a12 b23) (* a13 b33)))
  (set (a 3) (+ (* a21 b11) (* a22 b21) (* a23 b31)))
  (set (a 4) (+ (* a21 b12) (* a22 b22) (* a23 b32)))
  (set (a 5) (+ (* a21 b13) (* a22 b23) (* a23 b33)))
  (set (a 6) (+ (* a31 b11) (* a32 b21) (* a33 b31)))
  (set (a 7) (+ (* a31 b12) (* a32 b22) (* a33 b32)))
  (set (a 8) (+ (* a31 b13) (* a32 b23) (* a33 b33))))
