(defn sign [x]
  (cond
    (< x 0) -1
    (> x 0) 1
    0))

(defn clamp [x lo hi]
  (cond
    (< x lo) lo
    (> x hi) hi
    x))

(defn step [edge x]
  (if (< x edge) 0 1))

(defn smoothstep [lo hi x]
  (def t (clamp (/ (- x lo) (- hi lo)) 0 1))
  (* t t (- 3 (* t 2))))

(defn distance [p1 p2]
  (var sum 0)
  (for i 0 (length p1)
    (def x (- (p2 i) (p1 i)))
    (+= sum (* x x)))
  (math/sqrt sum))

(defn dot [a b]
  (var sum 0)
  (for i 0 (length a)
    (+= sum (* (a i) (b i))))
  sum)

(defn mix [x y a]
  (+ (* x (- 1 a)) (* y a)))

(defn vec-min [v] (apply min v))
(defn vec-max [v] (apply max v))

(defn fract [x] (mod x 1))

(defn hsv [h s v]
  (def h (fract h))
  (def s (clamp s 0 1))
  (def v (clamp v 0 1))

  (def m (* v (- 1 s)))
  (def z (* (- v m) (- 1 (math/abs (- (mod (* h 6) 2) 1)))))
  (def h- (* h 6))
  (cond
    (< h- 1) [v (+ z m) m]
    (< h- 2) [(+ z m) v m]
    (< h- 3) [m v (+ z m)]
    (< h- 4) [m (+ z m) v]
    (< h- 5) [(+ z m) m v]
             [v m (+ z m)]))

# TODO: it's dumb that I define this so far away from the GLSL version that I define
(defn hsl [h s l]
  (def h (fract h))
  (def s (clamp s 0 1))
  (def l (clamp l 0 1))

  (def c (* s (- 1 (math/abs (- (* 2 l) 1)))))
  (def h- (* 6 h))
  (def x (* c (- 1 (math/abs (- (mod h- 2) 1)))))

  (def [r g b]
    (cond
      (< h- 1) [c x 0]
      (< h- 2) [x c 0]
      (< h- 3) [0 c x]
      (< h- 4) [0 x c]
      (< h- 5) [x 0 c]
               [c 0 x]))

  (def m (- l (* 0.5 c)))
  [(+ r m) (+ g m) (+ b m)])

(defn vec-length [v]
  (var sum 0)
  (each x v (+= sum (* x x)))
  (math/sqrt sum))

(defn normalize [v]
  (def len (vec-length v))
  (map |(/ $ len) v))

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

(defn mat3/multiply [a b]
  (def result (array/new 9))
  (def [a11 a12 a13 a21 a22 a23 a31 a32 a33] a)
  (def [b11 b12 b13 b21 b22 b23 b31 b32 b33] b)
  (set (result 0) (+ (* a11 b11) (* a12 b21) (* a13 b31)))
  (set (result 1) (+ (* a11 b12) (* a12 b22) (* a13 b32)))
  (set (result 2) (+ (* a11 b13) (* a12 b23) (* a13 b33)))
  (set (result 3) (+ (* a21 b11) (* a22 b21) (* a23 b31)))
  (set (result 4) (+ (* a21 b12) (* a22 b22) (* a23 b32)))
  (set (result 5) (+ (* a21 b13) (* a22 b23) (* a23 b33)))
  (set (result 6) (+ (* a31 b11) (* a32 b21) (* a33 b31)))
  (set (result 7) (+ (* a31 b12) (* a32 b22) (* a33 b32)))
  (set (result 8) (+ (* a31 b13) (* a32 b23) (* a33 b33)))
  result)
