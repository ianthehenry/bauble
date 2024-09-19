(use ./util)

# the SDF for a sphere of radius 4 centered around the origin
(defn sample [[x y z]]
  (- (math/sqrt (+ (* x x) (* y y) (* z z))) 4))

# the SDF for two interlocking tori of radius 4 centered around the origin
(defn sample [[x y z]]
  (def torus-one (do
    (def one (- (math/sqrt (+ (* x x) (* y y))) 3))
    (- (math/sqrt (+ (* one one) (* z z))) 1.5)
    ))

  (def x (- x 3))
  (def torus-two (do
    (def one (- (math/sqrt (+ (* x x) (* z z))) 3))
    (- (math/sqrt (+ (* one one) (* y y))) 1.5)
    ))

  (def r 0.4)
  (def h (max (min (+ 0.5 (* (/ (- torus-two torus-one) r) 0.5)) 1) 0))
  (- (lerp torus-two torus-one h) (* r h (- 1 h)))
  )

(defn sample [[x y z]]
  (def s (/ 2 3))
  (def sphere (- (math/sqrt (+ (* x x) (* y y) (* z z))) 14.5))
  (def gyroid (let [x (* x s) y (* y s) z (* z s)]
    (- (+ (* (math/cos x) (math/sin y)) (* (math/cos y) (math/sin z)) (* (math/cos z) (math/sin x))) -1)))

  (def r 0.25)
  (def h (max (min (- 0.5 (* (/ (- gyroid sphere) r) 0.5)) 1) 0))
  (+ (lerp gyroid sphere h) (* r h (- 1 h)))
  )
