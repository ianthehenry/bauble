(defn math/sign [x]
  (cond
    (< x 0) -1
    (> x 0) 1
    0))

(def pi math/pi)
(def tau (* 2 pi))
(def tau/360 (/ pi 180))
(def pi/2 (/ pi 2))
(def pi/3 (/ pi 3))
(def pi/4 (/ pi 4))
(def pi/5 (/ pi 5))
(def pi/6 (/ pi 6))
(def pi/7 (/ pi 7))
(def pi/8 (/ pi 8))
(def pi/9 (/ pi 9))
(def pi/10 (/ pi 10))
(def pi/11 (/ pi 11))
(def pi/12 (/ pi 12))

(defn deg [x] (* tau/360 x))
(defn tau* [x] (* tau x))
(defn tau/ [x] (/ tau x))

(defn axis-vec [axis scale]
  (case axis
    :x [scale 0 0]
    :y [0 scale 0]
    :z [0 0 scale]
    :+x [scale 0 0]
    :+y [0 scale 0]
    :+z [0 0 scale]
    :-x [(- scale) 0 0]
    :-y [0 (- scale) 0]
    :-z [0 0 (- scale)]
    (error "unknown axis")))

(defn rgb [r g b]
  [(/ r 255) (/ g 255) (/ b 255)])

(defn hex-rgb [hex]
  (let [r (-> hex (band 0xff0000) (brshift 16))
        g (-> hex (band 0x00ff00) (brshift 8))
        b (-> hex (band 0x0000ff))]
    (rgb r g b)))

(defmacro reflex [combine shape & fs]
  (let [$shape (gensym)
        combine (if (tuple? combine) combine [combine])
        transformed (map (fn [f] (if (tuple? f) [;f $shape] [f $shape])) fs)]
    ~(let [,$shape ,shape]
      (,;combine ,$shape ,;transformed))))
