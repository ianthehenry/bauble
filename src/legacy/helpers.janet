(import ../glslisp/src/builtins :as generic)
(import ./axes)
(import ./globals)
(setdyn 'axis-vec (dyn 'axis-vec))

(defn id [x] x)

(def pi math/pi)
(def tau/2 pi)
(def tau (* 2 tau/2))
(def tau/360 (/ pi 180))

(defn falloff [l &opt r]
  (if (nil? r)
    (fn [l-] (falloff l- l))
    (generic/clamp
      (generic/- 1 (generic// (generic/distance globals/P l) r))
      0 1)))

(defn- make-ratio [x y]
  ~(def ,(symbol (string x "/" y)) (/ ,x ,y)))

(eval ~(upscope ,;(map |(make-ratio 'pi $) (range 2 13))))
(eval ~(upscope ,;(map |(make-ratio 'tau $) (range 3 13))))

(defn deg [x] (* tau/360 x))
(defn tau* [x] (* tau x))
(defn tau/ [x] (/ tau x))
(defn pi* [x] (* pi x))
(defn pi/ [x] (/ pi x))

(defn hex-rgb [hex]
  (let [r (-> hex (band 0xff0000) (brshift 16))
        g (-> hex (band 0x00ff00) (brshift 8))
        b (-> hex (band 0x0000ff))]
    [(/ r 0xff) (/ g 0xff) (/ b 0xff)]))

(defn- fork-helper [include-self args]
  (var expecting-join false)
  (var expecting-r false)
  (var join nil)
  (var r nil)
  (var shape nil)
  (def fs (if include-self @[id] @[]))
  (each arg args
    (cond
      expecting-join (do
        (set expecting-join false)
        (if (nil? join)
          (set join arg)
          (error ":join specified multiple times")))
      expecting-r (do
        (set expecting-r false)
        (if (nil? r)
          (set r arg)
          (error ":r specified multiple times")))
      (= arg :join) (set expecting-join true)
      (= arg :r) (set expecting-r true)
      (nil? shape) (set shape arg)
      (array/push fs arg)))

  (if expecting-join (error ":join requires an argument"))
  (if expecting-r (error ":r requires an argument"))
  (if (and join r)
    (error "cannot specify both :r and :join"))

  (default join (if r ~(union :r ,r) '(union)))
  (default shape (error "must specify an initial shape"))

  (let [$shape (gensym)
        join (if (tuple? join) join [join])
        transformed (map (fn [f] ~(-> ,$shape ,f)) fs)]
    ~(let [,$shape ,shape]
      (,;join ,;transformed))))

(defmacro fork [& args]
  (fork-helper false args))

(defmacro spoon [& args]
  (fork-helper true args))

(use ../glslisp/src/glsl-ports)
(def- sat 0.98)

(def red (hsv (/ 0 6) sat 1))
(def orange (hsv (/ 0.25 6) sat 1))
(def yellow (hsv (/ 1 6) sat 1))
(def green (hsv (/ 2 6) sat 1))
(def cyan (hsv (/ 3 6) sat 1))
(def sky (hsv (/ 3.5 6) sat 1))
(def blue (hsv (/ 4 6) sat 1))
(def purple (hsv (/ 4.5 6) sat 1))
(def magenta (hsv (/ 5 6) sat 1))
(def hot-pink (hsv (/ 5.5 6) sat 1))
(def white [1 1 1])
(def light-gray [0.75 0.75 0.75])
(def gray [0.5 0.5 0.5])
(def dark-gray [0.25 0.25 0.25])
(def black [0.03 0.03 0.03])
