(use ./helpers)
(use ./internal-helpers)
(use ./axes)
(use ./flex-fn)
(import ./raw)

# --- primitives ---

(def-flexible-fn box
  [[size type/vec3] [round type/float 0]]
  {type/vec3 |(set-param size $)
   type/float |(set-param size [$ $ $])
   :r |(set-param round $)}
  (if (= round 0)
    (raw/box size)
    (raw/offset round (raw/box (map |(- $ round) size)))))

(def-flexible-fn sphere
  [[radius type/float]]
  {type/float |(set-param radius $)}
  (raw/sphere radius))

# TODO: is it weird that the height is double the thing you pass it? it seems weird.
# this is true of box as well, though.
(def-flexible-fn cylinder
  [[axis] [radius] [height] [round type/float 0]]
  {type/float |(set-first [radius height] $)
   type/axis |(set-param axis $)
   :r |(set-param round $)}
  (if (= round 0)
    (raw/cylinder axis radius height)
    (raw/offset round
      (raw/cylinder axis (- radius round) (- height round)))))

(def-flexible-fn torus
  [[axis] [major-radius] [minor-radius]]
  {type/float |(set-first [major-radius minor-radius] $)
   type/axis |(set-param axis $)}
  (raw/torus axis major-radius minor-radius))

(def-flexible-fn half-space [[axis] [offset type/float 0]]
  {type/signed-axis |(set-param axis $)
   type/axis |(set-param axis $)
   type/float |(set-param offset $)}
  (let [[sign axis] (split-signed-axis axis)]
    (if (= offset 0)
      (raw/half-space axis sign)
      (raw/translate (axis-vec axis offset) (raw/half-space axis sign)))))

(def-flexible-fn cone
  [[axis]
   [radius type/float]
   [height type/float]
   [round type/float 0]]
  {type/signed-axis |(set-param axis $)
   type/axis |(set-param axis $)
   type/float |(set-first [radius height] $)
   :r |(set-param round $)}
  (let [[sign axis] (split-signed-axis axis)
        upside-down (neg? height)
        height (* 2 sign (math/abs height))
        ]
    (def tip-offset (* round (/ height radius)))
    (if (= 0 round)
      (raw/cone axis radius height upside-down)
      (raw/offset round
        (raw/translate (axis-vec axis (+ (* (if upside-down -1 1) sign round) (if upside-down tip-offset 0)))
          (raw/cone
            axis
            (- radius round)
            (- height tip-offset)
            upside-down))))))

(def-flexible-fn line
  [[start type/vec3] [end type/vec3] [thickness type/float 0]]
  {type/vec3 |(set-first [start end] $)
   type/float |(set-param thickness $)}
  (if (= 0 thickness)
    (raw/line start end)
    (raw/offset thickness (raw/line start end))))

# --- basic shape combinators ---

# TODO: I don't love the name "offset"
(def-flexible-fn offset [[distance type/float] [shape type/3d]]
  {type/3d |(set-param shape $)
   type/float |(set-param distance $)}
  (if (= distance 0)
    shape
    (raw/offset distance shape)))

(def-flexible-fn onion [[thickness type/float] [shape type/3d]]
  {type/3d |(set-param shape $)
   type/float |(set-param thickness $)}
  (raw/onion thickness shape))

(def-flexible-fn morph [[from-shape] [to-shape] [weight type/float 0.5]]
  {type/3d |(set-first [from-shape to-shape] $)
   type/float |(set-param weight $)}
  (raw/morph weight from-shape to-shape))

(defn- check-limit [vec3]
  (each num vec3
    (unless (and (int? num) (pos? num))
      (error "tile: limit values must be positive integers")))
  vec3)

(def-flexible-fn tile [[offset type/vec3] [limit type/vec3 nil] [shape]]
  {type/3d |(set-param shape $)
   type/vec3 |(set-param offset $)
   type/float |(set-param offset [$ $ $])
   :limit |(set-param limit (check-limit (to-vec3 $)))}
  (if (nil? limit)
    (raw/tile shape offset limit)
    (raw/translate (map3 [0 1 2] |(if (even? (limit $)) (* -0.5 (offset $)) 0))
      (raw/tile shape offset limit))))

# --- fancy shape combinators ---

(def-flexible-fn union [(shapes @[]) [radius type/float 0]]
  {type/3d |(array/push shapes $)
   :r |(set-param radius $)}
  (case (math/sign radius)
    -1 (error "union: radius cannot be negative")
    0 (raw/union shapes)
    1 (raw/smooth-union radius shapes)))

(def-flexible-fn intersect [(shapes @[]) [radius type/float 0]]
  {type/3d |(array/push shapes $)
   :r |(set-param radius $)}
  (case (math/sign radius)
    -1 (error "intersect: radius cannot be negative")
    0 (raw/intersect shapes)
    1 (raw/smooth-intersect radius shapes)))

(def-flexible-fn subtract [(shapes @[]) [radius type/float 0]]
  {type/3d |(array/push shapes $)
   :r |(set-param radius $)}
  (case (math/sign radius)
    -1 (error "subtract: radius cannot be negative")
    0 (raw/subtract shapes)
    1 (raw/smooth-subtract radius shapes)))

(def-flexible-fn translate
  [(offset @[0 0 0]) [shape type/3d]]
  {type/3d |(set-param shape $)
   type/vec3 |(vec3/+= offset $)
   :x |(+= (offset 0) (typecheck type/float $))
   :y |(+= (offset 1) (typecheck type/float $))
   :z |(+= (offset 2) (typecheck type/float $))}
  (raw/translate offset shape))

(def move translate)

(def-flexible-fn rotate [(matrix (mat3/make-identity)) (scale 1) [shape]]
  {type/3d |(set-param shape $)
   :tau |(set scale tau)
   :pi |(set scale pi)
   :deg |(set scale tau/360)
   :x |(mat3/multiply! matrix (rotate-x-matrix (* scale (typecheck type/float $))))
   :y |(mat3/multiply! matrix (rotate-y-matrix (* scale (typecheck type/float $))))
   :z |(mat3/multiply! matrix (rotate-z-matrix (* scale (typecheck type/float $))))}
  (raw/transform matrix shape))

(defn rotate-tau [& args]
  (rotate :tau ;args))

(defn rotate-deg [& args]
  (rotate :deg ;args))

(defn rotate-pi [& args]
  (rotate :pi ;args))

(def-flexible-fn scale [(scale @[1 1 1]) [shape]]
  {type/3d |(set-param shape $)
   type/float |(vec3/*= scale [$ $ $])
   type/vec3 |(vec3/*= scale $)
   :x |(vec3/*= scale [(typecheck type/float $) 1 1])
   :y |(vec3/*= scale [1 (typecheck type/float $) 1])
   :z |(vec3/*= scale [1 1 (typecheck type/float $)])}
  (if (vec3/same? scale)
    (raw/scale shape (scale 0))
    (raw/stretch shape scale)))

(def-flexible-fn mirror [[x type/bool false] [y type/bool false] [z type/bool false] [shape]]
  {type/3d |(set-param shape $)
   :x |(set-param x true)
   :y |(set-param y true)
   :z |(set-param z true)}
  (if (not (or x y z)) shape
    (do
      (def axes (buffer/new 3))
      (when x (buffer/push-string axes "x"))
      (when y (buffer/push-string axes "y"))
      (when z (buffer/push-string axes "z"))
      (raw/mirror-axes shape axes))))

# TODO: duplicated code between mirror and reflect.
# could make a macro to define them both.
(def-flexible-fn reflect [[x type/bool false] [y type/bool false] [z type/bool false] [shape]]
  {type/3d |(set-param shape $)
   :x |(set-param x true)
   :y |(set-param y true)
   :z |(set-param z true)}
  (if (not (or x y z)) shape
    (do
      (def axes (buffer/new 3))
      (when x (buffer/push-string axes "x"))
      (when y (buffer/push-string axes "y"))
      (when z (buffer/push-string axes "z"))
      (raw/reflect-axes shape axes))))

# TODO: should probably support the negative versions as well?
(def-flexible-fn mirror-plane [[axes] [shape]]
  {type/3d |(set-param shape $)
   :xz |(set-param axes [:x :z]) :zx |(set-param axes [:z :x])
   :yz |(set-param axes [:y :z]) :zy |(set-param axes [:z :y])
   :xy |(set-param axes [:x :y]) :yx |(set-param axes [:y :x])}
  (raw/mirror-plane shape axes))

(def-flexible-fn mirror-space [[shape]]
  {type/3d |(set-param shape $)}
  (raw/mirror-space shape))

(def-flexible-fn symmetry [[shape]]
  {type/3d |(set-param shape $)}
  (raw/mirror-axes (raw/mirror-space shape) "xyz"))

# TODO: it's weird that mirror-plane takes :xz and this
# takes :y to mean basically the same thing. on the one
# hand mirror-plane is directional -- :xz and :zx are
# different -- but that's dumb and i don't even know
# why that is.
(def-flexible-fn flip [[axis] [shape]]
  {type/3d |(set-param shape $)
   type/axis |(set-param axis $)
   type/signed-axis |(set-param axis $)}
  (def [sign axis] (split-signed-axis axis))
  (def axes (transpose-other-axes axis))
  (raw/flip shape axes (if (neg? sign) (negate-other-axes axis))))

# --- surfacing ---

(def-flexible-fn blinn-phong
  [[color type/vec3]
   [shape type/3d raw/r3]
   [shine type/float 0.25]
   [gloss type/float 4]
   [ambient type/float 0.2]]
  {type/vec3 |(set-param color $)
   type/3d |(set-param shape $)
   :shine |(set-param shine $)
   :gloss |(set-param gloss $)
   :ambient |(set-param ambient $)}
  (raw/blinn-phong shape color shine gloss ambient))

(def color blinn-phong)

(def-flexible-fn flat-color
  [[color] [shape type/3d raw/r3]]
  {type/vec3 |(set-param color $)
   type/3d |(set-param shape $)}
  (raw/flat-color shape color))

(def-flexible-fn fresnel
  [[color type/vec3 [1 1 1]]
   [shape]
   [strength type/float 0.25]
   [exponent type/float 5]]
  {type/vec3 |(set-param color $)
   type/float |(set-param strength $)
   type/3d |(set-param shape $)
   :exponent |(set-param exponent $)}
  (raw/fresnel shape color strength exponent))

(def-flexible-fn cel
  [[color type/vec3]
   [shape type/3d raw/r3]
   [shine type/float 1]
   [gloss type/float 4]
   [ambient type/float 0.5]
   [steps type/float 1]]
  {type/vec3 |(set-param color $)
   type/3d |(set-param shape $)
   type/float |(set-param steps $)
   :steps |(set-param steps $)
   :shine |(set-param shine $)
   :gloss |(set-param gloss $)
   :ambient |(set-param ambient $)}
  (raw/cel shape color shine gloss ambient steps))

# TODO: I don't love the name "resurface"
(def-flexible-fn resurface
  [[shape type/3d] [color type/3d]]
  {type/3d |(set-first [shape color] $)
   :shape |(set-param shape $)
   :color |(set-param shape $)}
  (raw/resurface shape color))

# TODO: are these useful?

(defn red [& args]
  (color [0.9 0.1 0.1] ;args))

(defn green [& args]
  (color [0.1 0.9 0.1] ;args))

(defn blue [& args]
  (color [0.1 0.2 0.9] ;args))

(defn cyan [& args]
  (color [0.1 0.9 0.9] ;args))

(defn magenta [& args]
  (color [0.9 0.1 0.9] ;args))

(defn yellow [& args]
  (color [0.9 0.9 0.1] ;args))

(defn orange [& args]
  (color [1.0 0.3 0.1] ;args))

(defn white [& args]
  (color [0.9 0.9 0.9] ;args))

(defn gray [& args]
  (color [0.4 0.4 0.4] ;args))

(defn black [& args]
  (color [0.05 0.05 0.05] ;args))
