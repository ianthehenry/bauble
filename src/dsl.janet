(use ./helpers)
(use ./internal-helpers)
(use ./axes)
(use ./flex-fn)
(import ./raw)
(import ./light)
(import ./glslisp/src/builtins :as generic)

(defmacro- pivoting [form]
  ~(if (nil? pivot)
    ,form
    (raw/pivot shape pivot (fn [shape] ,form))))

# --- primitives ---

# TODO: okay, so, interesting. because i broke this into
# separate steps, :round actually gets evaluated twice
# here. ugh. gotta fix that...
(def-flexible-fn box [size [round 0]]
  {type/vec3 |(set-param size $)
   type/float |(set-param size [$ $ $])
   :r |(set-param round $ type/float)}
  (if (= round 0)
    (raw/box size)
    (raw/offset round (raw/box (generic/- size round)))))

(def-flexible-fn sphere [radius]
  {type/float |(set-param radius $)}
  (raw/sphere radius))

# TODO: the 2D revolution ellipsoid looks horrible, but only at certain aspect ratios.
# (def-flexible-fn ellipsoid [[size nil] [axis nil] [ellipse-size nil]]
#   {type/vec3 |(set-param size $)
#    type/vec2 |(set-param ellipse-size $)
#    type/axis |(set-param axis $)}
#   (if size
#     (do
#       (assert (nil? axis) "axis is only allowed for ellipsoid with two size parameters")
#       (assert (nil? ellipse-size) "duplicate size parameters")
#       (raw/ellipsoid size))
#     (do
#       (assert (not (nil? ellipse-size)) "missing size parameters")
#       (assert (not (nil? axis)) "two parameter ellipsoid requires an axis")
#       (assert (nil? size) "duplicate size parameters")
#       # TODO: this should really just be a revolution of an ellipse
#       (raw/ellipsoid2 axis ellipse-size))))

(def-flexible-fn ellipsoid [size]
  {type/vec3 |(set-param size $)}
  (raw/ellipsoid size))

# TODO: is it weird that the height is double the thing you pass it? it seems weird.
# this is true of box as well, though.
(def-flexible-fn cylinder [axis radius height [round 0]]
  {type/float |(set-first [radius height] $)
   type/axis |(set-param axis $)
   :r |(set-param round $ type/float)}
  (if (= round 0)
    (raw/cylinder axis radius height)
    # TODO: round interpolated multiple times
    (raw/offset round
      (raw/cylinder axis (generic/- radius round) (generic/- height round)))))

(def-flexible-fn torus [axis major-radius minor-radius]
  {type/float |(set-first [major-radius minor-radius] $)
   type/axis |(set-param axis $)}
  (raw/torus axis major-radius minor-radius))

(def-flexible-fn half-space [axis [offset 0]]
  {type/signed-axis |(set-param axis $)
   type/axis |(set-param axis $)
   type/float |(set-param offset $)}
  (let [[sign axis] (split-signed-axis axis)]
    (if (= offset 0)
      (raw/half-space axis sign)
      (raw/move
        (raw/half-space axis sign)
        (axis-vec axis offset)))))

(def-flexible-fn ground [[offset 0]]
  {type/float |(set-param offset $)}
  (raw/ground offset))

(def-flexible-fn cone [axis radius height [round 0]]
  {type/signed-axis |(set-param axis $)
   type/axis |(set-param axis $)
   type/float |(set-first [radius height] $)
   :r |(set-param round $ type/float)}
  (let [[sign axis] (split-signed-axis axis)
        upside-down (neg? sign)]
    (if (= 0 round)
      (raw/cone axis radius height upside-down)
      (raw/rounded-cone axis radius height round upside-down))))

(def-flexible-fn line [start end [thickness 0]]
  {type/vec3 |(set-first [start end] $)
   type/float |(set-param thickness $)}
  (if (= 0 thickness)
    (raw/line start end)
    (raw/offset thickness (raw/line start end))))

# --- shape combinators ---

(def-flexible-fn union [(shapes @[]) [round 0]]
  {type/3d |(array/push shapes $)
   :r |(set-param round $ type/float)}
  (if (= round 0)
    (raw/union shapes)
    (raw/smooth-union round shapes)))

(def-flexible-fn intersect [(shapes @[]) [round 0]]
  {type/3d |(array/push shapes $)
   :r |(set-param round $ type/float)}
  (if (= round 0)
    (raw/intersect shapes)
    (raw/smooth-intersect round shapes)))

(def-flexible-fn subtract [(shapes @[]) [round 0]]
  {type/3d |(array/push shapes $)
   :r |(set-param round $ type/float)}
  (if (= round 0)
    (raw/subtract shapes)
    (raw/smooth-subtract round shapes)))

# --- basic shape combinators ---

# TODO: I don't love the name "offset".
# can we get by with something like (distort (+ p 5))?
(def-flexible-fn offset [shape distance]
  {type/3d |(set-param shape $)
   type/float |(set-param distance $)}
  (if (= distance 0)
    shape
    (raw/offset distance shape)))

(def-flexible-fn onion [shape thickness]
  {type/3d |(set-param shape $)
   type/float |(set-param thickness $)}
  (raw/onion thickness shape))

(def-flexible-fn slow [shape rate]
  {type/3d |(set-param shape $)
   type/float |(set-param rate $)}
  (raw/slow shape rate))

(def-flexible-fn morph [from-shape to-shape [weight 0.5]]
  {type/3d |(set-first [from-shape to-shape] $)
   type/float |(set-param weight $)}
  (raw/morph weight from-shape to-shape))

(defn- check-limit [vec3]
  (each num vec3
    (unless (and (int? num) (pos? num))
      (errorf "tile:limit %p is not a positive integer" num)))
  vec3)

(def-flexible-fn tile [[shape nil] [f nil] offset [limit nil]]
  {type/3d |(set-param shape $)
   type/fn |(set-param f $)
   type/vec3 |(set-param offset $)
   type/float |(set-param offset [$ $ $])
   :limit |(->> $
      (typecheck :limit [type/float type/vec3])
      (to-vec3)
      (check-limit)
      (set-param limit))}
  (if (and (nil? shape) (nil? f))
    (error "tile requires either a shape or a function to generate a shape"))
  (raw/tile shape f offset limit))

(def-flexible-fn radial [[shape nil] [f nil] axis count [radius 0]]
  {type/3d |(set-param shape $)
   type/fn |(set-param f $)
   type/axis |(set-param axis $)
   type/float |(set-first [count radius] $)}
  (if (and (nil? shape) (nil? f))
    (error "radial requires either a shape or a function to generate a shape"))
  (raw/radial shape f (generic// (* 2 math/pi) count) radius axis))

(def-flexible-fn distort [shape expression]
  {type/3d |(set-param shape $)
   type/vec3 |(set-param expression $)}
  (raw/distort shape expression))

(def-flexible-fn move [shape (offset @[0 0 0])]
  {type/3d |(set-param shape $)
   type/vec3 |(generic/+= offset $)
   :x |(generic/+= (offset 0) (typecheck :x type/float $))
   :y |(generic/+= (offset 1) (typecheck :y type/float $))
   :z |(generic/+= (offset 2) (typecheck :z type/float $))}
  (raw/move shape offset))

(def-flexible-fn rotate [shape (matrix mat3/identity) (scale 1) [pivot nil]]
  {type/3d |(set-param shape $)
   :tau |(set scale tau)
   :pi |(set scale pi)
   :deg |(set scale tau/360)
   :x |(set matrix (generic/mat3/multiply matrix (generic/rotate-x-matrix (generic/* scale (typecheck :x type/float $)))))
   :y |(set matrix (generic/mat3/multiply matrix (generic/rotate-y-matrix (generic/* scale (typecheck :y type/float $)))))
   :z |(set matrix (generic/mat3/multiply matrix (generic/rotate-z-matrix (generic/* scale (typecheck :z type/float $)))))
   :pivot |(set-param pivot (typecheck :pivot type/vec3 $))}
  (pivoting (raw/transform shape matrix)))

(def-flexible-fn scale [shape (scale @[1 1 1]) [pivot nil]]
  {type/3d |(set-param shape $)
   type/float |(generic/*= scale $)
   type/vec3 |(generic/*= scale $)
   :x |(generic/*= scale [(typecheck :x type/float $) 1 1])
   :y |(generic/*= scale [1 (typecheck :y type/float $) 1])
   :z |(generic/*= scale [1 1 (typecheck :z type/float $)])
   :pivot |(set-param pivot (typecheck :pivot type/vec3 $))}
  (pivoting
    (if (vec3/same? scale)
      (raw/scale shape (scale 0))
      (raw/stretch shape scale))))

(defn- get-axes [x y z]
  (def axes (buffer/new 3))
  (when x (buffer/push-string axes "x"))
  (when y (buffer/push-string axes "y"))
  (when z (buffer/push-string axes "z"))
  axes)

(def-flexible-fn mirror [shape [r 0] [x false] [y false] [z false]]
  {type/3d |(set-param shape $)
   :r |(set-param r $ type/float)
   :x |(set-param x true)
   :y |(set-param y true)
   :z |(set-param z true)}
  (if (not (or x y z))
    shape
    (let [axes (get-axes x y z)]
      (if (= r 0)
        (raw/mirror shape axes)
        (raw/biased-sqrt shape r axes)))))

(def-flexible-fn reflect [shape [x false] [y false] [z false]]
  {type/3d |(set-param shape $)
   :x |(set-param x true)
   :y |(set-param y true)
   :z |(set-param z true)}
  (if (not (or x y z))
    shape
    (raw/reflect-axes shape (get-axes x y z))))

# TODO: should probably support the negative versions as well?
(def-flexible-fn mirror-plane [shape axes]
  {type/3d |(set-param shape $)
   :xz |(set-param axes [:x :z]) :zx |(set-param axes [:z :x])
   :yz |(set-param axes [:y :z]) :zy |(set-param axes [:z :y])
   :xy |(set-param axes [:x :y]) :yx |(set-param axes [:y :x])}
  (raw/mirror-plane shape axes))

(def-flexible-fn mirror-space [shape]
  {type/3d |(set-param shape $)}
  (raw/mirror-space shape))

(def-flexible-fn symmetry [shape]
  {type/3d |(set-param shape $)}
  (raw/mirror (raw/mirror-space shape) "xyz"))

# TODO: it's weird that mirror-plane takes :xz and this
# takes :y to mean basically the same thing. on the one
# hand mirror-plane is directional -- :xz and :zx are
# different -- but that's dumb and i don't even know
# why that is.
(def-flexible-fn flip [shape axis]
  {type/3d |(set-param shape $)
   type/axis |(set-param axis $)
   type/signed-axis |(set-param axis $)}
  (def [sign axis] (split-signed-axis axis))
  (def axes (transpose-other-axes axis))
  (raw/flip shape axes (if (neg? sign) (negate-other-axes axis))))

(def-flexible-fn twist [shape axis rate]
  {type/3d |(set-param shape $)
   type/axis |(set-param axis $)
   type/float |(set-param rate $)}
  (raw/twist shape axis rate))

(def-flexible-fn swirl [shape axis rate]
  {type/3d |(set-param shape $)
   type/axis |(set-param axis $)
   type/float |(set-param rate $)}
  (raw/swirl shape axis rate))

(def-flexible-fn bend [shape axis towards rate]
  {type/3d |(set-param shape $)
   type/axis |(set-first [axis towards] $)
   type/signed-axis |(set-first [axis towards] $)
   type/float |(set-param rate $)}
  (let [[sign1 axis] (split-signed-axis axis)
        [sign2 towards] (split-signed-axis towards)]
    (raw/bend shape axis towards (generic/* -1 rate sign1 sign2))))

# --- surfacing ---

(def-flexible-fn blinn-phong
  [[shape raw/r3] color [shine 0.25] [gloss 4]]
  {type/vec3 |(set-param color $)
   type/3d |(set-param shape $)
   :shine |(set-param shine $ type/float)
   :gloss |(set-param gloss $ type/float)}
  (raw/blinn-phong shape color shine gloss))

(def shade blinn-phong)

(def-flexible-fn fresnel
  [shape [color [1 1 1]] [strength 0.25] [exponent 5]]
  {type/vec3 |(set-param color $)
   type/float |(set-param strength $)
   type/3d |(set-param shape $)
   :exponent |(set-param exponent $ type/float)}
  (raw/fresnel shape color strength exponent))

# TODO: I don't love the name "resurface"
(def-flexible-fn resurface [shape color]
  {type/3d |(set-first [shape color] $)
   :shape |(set-param shape $ type/3d)
   :color |(set-param color $ type/3d)}
  (raw/resurface shape color))

(def-flexible-fn map-distance [shape function]
  {type/3d |(set-param shape $)
   type/fn |(set-param function $)}
  (raw/map-distance shape function))

(def-flexible-fn map-color [shape function]
  {type/3d |(set-param shape $)
   type/fn |(set-param function $)}
  (raw/map-color shape function))

(def-flexible-fn bound [shape boundary [threshold 1]]
  {type/3d |(set-first [shape boundary] $)
   :threshold |(set-param threshold (typecheck :threshold type/float $))}
  (raw/bound shape boundary threshold))

(def-flexible-fn bounded [shape f magnitude [threshold 1]]
  {type/3d |(set-param shape $)
   type/fn |(set-param f $)
   type/float |(set-param magnitude $)
   :threshold |(set-param threshold (typecheck :threshold type/float $))}
  (raw/bounded shape f magnitude threshold))

# TODO: need a better name than this. also, is this too specific?
# could this be an argument to offset or something instead?
# (offset 10 :bounded (perlin+ ))...? hmm. maybe something to think about.
(def-flexible-fn bounded-offset [shape magnitude offset-scale [threshold 1]]
  {type/3d |(set-param shape $)
   type/float |(set-first [magnitude offset-scale] $)
   :threshold |(set-param threshold (typecheck :threshold type/float $))}
  (raw/bounded shape (fn [$ m] (offset ~(* ,m ,offset-scale) shape)) magnitude threshold))

(def-flexible-fn pivot [shape pivot-point f]
  {type/3d |(set-param shape $)
   type/vec3 |(set-param pivot-point $)
   type/fn |(set-param f $)}
  (raw/pivot shape pivot-point f))

(defmacro color [shape & body]
  (if (empty? body)
    ~(,raw/flat-color ,shape)
    ~(,map-color ,shape (fn [c] ,;body))))

(def-flexible-fn light [[shape nil] position [color [1 1 1]] [brightness 1] [shadow nil]]
  {type/3d |(set-param shape $)
   type/vec3 |(set-param position $)
   :brightness |(set-param brightness (typecheck :brightness [type/float type/fn] $))
   :color |(set-param color (typecheck :color type/vec3 $))
   :shadow |(set-param shadow (typecheck :shadow [type/float type/bool] $))}
  (def light (light/point/new position color brightness shadow))
  (if (nil? shape)
    light
    (raw/apply-light shape light)))

(def-flexible-fn ambient [[shape nil] [color [1 1 1]] [brightness 0.05]]
  {type/3d |(set-param shape $)
   type/vec3 |(set-param color $)
   type/float |(set-param brightness $)}
  (def light (light/ambient/new color brightness))
  (if (nil? shape)
    light
    (raw/apply-light shape light)))

(def-flexible-fn illuminate [shape light]
  {type/3d |(set-param shape $)
   type/light |(set-param light $)}
  (raw/apply-light shape light))
