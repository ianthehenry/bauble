(use ./helpers)
(use ./internal-helpers)
(use ./axes)
(use ./flex-fn)
(import ./raw)
(import ./glslisp/src/builtins :as generic)

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
      (raw/translate
        (raw/half-space axis sign)
        (axis-vec axis offset)))))

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

# TODO: we interpolate `offset` multiple times
# in the limit case in order to center it, which
# is probably not good
(def-flexible-fn tile [shape offset [limit nil]]
  {type/3d |(set-param shape $)
   type/vec3 |(set-param offset $)
   type/float |(set-param offset [$ $ $])
   :limit |(->> $
      (typecheck :limit [type/float type/vec3])
      (to-vec3)
      (check-limit)
      (set-param limit))}
  (if (nil? limit)
    (raw/tile shape offset limit)
    (raw/translate (raw/tile shape offset limit)
      (map3 [0 1 2] |(if (even? (limit $)) (* -0.5 (offset $)) 0)))))

(def-flexible-fn distort [shape expression]
  {type/3d |(set-param shape $)
   type/vec3 |(set-param expression $)}
  (raw/distort shape expression))

(def-flexible-fn translate [shape (offset @[0 0 0])]
  {type/3d |(set-param shape $)
   type/vec3 |(vec3/+= offset $)
   :x |(generic/+= (offset 0) (typecheck :x type/float $))
   :y |(generic/+= (offset 1) (typecheck :y type/float $))
   :z |(generic/+= (offset 2) (typecheck :z type/float $))}
  (raw/translate shape offset))

(def move translate)

(def-flexible-fn rotate [shape (matrix mat3/identity) (scale 1)]
  {type/3d |(set-param shape $)
   :tau |(set scale tau)
   :pi |(set scale pi)
   :deg |(set scale tau/360)
   :x |(set matrix (generic/mat3/multiply matrix (generic/rotate-x-matrix (generic/* scale (typecheck :x type/float $)))))
   :y |(set matrix (generic/mat3/multiply matrix (generic/rotate-y-matrix (generic/* scale (typecheck :y type/float $)))))
   :z |(set matrix (generic/mat3/multiply matrix (generic/rotate-z-matrix (generic/* scale (typecheck :z type/float $)))))}

  (raw/transform shape matrix))

(defn rotate-tau [& args]
  (rotate :tau ;args))

(defn rotate-deg [& args]
  (rotate :deg ;args))

(defn rotate-pi [& args]
  (rotate :pi ;args))

(def-flexible-fn scale [shape (scale @[1 1 1])]
  {type/3d |(set-param shape $)
   type/float |(generic/*= scale $)
   type/vec3 |(generic/*= scale $)
   :x |(generic/*= scale [(typecheck :x type/float $) 1 1])
   :y |(generic/*= scale [1 (typecheck :y type/float $) 1])
   :z |(generic/*= scale [1 1 (typecheck :z type/float $)])}
  (if (vec3/same? scale)
    (raw/scale shape (scale 0))
    (raw/stretch shape scale)))

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
    (raw/bend shape axis towards (* -1 rate sign1 sign2))))

# --- surfacing ---

(def-flexible-fn blinn-phong
  [[shape raw/r3] color [shine 0.25] [gloss 4] [ambient 0.2]]
  {type/vec3 |(set-param color $)
   type/3d |(set-param shape $)
   :shine |(set-param shine $ type/float)
   :gloss |(set-param gloss $ type/float)
   :ambient |(set-param ambient $ type/float)}
  (raw/blinn-phong shape color shine gloss ambient))

(def shade blinn-phong)

(def-flexible-fn flat-color [[shape raw/r3] color]
  {type/vec3 |(set-param color $)
   type/3d |(set-param shape $)}
  (raw/flat-color shape color))

(def-flexible-fn fresnel
  [shape [color [1 1 1]] [strength 0.25] [exponent 5]]
  {type/vec3 |(set-param color $)
   type/float |(set-param strength $)
   type/3d |(set-param shape $)
   :exponent |(set-param exponent $ type/float)}
  (raw/fresnel shape color strength exponent))

# TODO: typecheck that steps is an integer?
(def-flexible-fn cel
  [[shape raw/r3] color [shine 1] [gloss 4] [ambient 0.5] [steps 1]]
  {type/vec3 |(set-param color $)
   type/3d |(set-param shape $)
   type/float |(set-param steps $)
   :steps |(set-param steps $ type/float)
   :shine |(set-param shine $ type/float)
   :gloss |(set-param gloss $ type/float)
   :ambient |(set-param ambient $ type/float)}
  (raw/cel shape color shine gloss ambient steps))

# TODO: I don't love the name "resurface"
(def-flexible-fn resurface [shape color]
  {type/3d |(set-first [shape color] $)
   :shape |(set-param shape $ type/3d)
   :color |(set-param color $ type/3d)}
  (raw/resurface shape color))

# TODO: are these useful?

(defn red [& args]
  (blinn-phong [0.9 0.1 0.1] ;args))

(defn green [& args]
  (blinn-phong [0.1 0.9 0.1] ;args))

(defn blue [& args]
  (blinn-phong [0.1 0.2 0.9] ;args))

(defn cyan [& args]
  (blinn-phong [0.1 0.9 0.9] ;args))

(defn magenta [& args]
  (blinn-phong [0.9 0.1 0.9] ;args))

(defn yellow [& args]
  (blinn-phong [0.9 0.9 0.1] ;args))

(defn orange [& args]
  (blinn-phong [1.0 0.3 0.1] ;args))

(defn white [& args]
  (blinn-phong [0.9 0.9 0.9] ;args))

(defn gray [& args]
  (blinn-phong [0.4 0.4 0.4] ;args))

(defn black [& args]
  (blinn-phong [0.05 0.05 0.05] ;args))
