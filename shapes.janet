(def- unit-vec3 [1 1 1])
(def- zero-vec3 [0 0 0])
(def- identity-matrix-3 [1 0 0 0 1 0 0 0 1])

(defn- float [n]
  (if (int? n) (string n ".0") (string n)))

(defn- vec3 [[x y z]]
  (string/format `vec3(%s, %s, %s)` (float x) (float y) (float z)))

(defn- vec2 [[x y]]
  (string/format `vec2(%s, %s)` (float x) (float y)))

(defn- mat3 [m]
  (string/format "mat3(%s, %s, %s, %s, %s, %s, %s, %s, %s)"
    (float (m 0)) (float (m 1)) (float (m 2))
    (float (m 3)) (float (m 4)) (float (m 5))
    (float (m 6)) (float (m 7)) (float (m 8))))

(defmacro- defconstructor [name proto args & body]
  (let [$proto (gensym)]
    ~(def ,name
      (let [,$proto ,proto]
        (fn ,args (table/setproto (do ,;body) ,$proto))))))

(defmacro- defconstructor- [name proto args & body]
  (let [$proto (gensym)]
    ~(def- ,name
      (let [,$proto ,proto]
        (fn ,args (table/setproto (do ,;body) ,$proto))))))

(defmacro- defcompiler [name self compile-body args & body]
  ~(defconstructor ,name
    @{:compile (fn [,self comp-state coord] ,compile-body)}
    ,args
    ,;body))

(defcompiler translate
  {:offset offset :expr expr} (:compile expr comp-state (string/format "(%s - %s)" coord (vec3 offset)))
  [offset expr] @{:offset offset :expr expr})

(defcompiler box
  {:size size :center center}
  (:function comp-state :box "s3d_box"
    [coord (vec3 center) (vec3 size)]
    ["vec3 p" "vec3 center" "vec3 size"] `
    vec3 q = abs(p - center) - size;
    return length(max(q, 0.0)) + min(max(q.x, max(q.y, q.z)), 0.0);
    `)
  [&opt size center]
  (default size unit-vec3)
  (default center zero-vec3)
  @{:size size :center center})

(defcompiler sphere
  {:radius radius :center center}
  (:function comp-state :sphere "s3d_sphere"
    [coord (vec3 center) (float radius)]
    ["vec3 p" "vec3 center" "float radius"] `
    return length(p - center) - radius;
    `)
  [&opt radius center]
  (default radius 1)
  (default center zero-vec3)
  @{:radius radius :center center})

(defcompiler line
  {:radius radius :start start :end end}
  (:function comp-state :line "s3d_line"
    [coord (vec3 start) (vec3 end) (float radius)]
    ["vec3 p" "vec3 a" "vec3 b" "float r"] `
    vec3 pa = p - a, ba = b - a;
    float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0);
    return length(pa - ba * h) - r;
    `)
  [start end &opt radius]
  (default radius 0)
  @{:start start :end end :radius radius})

(defn- rotate-x-matrix [angle]
  (let [c (math/cos angle)
        s (math/sin angle)]
    [1 0 0
     0 c (- s)
     0 s c]))

(defn- rotate-y-matrix [angle]
  (let [c (math/cos angle)
        s (math/sin angle)]
    [c 0 s
     0 1 0
     (- s) 0 c]))

(defn- rotate-z-matrix [angle]
  (let [c (math/cos angle)
        s (math/sin angle)]
    [c (- s) 0
     s c 0
     0 0 1]))

(defconstructor- transform
  @{:compile (fn [{:matrix matrix :expr expr} comp-state coord]
    (:compile expr comp-state (string/format "(%s * %s)" coord (mat3 matrix))))}
  [matrix expr] @{:matrix matrix :expr expr})

(defn rotate-x [angle expr]
  (transform (rotate-x-matrix angle) expr))

(defn rotate-y [angle expr]
  (transform (rotate-y-matrix angle) expr))

(defn rotate-z [angle expr]
  (transform (rotate-z-matrix angle) expr))

# our matrices are just flat tuples. we could do this in C but whatever.
(defn- matrix-multiply-3 [a b]
  (def [a11 a12 a13 a21 a22 a23 a31 a32 a33] a)
  (def [b11 b12 b13 b21 b22 b23 b31 b32 b33] b)
  [
  (+ (* a11 b11) (* a12 b21) (* a13 b31))
  (+ (* a11 b12) (* a12 b22) (* a13 b32))
  (+ (* a11 b13) (* a12 b23) (* a13 b33))
  (+ (* a21 b11) (* a22 b21) (* a23 b31))
  (+ (* a21 b12) (* a22 b22) (* a23 b32))
  (+ (* a21 b13) (* a22 b23) (* a23 b33))
  (+ (* a31 b11) (* a32 b21) (* a33 b31))
  (+ (* a31 b12) (* a32 b22) (* a33 b32))
  (+ (* a31 b13) (* a32 b23) (* a33 b33))
  ])

(defn rotate [& args]
  (var axis nil)
  (var matrix nil)
  (var expr nil)

  (defn assert-number [x]
    (if (number? x)
      x
      (error "rotation must be a number")))

  (defn incorporate (new-matrix)
    (if (nil? matrix)
      (set matrix new-matrix)
      (set matrix (matrix-multiply-3 matrix new-matrix)))
    (set axis nil))

  (defn axis? [arg]
    (case arg
      :x true
      :y true
      :z true
      false))

  (each arg args
    (case axis
      :x (incorporate (rotate-x-matrix (assert-number arg)))
      :y (incorporate (rotate-y-matrix (assert-number arg)))
      :z (incorporate (rotate-z-matrix (assert-number arg)))
      (if (axis? arg)
        (set axis arg)
        (do
          (unless (nil? expr)
            (error "multiple expressions"))
          (set expr arg)))))

  (unless (nil? axis) (error "no angle for rotation axis"))
  (when (nil? matrix) (set matrix identity-matrix-3))
  (when (nil? expr) (error "nothing to rotate"))

  (transform matrix expr))

(defn- fold-exprs [base-name &named preamble fn-first fn-rest postamble return]
  (default preamble (fn [_] ""))
  (default postamble (fn [_] ""))
  (fn [self comp-state coord]
    (def exprs (self :exprs))
    (if (= 1 (length exprs))
      (:compile (first exprs) comp-state coord)
      (:sdf-3d comp-state self base-name coord (fn [coord]
        (string/join [
          (preamble self)
          (fn-first (:compile (first exprs) comp-state coord))
          ;(->> exprs (drop 1) (map |(:compile $ comp-state coord)) (map fn-rest))
          (postamble self)
          (string/format "return %s;" return)
        ] "\n"))))))

(defn- get-axes-and-expr [args]
  (var x false)
  (var y false)
  (var z false)
  (var expr nil)

  (each arg args
    (match arg
      :x (if x (error "duplicate axis") (set x true))
      :y (if y (error "duplicate axis") (set y true))
      :z (if z (error "duplicate axis") (set z true))
      (if expr (error "multiple expressions") (set expr arg))))

  (when (not (or x y z)) (error "no axis"))

  (def axes (buffer/new 3))
  (when x (buffer/push-string axes "x"))
  (when y (buffer/push-string axes "y"))
  (when z (buffer/push-string axes "z"))

  (when (nil? expr) (error "no expression"))

  [axes expr])

(defcompiler mirror
  self
  (:sdf-3d comp-state self "mirror" coord (fn [coord]
    (def {:expr expr :axes axes} self)
    (string/format "%s.%s = abs(%s.%s); return %s;" coord axes coord axes (:compile expr comp-state coord))))
  [& args]
  (def [axes expr] (get-axes-and-expr args))
  @{:axes axes :expr expr})

(defcompiler reflect
  self
  (:sdf-3d comp-state self "sym" coord (fn [coord]
    (def {:expr expr :axes axes} self)
    (string/format "%s.%s = -%s.%s; return %s;" coord axes coord axes (:compile expr comp-state coord))))
  [& args]
  (def [axes expr] (get-axes-and-expr args))
  @{:axes axes :expr expr})

(defconstructor union
  @{:compile (fold-exprs "union"
      :fn-first |(string/format "float d = %s;" $)
      :fn-rest |(string/format "d = min(d, %s);" $)
      :return "d")}
  [& exprs] @{:exprs exprs})

(defconstructor intersect
  @{:compile (fold-exprs "intersect"
      :fn-first |(string/format "float d = %s;" $)
      :fn-rest |(string/format "d = max(d, %s);" $)
      :return "d")}
  [& exprs] @{:exprs exprs})

(defconstructor subtract
  @{:compile (fold-exprs "subtract"
      :fn-first |(string/format "float d = %s;" $)
      :fn-rest |(string/format "d = max(d, -%s);" $)
      :return "d")}
  [& exprs] @{:exprs exprs})

(defconstructor smooth-union
  @{:compile (fold-exprs "smooth_union"
      :preamble |(string/format "float b, h = 0.0, k = %s;" (float ($ :size)))
      :fn-first |(string/format "float a = %s;" $)
      :fn-rest |(string/format "b = %s; h=clamp(0.5 + 0.5 * (b - a) / k, 0.0, 1.0); a = mix(b, a, h) - k * h * (1.0 - h);" $)
      :return "a")}
  [size & exprs] @{:size size :exprs exprs})

(defconstructor smooth-subtract
  @{:compile (fold-exprs "smooth_subtract"
      :preamble |(string/format "float b, h = 0.0, k = %s;" (float ($ :size)))
      :fn-first |(string/format "float a = %s;" $)
      :fn-rest |(string/format "b = %s; h=clamp(0.5 - 0.5 * (a + b) / k, 0.0, 1.0); a = mix(a, -b, h) + k * h * (1.0 - h);" $)
      :return "a")}
  [size & exprs] @{:size size :exprs exprs})

(defconstructor smooth-intersect
  @{:compile (fold-exprs "smooth_intersect"
      :preamble |(string/format "float b, h = 0.0, k = %s;" (float ($ :size)))
      :fn-first |(string/format "float a = %s;" $)
      :fn-rest |(string/format "b = %s; h=clamp(0.5 - 0.5 * (b - a) / k, 0.0, 1.0); a = mix(b, a, h) + k * h * (1.0 - h);" $)
      :return "a")}
  [size & exprs] @{:size size :exprs exprs})

(def- TAU (* 2 math/pi))

(defn tau [x] (* TAU x))

(defn tau/ [x] (/ TAU x))

# TODO: this is a terrible name for this
(defmacro reflex [combine expr & fs]
  (let [$expr (gensym)
        combine (if (tuple? combine) combine [combine])
        transformed (map (fn [f] (if (tuple? f) [;f $expr] [f $expr])) fs)]
    ~(let [,$expr ,expr]
      (,;combine ,$expr ,;transformed))))
