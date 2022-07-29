(defn- float [n]
  (if (int? n) (string n ".0") (string n)))

(defn- vec3 [[x y z]]
  (string/format `vec3(%s, %s, %s)` (float x) (float y) (float z)))

(defn- vec2 [[x y]]
  (string/format `vec2(%s, %s)` (float x) (float y)))

(def- translate-proto @{
  :compile (fn [{:offset offset :expr expr} comp-state coord]
    (:compile expr comp-state (string/format "(%s - %s)" coord (vec3 offset))))
  })

(def- box-proto @{
  :compile (fn [{:size size :center center} comp-state coord]
    (:function comp-state :box "s3d_box"
      [coord (vec3 center) (vec3 size)]
      ["vec3 p" "vec3 center" "vec3 size"] `
      vec3 q = abs(p - center) - size;
      return length(max(q, 0.0)) + min(max(q.x, max(q.y, q.z)), 0.0);
      `))
  })

(def- sphere-proto @{
  :compile (fn [{:radius radius :center center} comp-state coord]
    (:function comp-state :sphere "s3d_sphere"
      [coord (vec3 center) (float radius)]
      ["vec3 p" "vec3 center" "float radius"] `
      return length(p - center) - radius;
      `))
  })

(def- line-proto @{
  :compile (fn [{:radius radius :start start :end end} comp-state coord]
    (:function comp-state :line "s3d_line"
      [coord (vec3 start) (vec3 end) (float radius)]
      ["vec3 p" "vec3 a" "vec3 b" "float r"] `
      vec3 pa = p - a, ba = b - a;
      float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0);
      return length(pa - ba * h) - r;
      `))
  })

(def- rotate-proto @{
  :compile (fn [{:x x :y y :z z :expr expr} comp-state coord]
    (let [cx (math/cos x)
          sx (math/sin x)
          cy (math/cos y)
          sy (math/sin y)
          cz (math/cos z)
          sz (math/sin z)]
      (:compile expr comp-state (string/format "(%s * mat3(%s, %s, %s, %s, %s, %s, %s, %s, %s))"
        coord
        (* cx cy) (- (* cx sy sz) (* sx cz)) (+ (* cx sy cz) (* sx sz))
        (* sx cy) (+ (* sx sy sz) (* cx cz)) (- (* sx sy cz) (* cx sz))
        (- sy)    (* cy sz)                  (* cy cz)
        ))))
  })

(def- rotate-x-proto @{
  :compile (fn [{:angle angle :expr expr} comp-state coord]
    (let [c (math/cos angle)
          s (math/sin angle)]
      (:compile expr comp-state (string/format "(%s * mat3(1, 0, 0, 0, %s, %s, 0, %s, %s))"
        coord (float c) (float (- s)) (float s) (float c)))))
  })

(def- rotate-y-proto @{
  :compile (fn [{:angle angle :expr expr} comp-state coord]
    (let [c (math/cos angle)
          s (math/sin angle)]
      (:compile expr comp-state (string/format "(%s * mat3(%s, 0, %s, 0, 1, 0, %s, 0, %s))"
        coord (float c) (float s) (float (- s)) (float c)))))
  })

(def- rotate-z-proto @{
  :compile (fn [{:angle angle :expr expr} comp-state coord]
    (let [c (math/cos angle)
          s (math/sin angle)]
      (:compile expr comp-state (string/format "(%s * mat3(%s, %s, 0, %s, %s, 0, 0, 0, 1))"
        coord (float c) (float (- s)) (float s) (float c)))))
  })

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

(def- mirror-proto @{
  :compile (fn [self comp-state coord]
    (:sdf-3d comp-state self "sym" coord (fn [coord]
      (def {:expr expr :axes axes} self)
      (string/format "%s.%s = abs(%s.%s); return %s;" coord axes coord axes (:compile expr comp-state coord))
      )))
  })

(def- reflect-proto @{
  :compile (fn [self comp-state coord]
    (:sdf-3d comp-state self "sym" coord (fn [coord]
      (def {:expr expr :axes axes} self)
      (string/format "%s.%s = -%s.%s; return %s;" coord axes coord axes (:compile expr comp-state coord))
      )))
  })

(def- union-proto @{
  :compile (fold-exprs "union"
    :fn-first |(string/format "float d = %s;" $)
    :fn-rest |(string/format "d = min(d, %s);" $)
    :return "d")
  })

(def- intersect-proto @{
  :compile (fold-exprs "intersect"
    :fn-first |(string/format "float d = %s;" $)
    :fn-rest |(string/format "d = max(d, %s);" $)
    :return "d")
  })

(def- subtract-proto @{
  :compile (fold-exprs "subtract"
    :fn-first |(string/format "float d = %s;" $)
    :fn-rest |(string/format "d = max(d, -%s);" $)
    :return "d")
  })

(def- smooth-union-proto @{
  :compile (fold-exprs "smooth_union"
    :preamble |(string/format "float b, h = 0.0, k = %s;" (float ($ :size)))
    :fn-first |(string/format "float a = %s;" $)
    :fn-rest |(string/format "b = %s; h=clamp(0.5 + 0.5 * (b - a) / k, 0.0, 1.0); a = mix(b, a, h) - k * h * (1.0 - h);" $)
    :return "a")
  })

(def- smooth-subtract-proto @{
  :compile (fold-exprs "smooth_subtract"
    :preamble |(string/format "float b, h = 0.0, k = %s;" (float ($ :size)))
    :fn-first |(string/format "float a = %s;" $)
    :fn-rest |(string/format "b = %s; h=clamp(0.5 - 0.5 * (a + b) / k, 0.0, 1.0); a = mix(a, -b, h) + k * h * (1.0 - h);" $)
    :return "a")
  })

(def- smooth-intersect-proto @{
  :compile (fold-exprs "smooth_intersect"
    :preamble |(string/format "float b, h = 0.0, k = %s;" (float ($ :size)))
    :fn-first |(string/format "float a = %s;" $)
    :fn-rest |(string/format "b = %s; h=clamp(0.5 - 0.5 * (b - a) / k, 0.0, 1.0); a = mix(b, a, h) + k * h * (1.0 - h);" $)
    :return "a")
  })

(def- unit-vec3 [1 1 1])
(def- zero-vec3 [0 0 0])

(defn box [&opt size center]
  (default size unit-vec3)
  (default center zero-vec3)
  (table/setproto @{:size size :center center} box-proto))

(defn line [start end &opt radius]
  (default radius 0)
  (table/setproto @{:start start :end end :radius radius} line-proto))

(defn sphere [&opt radius center]
  (default radius 1)
  (default center zero-vec3)
  (table/setproto @{:radius radius :center center} sphere-proto))

(defn translate [offset expr]
  (table/setproto @{:offset offset :expr expr} translate-proto))

(defn rotate-x [angle expr]
  (table/setproto @{:angle angle :expr expr} rotate-x-proto))

(defn rotate-y [angle expr]
  (table/setproto @{:angle angle :expr expr} rotate-y-proto))

(defn rotate-z [angle expr]
  (table/setproto @{:angle angle :expr expr} rotate-z-proto))

# this is dumb. should look at arguments as they come in
(defn rotate [expr &named x y z]
  (default x 0)
  (default y 0)
  (default z 0)
  (match [(= x 0) (= y 0) (= z 0)]
    [true true true] expr
    [false true true] (rotate-x x expr)
    [true false true] (rotate-y y expr)
    [true true false] (rotate-z z expr)
    (table/setproto @{:x x :y y :z z :expr expr} rotate-proto)))

(defn union [& exprs]
  (table/setproto @{:exprs exprs} union-proto))

(defn smooth-union [size & exprs]
  (table/setproto @{:size size :exprs exprs} smooth-union-proto))

(defn intersect [& exprs]
  (table/setproto @{:exprs exprs} intersect-proto))

(defn smooth-intersect [size & exprs]
  (table/setproto @{:size size :exprs exprs} smooth-intersect-proto))

(defn subtract [& exprs]
  (table/setproto @{:exprs exprs} subtract-proto))

(defn smooth-subtract [size & exprs]
  (table/setproto @{:size size :exprs exprs} smooth-subtract-proto))

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

  (def axes (buffer/new 3))
  (when x (buffer/push-string axes "x"))
  (when y (buffer/push-string axes "y"))
  (when z (buffer/push-string axes "z"))

  (when (nil? expr) (error "no expression"))

  [axes expr])

(defn mirror [& args]
  (def [axes expr] (get-axes-and-expr args))
  (table/setproto @{:axes axes :expr expr} mirror-proto))

(defn reflect [& args]
  (def [axes expr] (get-axes-and-expr args))
  (table/setproto @{:axes axes :expr expr} reflect-proto))

(def- *tau* (* 2 math/pi))

(defn tau [x] (* *tau* x))

(defn tau/ [x] (/ *tau* x))

# TODO: this is a terrible name for this
(defmacro reflex [combine expr & fs]
  (let [$expr (gensym)
        combine (if (tuple? combine) combine [combine])
        transformed (map (fn [f] (if (tuple? f) [;f $expr] [f $expr])) fs)]
    ~(let [,$expr ,expr]
      (,;combine ,$expr ,;transformed))))
