(def- unit-vec3 [1 1 1])
(def- zero-vec3 [0 0 0])

(defn- math/sign [x]
  (cond
    (< x 0) -1
    (> x 0) 1
    0))

(defn- vec3/+= [target other]
  (+= (target 0) (other 0))
  (+= (target 1) (other 1))
  (+= (target 2) (other 2)))

(defn- vec3/*= [target other]
  (*= (target 0) (other 0))
  (*= (target 1) (other 1))
  (*= (target 2) (other 2)))

(defn- vec3/same? [[a b c]]
  (and (= a b) (= b c)))

(defn- idiv [a b]
  (math/floor (/ a b)))

(defn- map3 [vec3 f]
  [(f (vec3 0)) (f (vec3 1)) (f (vec3 2))])

(defn- to-vec3 [x]
  (if (number? x) [x x x] x))

(defn- float [n]
  (if (int? n) (string n ".0") (string n)))

(defn- vec3 [[x y z]]
  (string/format `vec3(%s, %s, %s)` (float x) (float y) (float z)))

(defn- vec2 [[x y]]
  (string/format `vec2(%s, %s)` (float x) (float y)))

(defn- other-axes [axis]
  (case axis
    :x [:y :z]
    :y [:x :z]
    :z [:x :y]
    (error "unknown axis")))

(defn- string-of-axis [axis]
  (case axis
    :x "x"
    :y "y"
    :z "z"
    (error "unknown axis")))

(defn- string-of-axes [args]
  (def result (buffer/new (length args)))
  (each axis args
    (buffer/push-string result (string-of-axis axis)))
  result)

(defn- split-signed-axis [arg]
  (case arg
    :-x [-1 :x] :x [1 :x] :+x [1 :x]
    :-y [-1 :y] :y [1 :y] :+y [1 :y]
    :-z [-1 :z] :z [1 :z] :+z [1 :z]
    (error "unknown signed axis")))

(defn- mat3 [m]
  (string/format "mat3(%s, %s, %s, %s, %s, %s, %s, %s, %s)"
    (float (m 0)) (float (m 1)) (float (m 2))
    (float (m 3)) (float (m 4)) (float (m 5))
    (float (m 6)) (float (m 7)) (float (m 8))))

(defn- arg-kvps [args]
  (mapcat |[(keyword $) $] args))

(defmacro- def-constructor- [name args proto]
  (let [$proto (gensym)]
    ~(def- ,name
      (let [,$proto ,proto]
        (fn ,args (struct/with-proto ,$proto ,;(arg-kvps args)))))))

(defmacro- def-primitive- [name args compile-body]
  ~(def-constructor- ,name ,args
    {:compile (fn [self comp-state coord] (let [,(struct ;(arg-kvps args)) self] ,compile-body))
     :surface (fn [self comp-state coord] "0.5 * (1.0 + normal)")}))

# an input-operator can only change the input coordinates
(defmacro- def-input-operator- [name args alter-fn]
  ~(def-constructor- ,name ,args
    (let [alter ,alter-fn]
      {:compile (fn [self comp-state coord] (:compile (self :shape) comp-state (alter self comp-state coord)))
       :surface (fn [self comp-state coord] (:surface (self :shape) comp-state (alter self comp-state coord)))})))

(defmacro- def-operator- [name args compile-body]
  ~(def-constructor- ,name ,args
    {:compile (fn [self comp-state coord] (let [,(struct ;(arg-kvps args)) self] ,compile-body))
     :surface (fn [self comp-state coord] (:surface (self :shape) comp-state coord))}))

(defmacro- def-surfacer- [name args surface-body]
  ~(def-constructor- ,name ,args
    {:compile (fn [self comp-state coord] (:compile (self :shape) comp-state coord))
     :surface (fn [self comp-state coord] (let [,(struct ;(arg-kvps args)) self] ,surface-body))}))

(def-input-operator- new-translate [offset shape]
  (fn [{:offset offset} comp-state coord]
    (string/format "(%s - %s)" coord (vec3 offset))))

(def-operator- new-offset [amount shape]
  (string/format "(%s - %s)" (:compile shape comp-state coord) (float amount)))

(def-operator- new-onion [thickness shape]
  (string/format "(abs(%s) - %s)" (:compile shape comp-state coord) (float thickness)))

# TODO: "amount" is interpolated multiple times here
(def-operator- new-scale [shape amount]
  (string/format "(%s * %s)"
    (:compile shape comp-state (string/format "(%s / %s)" coord (float amount)))
    (float amount)))

# TODO: "amount" is interpolated multiple times here
(def-operator- new-stretch [shape amount]
  (string/format "(%s * abs(min3(%s)))"
    (:compile shape comp-state (string/format "(%s / %s)" coord (vec3 amount)))
    (vec3 amount)))

(def-primitive- new-r3 [] "0.0")
(def- r3 (new-r3))

(def-primitive- new-box [size]
  (:function comp-state "float" :box "s3d_box"
    [coord (vec3 size)]
    ["vec3 p" "vec3 size"] `
    vec3 q = abs(p) - size;
    return length(max(q, 0.0)) + min(max(q.x, max(q.y, q.z)), 0.0);
    `))

(def-primitive- new-cylinder [axis radius height]
  (:function comp-state "float" [:cylinder axis] "s3d_cylinder"
    [coord (float radius) (float height)]
    ["vec3 p" "float radius" "float height"]
    (string/format `
    vec2 d = abs(vec2(length(p.%s), p.%s)) - vec2(radius, height);
    return min(max(d.x, d.y), 0.0) + length(max(d, 0.0));
    ` (string-of-axes (other-axes axis)) (string-of-axis axis))))

(def-primitive- new-cone [axis radius height upside-down]
  (:function comp-state "float" [:cone axis] "s3d_cone"
    [coord (float radius) (float height)]
    ["vec3 p" "float radius" "float height"]
    (string `
    vec2 q = vec2(radius, height);
    vec2 w = vec2(length(p.`(string-of-axes (other-axes axis))`), `(if upside-down "" "height - ")`p.`(string-of-axis axis)`);
    vec2 a = w - q * clamp(dot(w, q) / dot(q, q), 0.0, 1.0);
    vec2 b = w - q * vec2(clamp(w.x / q.x, 0.0, 1.0), 1.0);
    float k = sign(q.y);
    float d = min(dot(a, a), dot(b, b));
    float s = max(k * (w.x * q.y - w.y * q.x), k * (w.y - q.y));
    return sqrt(d) * sign(s);
    `)))

(def-primitive- new-sphere [radius]
  (string "(length("coord") - "(float radius)")"))

(def-primitive- new-half-space [axis sign]
  (string (if (neg? sign) "" "-") coord "." (string-of-axes [axis])))

(def-primitive- new-line [start end]
  (:function comp-state "float" :line "s3d_line"
    [coord (vec3 start) (vec3 end)]
    ["vec3 p" "vec3 a" "vec3 b"] `
    vec3 pa = p - a, ba = b - a;
    float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0);
    return length(pa - ba * h);
    `))

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

(def-input-operator- new-transform [matrix shape]
  (fn [{:matrix matrix} comp-state coord] (string/format "(%s * %s)" coord (mat3 matrix))))

(defn mat3/make-identity []
  @[1 0 0 0 1 0 0 0 1])

# this could be C but whatever
(defn- mat3/multiply! [a b]
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

(defn- fold-shapes [base-name &named preamble fn-first fn-rest postamble type extra-args extra-params return]
  (default preamble (fn [_] ""))
  (default postamble (fn [_] ""))
  (default type "float")
  (default extra-args [])
  (default extra-params [])
  (fn [self comp-state coord]
    (defn proxy [shape]
      {:compile (fn [_] (:compile shape comp-state "p"))
       :surface (fn [_] (:surface shape comp-state "p"))})
    (def shapes (self :shapes))
    (:function comp-state type [self base-name] base-name
      [coord ;extra-args]
      ["vec3 p" ;extra-params]
      (string/join [
        (preamble self)
        (fn-first (proxy (first shapes)))
        ;(->> shapes (drop 1) (map |(fn-rest (proxy $))))
        (postamble self)
        (string/format "return %s;" return)
        ] "\n"))))

(defn- get-axes-and-shape [args]
  (var x false)
  (var y false)
  (var z false)
  (var shape nil)

  (each arg args
    (match arg
      :x (if x (error "duplicate axis") (set x true))
      :y (if y (error "duplicate axis") (set y true))
      :z (if z (error "duplicate axis") (set z true))
      (if shape (error "multiple expressions") (set shape arg))))

  (when (not (or x y z)) (error "no axis"))

  (def axes (buffer/new 3))
  (when x (buffer/push-string axes "x"))
  (when y (buffer/push-string axes "y"))
  (when z (buffer/push-string axes "z"))

  (when (nil? shape) (error "no expression"))

  [axes shape])

(def-input-operator- new-mirror-axes [shape axes]
  (fn [{:axes axes} comp-state coord]
    (if (= (length axes) 3)
      (string `abs(`coord`)`)
      (:function comp-state "vec3" [:abs axes] (string "abs_" axes)
        [coord]
        ["vec3 p"]
        (string `p.`axes` = abs(p.`axes`); return p;`)))))

(def-input-operator- new-mirror-plane [shape axes]
  (fn [{:axes axes} comp-state coord]
    (def [axis1 axis2] axes)
    (defn select [axis]
      (cond
        (= axis1 axis) "hi"
        (= axis2 axis) "lo"
        (string `p.` axis)))
    (:function comp-state "vec3" [:sort axes] (string "sort_" axis1 axis2)
      [coord]
      ["vec3 p"]
      (string `
        float lo = min(p.`axis1`, p.`axis2`);
        float hi = max(p.`axis1`, p.`axis2`);
        return vec3(`(select :x)`, `(select :y)`, `(select :z)`);
        `))))

(def-input-operator- new-mirror-space [shape]
  (fn [self comp-state coord]
    (:function comp-state "vec3" :sort-xyz "sort_xyz"
      [coord]
      ["vec3 p"]
      `
      float smallest = min3(p);
      float largest = max3(p);
      float middlest =
        p.x > smallest && p.x < largest ? p.x :
        p.y > smallest && p.y < largest ? p.y :
        p.z;
      return vec3(smallest, middlest, largest);
      `)))

(defn- transpose-other-axes [axis]
  (case axis
    :x "xzy"
    :y "zyx"
    :z "yxz"
    (error "unknown axis")))

(defn- negate-other-axes [axis]
  (case axis
    :x [1 -1 -1]
    :y [-1 1 -1]
    :z [-1 -1 1]
    (error "unknown axis")))

(def-input-operator- new-flip [shape axes signs]
  (fn [{:axes axes :signs signs} comp-state coord]
    (if (nil? signs)
      (string coord "." axes)
      (string `(`coord`.`axes` * `(vec3 signs)`)`))))

(def-input-operator- new-reflect-axes [shape axes]
  (fn [{:axes axes} comp-state coord]
    (if (= (length axes) 3)
      (string `-` coord)
      (:function comp-state "vec3" [:neg axes] (string "neg_" axes)
        [coord]
        ["vec3 p"]
        (string `p.`axes` = -p.`axes`; return p;`)))))

# TODO: all of the union/intersect/subtract operators evaluate
# every surface in their collection, even when it will not
# contribute at all to the result. we could optimize this by
# only evaluating the "nearest" surface, or surfaces with
# a blend coefficient greater than 0.
(def-constructor- new-union [shapes]
  {:compile (fold-shapes "union"
      :fn-first |(string/format "float d = %s;" (:compile $))
      :fn-rest |(string/format "d = min(d, %s);" (:compile $))
      :return "d")
  :surface (fold-shapes "union_surface"
    :type "vec3"
    :extra-args ["world_p" "camera" "normal" "light_intensities"]
    :extra-params ["vec3 world_p" "vec3 camera" "vec3 normal" "float light_intensities[3]"]
    :fn-first |(string/format "float d = %s; float d2; vec3 color = %s;" (:compile $) (:surface $))
    :fn-rest |(string/format "d2 = %s; if (d2 < d) { d = d2; color = %s; }" (:compile $) (:surface $))
    :return "color")})

(def-constructor- new-intersect [shapes]
  {:compile (fold-shapes "intersect"
      :fn-first |(string/format "float d = %s;" (:compile $))
      :fn-rest |(string/format "d = max(d, %s);" (:compile $))
      :return "d")
  :surface (fold-shapes "intersect_surface"
    :type "vec3"
    :extra-args ["world_p" "camera" "normal" "light_intensities"]
    :extra-params ["vec3 world_p" "vec3 camera" "vec3 normal" "float light_intensities[3]"]
    :fn-first |(string/format "float d = %s; float d2; vec3 color = %s;" (:compile $) (:surface $))
    :fn-rest |(string/format "d2 = %s; if (d2 > d) { d = d2; color = %s; }" (:compile $) (:surface $))
    :return "color")})

(def-constructor- new-subtract [shapes]
  {:compile (fold-shapes "subtract"
      :fn-first |(string/format "float d = %s;" (:compile $))
      :fn-rest |(string/format "d = max(d, -%s);" (:compile $))
      :return "d")
  :surface (fold-shapes "subtract_surface"
    :type "vec3"
    :extra-args ["world_p" "camera" "normal" "light_intensities"]
    :extra-params ["vec3 world_p" "vec3 camera" "vec3 normal" "float light_intensities[3]"]
    :fn-first |(string/format "float d = %s; float d2; vec3 color = %s;" (:compile $) (:surface $))
    :fn-rest |(string/format "d2 = -%s; if (d2 >= d) { d = d2; color = %s; }" (:compile $) (:surface $))
    :return "color")})

(def-constructor- new-smooth-union [size shapes]
  {:compile (fold-shapes "smooth_union"
      :preamble |(string/format "float b, h = 0.0, k = %s;" (float ($ :size)))
      :fn-first |(string/format "float a = %s;" (:compile $))
      :fn-rest |(string/format `
        b = %s;
        h = clamp(0.5 + 0.5 * (b - a) / k, 0.0, 1.0);
        a = mix(b, a, h) - k * h * (1.0 - h);
        ` (:compile $))
      :return "a")
    :surface (fold-shapes "smooth_union_surface"
      :type "vec3"
      :extra-args ["world_p" "camera" "normal" "light_intensities"]
      :extra-params ["vec3 world_p" "vec3 camera" "vec3 normal" "float light_intensities[3]"]
      :preamble |(string/format "float b, h, k = %s;" (float ($ :size)))
      :fn-first |(string/format "float a = %s; vec3 color = %s;" (:compile $) (:surface $))
      :fn-rest |(string/format `
        b = %s;
        h = clamp(0.5 + 0.5 * (b - a) / k, 0.0, 1.0);
        a = mix(b, a, h) - k * h * (1.0 - h);
        color = mix(%s, color, h);
        ` (:compile $) (:surface $))
      :return "color")})

(def-constructor- new-smooth-intersect [size shapes]
  {:compile (fold-shapes "smooth_intersect"
      :preamble |(string/format "float b, h, k = %s;" (float ($ :size)))
      :fn-first |(string/format "float a = %s;" (:compile $))
      :fn-rest |(string/format `
        b = %s;
        h = clamp(0.5 - 0.5 * (b - a) / k, 0.0, 1.0);
        a = mix(b, a, h) + k * h * (1.0 - h);
        ` (:compile $))
      :return "a")
    :surface (fold-shapes "smooth_intersect_surface"
      :type "vec3"
      :extra-args ["world_p" "camera" "normal" "light_intensities"]
      :extra-params ["vec3 world_p" "vec3 camera" "vec3 normal" "float light_intensities[3]"]
      :preamble |(string/format "float b, h, k = %s;" (float ($ :size)))
      :fn-first |(string/format "float a = %s; vec3 color = %s;" (:compile $) (:surface $))
      :fn-rest |(string/format `
        b = %s;
        h = clamp(0.5 - 0.5 * (b - a) / k, 0.0, 1.0);
        a = mix(b, a, h) + k * h * (1.0 - h);
        color = mix(%s, color, h);
        ` (:compile $) (:surface $))
      :return "color")})

(def-constructor- new-smooth-subtract [size shapes]
  {:compile (fold-shapes "smooth_subtract"
      :preamble |(string/format "float b, h, k = %s;" (float ($ :size)))
      :fn-first |(string/format "float a = %s;" (:compile $))
      :fn-rest |(string/format `
        b = %s;
        h = clamp(0.5 - 0.5 * (a + b) / k, 0.0, 1.0);
        a = mix(a, -b, h) + k * h * (1.0 - h);
        ` (:compile $))
      :return "a")
    :surface (fold-shapes "smooth_subtract_surface"
      :type "vec3"
      :extra-args ["world_p" "camera" "normal" "light_intensities"]
      :extra-params ["vec3 world_p" "vec3 camera" "vec3 normal" "float light_intensities[3]"]
      :preamble |(string/format "float b, h, k = %s;" (float ($ :size)))
      :fn-first |(string/format "float a = %s; vec3 color = %s;" (:compile $) (:surface $))
      :fn-rest |(string/format `
        b = %s;
        h = clamp(0.5 - 0.5 * (a + b) / k, 0.0, 1.0);
        a = mix(a, -b, h) + k * h * (1.0 - h);
        color = mix(color, %s, h);
        ` (:compile $) (:surface $))
      :return "color")})

(def-input-operator- new-tile [shape offset limit]
  (fn [{:offset offset :shape shape :limit limit} comp-state coord]
    (if (nil? limit)
      (:function comp-state "vec3" :tile "tile"
        [coord (vec3 offset)]
        ["vec3 p" "vec3 offset"]
        "return mod(p + 0.5 * offset, offset) - 0.5 * offset;")
      (let [min-limit (map3 limit |(idiv (- $ 1) 2))
            max-limit (map3 limit |(idiv $ 2))]
        (:function comp-state "vec3" :tile-limit "tile"
          [coord (vec3 offset) (vec3 min-limit) (vec3 max-limit)]
          ["vec3 p" "vec3 offset" "vec3 min_limit" "vec3 max_limit"]
          "return p - offset * clamp(round(p / offset), -min_limit, max_limit);")))))

(def-constructor- new-morph [weight shape1 shape2]
  {:compile (fn [self comp-state coord]
    (def {:weight weight :shape1 shape1 :shape2 shape2} self)
    (:function comp-state "float" [self :distance] "morph"
      [coord (float weight)]
      ["vec3 p" "float weight"]
      (string/format "return mix(%s, %s, weight);"
        (:compile shape1 comp-state "p")
        (:compile shape2 comp-state "p"))))
    :surface (fn [self comp-state coord]
      (def {:weight weight :shape1 shape1 :shape2 shape2} self)
      (:function comp-state "vec3" [self :surface] "morph_surface"
        [coord "world_p" "camera" "normal" "light_intensities" (float weight)]
        ["vec3 p" "vec3 world_p" "vec3 camera" "vec3 normal" "float light_intensities[3]" "float weight"]
        (string/format "return mix(%s, %s, weight);"
          (:surface shape1 comp-state "p")
          (:surface shape2 comp-state "p"))))})

(def-surfacer- new-flat-color [shape color]
  (vec3 color))

(def-surfacer- new-blinn-phong [shape color shine gloss ambient]
  (:function comp-state "vec3" :blinn-phong "blinn_phong"
    [coord "world_p" "camera" "normal" "light_intensities" (vec3 color) (float shine) (float (* gloss gloss)) (float ambient)]
    ["vec3 p" "vec3 world_p" "vec3 camera" "vec3 normal" "float light_intensities[3]" "vec3 color" "float shine" "float gloss" "float ambient"]
    `
    vec3 view_dir = normalize(camera - world_p);
    vec3 result = color * ambient;

    for (int i = 0; i < lights.length(); i++) {
      vec3 light_color = lights[i].color * light_intensities[i];
      vec3 light_dir = normalize(lights[i].position - world_p);
      vec3 halfway_dir = normalize(light_dir + view_dir);

      float specular_strength = shine * pow(max(dot(normal, halfway_dir), 0.0), gloss);
      float diffuse = max(0.0, dot(normal, light_dir));
      result += light_color * specular_strength;
      result += color * diffuse * light_color;
    }
    return result;
    `))

(def-surfacer- new-cel [shape color shine gloss ambient steps]
  (:function comp-state "vec3" :cel "cel"
    [coord "world_p" "camera" "normal" "light_intensities" (vec3 color) (float shine) (float (* gloss gloss)) (float ambient) (float steps)]
    ["vec3 p" "vec3 world_p" "vec3 camera" "vec3 normal" "float light_intensities[3]" "vec3 color" "float shine" "float gloss" "float ambient" "float steps"]
    `
    vec3 view_dir = normalize(camera - world_p);
    vec3 light = vec3(0.0);

    for (int i = 0; i < lights.length(); i++) {
      vec3 light_color = lights[i].color * light_intensities[i];
      vec3 light_dir = normalize(lights[i].position - world_p);
      vec3 halfway_dir = normalize(light_dir + view_dir);

      float specular_strength = shine * pow(max(dot(normal, halfway_dir), 0.0), gloss);
      float diffuse = max(0.0, dot(normal, light_dir));
      light += light_color * (diffuse + specular_strength);
    }
    return color * (ambient + (1.0 - ambient) * round(light * steps) / steps);
    `))

(def-surfacer- new-fresnel [shape color strength exponent]
  (:function comp-state "vec3" self "fresnel"
    [coord "world_p" "camera" "normal" "light_intensities" (vec3 color) (float strength) (float exponent)]
    ["vec3 p" "vec3 world_p" "vec3 camera" "vec3 normal" "float light_intensities[3]" "vec3 color" "float strength" "float exponent"]
    (string `
    vec3 view_dir = normalize(camera - world_p);
    float fresnel = pow(1.0 - dot(normal, view_dir), exponent);
    return `(:surface shape comp-state coord)` + color * strength * fresnel;
    `)))

(def-constructor- new-resurface [shape color]
  {:compile (fn [{:shape shape} comp-state coord] (:compile shape comp-state coord))
    :surface (fn [{:color color} comp-state coord] (:surface color comp-state coord))})

# ----- general helpers ------

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

# TODO: this is a terrible name for this
(defmacro reflex [combine shape & fs]
  (let [$shape (gensym)
        combine (if (tuple? combine) combine [combine])
        transformed (map (fn [f] (if (tuple? f) [;f $shape] [f $shape])) fs)]
    ~(let [,$shape ,shape]
      (,;combine ,$shape ,;transformed))))

# --- pipe macro ---

(defn- invocation? [form]
  (and (= (type form) :tuple) (= (tuple/type form) :parens)))

(defn- maybe-invoke [x]
  (if (function? x) (x) x))

# split that performs no allocation if
# it never encounters anything to split
(defn- split-map [xs prefix f]
  (var result nil)
  (var start 0)
  (var saved nil)
  (for i 0 (length xs)
    (when-let [split-point (f (xs i))]
      (def up-to-now (slice xs start i))
      (if (nil? result)
        (if (= (length up-to-now) 1)
          (set result @[prefix [maybe-invoke (up-to-now 0)]])
          (set result @[prefix up-to-now]))
        (array/push result [saved ;up-to-now]))
      (set start (+ i 1))
      (set saved split-point)))
  (if (nil? result)
    xs
    (tuple/slice (array/push result [saved ;(slice xs start)]))))

(defn- rewrite-pipe [invocation]
  (split-map invocation '-> (fn [form]
    (if (and (invocation? form)
             (= (length form) 2)
             (= (form 0) 'short-fn)
             (= (type (form 1)) :symbol))
      (form 1)
      nil))))

(defn- resolve-form [form]
  (if (invocation? form)
    (rewrite-pipe form)
    form))

(defmacro pipe [& forms]
  ~(do
    ,;(map (fn [form] (prewalk resolve-form form)) forms)))

# this fancy function definition stuff could be in its own helper module

(def- type/bool @"bool")
(def- type/vec2 @"vec2")
(def- type/vec3 @"vec3")
(def- type/vec4 @"vec4")
(def- type/float @"float")
(def- type/keyword @"keyword")
(def- type/3d @"3d-sdf")
(def- type/axis @"axis")
(def- type/signed-axis @"signed-axis")
(def- type/unknown @"unknown")
(def- unset (gensym))

(def- keyword-types {
  :x type/axis
  :y type/axis
  :z type/axis
  :+x type/signed-axis
  :+y type/signed-axis
  :+z type/signed-axis
  :-x type/signed-axis
  :-y type/signed-axis
  :-z type/signed-axis
  })

(defn- typeof [value]
  (case (type value)
    :number type/float
    :boolean type/bool
    :keyword (get keyword-types value type/unknown)
    :struct type/3d # TODO: obviously this is wrong once I add 2D SDFs
    :tuple (case (length value)
      2 type/vec2
      3 type/vec3
      4 type/vec4
      type/unknown)
    type/unknown))

(defn- typecheck [expected-type value]
  (if (nil? expected-type) value
    (let [actual-type (typeof value)]
      (if (= expected-type actual-type)
        value
        (errorf "%s: expected %s, got %s" (dyn :fn-name) expected-type actual-type)))))

(defmacro- def-param [name type]
  ~(def ,name @{:type ,type :value ',unset :name ',name}))

(defn- set? [value]
  (not= value unset))

(defn- set-param [param value]
  (if (set? (param :value))
    (errorf "%s: %s specified multiple times" (dyn :fn-name) (param :name))
    (set (param :value) (typecheck (param :type) value))))

# the error message here only makes sense if this is
# used for handling positional arguments
(defn- set-first [params value]
  (prompt :break
    (each param params
      (when (not (set? (param :value)))
        (set-param param value)
        (return :break)))
    (errorf "%s: unexpected argument %p" (dyn :fn-name) value)))

(defn- get-param [param default-value]
  (let [value (param :value)]
    (if (set? value) value
      (if (set? default-value) default-value
        (errorf "%s: %s: missing required argument" (dyn :fn-name) (param :name))))))

(defn- handle-args [args spec]
  (var i 0)
  (var last-index (- (length args) 1))
  (while (<= i last-index)
    (def arg (args i))
    (var handled-as-keyword false)
    (when (= (type arg) :keyword)
      (when-let [dispatch (spec arg)]
        (set handled-as-keyword true)
        (if (= (function/max-arity dispatch) 0)
          (dispatch)
          (if (= i last-index)
            (errorf "%s: named argument %s without value" (dyn :fn-name) arg)
            (dispatch (args (++ i)))))))
    (def type (typeof arg))
    (unless handled-as-keyword
      (if-let [dispatch (spec type)]
        (dispatch arg)
        (errorf "%s: unexpected argument %p" (dyn :fn-name) arg)))
    (++ i)))

(defmacro- swap [f arg1 arg2]
  ~(,f ,arg2 ,arg1))

(defn get-strict [list index default-value]
  (if (< index (length list))
    (list index)
    default-value))

(defmacro- def-flexible-fn [fn-name bindings spec & body]
  (def param-defs
    (swap map bindings (fn [binding]
      (def [name arg] binding)
      (case (tuple/type binding)
        :parens ~(var ,name ,arg)
        :brackets ~(def-param ,name ,arg)))))
  (def get-bindings-defs
    (swap mapcat bindings (fn [binding]
      (if (= (tuple/type binding) :parens) []
        (let [[name type] binding
              default-value (get-strict binding 2 ~(quote ,unset))]
          ~(,name (get-param ,name ,default-value)))))))
  (def $args (gensym))
  ~(defn ,fn-name [& ,$args]
    (with-dyns [:fn-name ',fn-name]
      ,;param-defs
      (,handle-args ,$args ,spec)
      (let ,get-bindings-defs ,;body))))

# --- primitive shapes ---

(def-flexible-fn box
  [[size type/vec3] [round type/float 0]]
  {type/vec3 |(set-param size $)
   type/float |(set-param size [$ $ $])
   :r |(set-param round $)}
  (if (= round 0)
    (new-box size)
    (new-offset round (new-box (map |(- $ round) size)))))

(def-flexible-fn sphere
  [[radius type/float]]
  {type/float |(set-param radius $)}
  (new-sphere radius))

# TODO: is it weird that the height is double the thing you pass it? it seems weird.
# this is true of box as well, though.
(def-flexible-fn cylinder
  [[axis] [radius] [height] [round type/float 0]]
  {type/float |(set-first [radius height] $)
   type/axis |(set-param axis $)
   :r |(set-param round $)}
  (if (= round 0)
    (new-cylinder axis radius height)
    (new-offset round
      (new-cylinder axis (- radius round) (- height round)))))

(def-flexible-fn half-space [[axis] [offset type/float 0]]
  {type/signed-axis |(set-param axis $)
   type/axis |(set-param axis $)
   type/float |(set-param offset $)}
  (let [[sign axis] (split-signed-axis axis)]
    (if (= offset 0)
      (new-half-space axis sign)
      (new-translate (axis-vec axis offset) (new-half-space axis sign)))))

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
      (new-cone axis radius height upside-down)
      (new-offset round
        (new-translate (axis-vec axis (+ (* (if upside-down -1 1) sign round) (if upside-down tip-offset 0)))
          (new-cone
            axis
            (- radius round)
            (- height tip-offset)
            upside-down))))))

(def-flexible-fn line
  [[start type/vec3] [end type/vec3] [thickness type/float 0]]
  {type/vec3 |(set-first [start end] $)
   type/float |(set-param thickness $)}
  (if (= 0 thickness)
    (new-line start end)
    (new-offset thickness (new-line start end))))

# --- basic shape combinators ---

# TODO: I don't love the name "offset"
(def-flexible-fn offset [[distance type/float] [shape type/3d]]
  {type/3d |(set-param shape $)
   type/float |(set-param distance $)}
  (if (= distance 0)
    shape
    (new-offset distance shape)))

(def-flexible-fn onion [[thickness type/float] [shape type/3d]]
  {type/3d |(set-param shape $)
   type/float |(set-param thickness $)}
  (new-onion thickness shape))

(def-flexible-fn morph [[from-shape] [to-shape] [weight type/float 0.5]]
  {type/3d |(set-first [from-shape to-shape] $)
   type/float |(set-param weight $)}
  (new-morph weight from-shape to-shape))

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
    (new-tile shape offset limit)
    (new-translate (map3 [0 1 2] |(if (even? (limit $)) (* -0.5 (offset $)) 0))
      (new-tile shape offset limit))))

# --- fancy shape combinators ---

(def-flexible-fn union [(shapes @[]) [radius type/float 0]]
  {type/3d |(array/push shapes $)
   :r |(set-param radius $)}
  (case (math/sign radius)
    -1 (error "union: radius cannot be negative")
    0 (new-union shapes)
    1 (new-smooth-union radius shapes)))

(def-flexible-fn intersect [(shapes @[]) [radius type/float 0]]
  {type/3d |(array/push shapes $)
   :r |(set-param radius $)}
  (case (math/sign radius)
    -1 (error "intersect: radius cannot be negative")
    0 (new-intersect shapes)
    1 (new-smooth-intersect radius shapes)))

(def-flexible-fn subtract [(shapes @[]) [radius type/float 0]]
  {type/3d |(array/push shapes $)
   :r |(set-param radius $)}
  (case (math/sign radius)
    -1 (error "subtract: radius cannot be negative")
    0 (new-subtract shapes)
    1 (new-smooth-subtract radius shapes)))

(def-flexible-fn translate
  [(offset @[0 0 0]) [shape type/3d]]
  {type/3d |(set-param shape $)
   type/vec3 |(vec3/+= offset $)
   :x |(+= (offset 0) (typecheck type/float $))
   :y |(+= (offset 1) (typecheck type/float $))
   :z |(+= (offset 2) (typecheck type/float $))}
  (new-translate offset shape))

(def move translate)

(def-flexible-fn rotate [(matrix (mat3/make-identity)) (scale 1) [shape]]
  {type/3d |(set-param shape $)
   :tau |(set scale tau)
   :pi |(set scale pi)
   :deg |(set scale tau/360)
   :x |(mat3/multiply! matrix (rotate-x-matrix (* scale (typecheck type/float $))))
   :y |(mat3/multiply! matrix (rotate-y-matrix (* scale (typecheck type/float $))))
   :z |(mat3/multiply! matrix (rotate-z-matrix (* scale (typecheck type/float $))))}
  (new-transform matrix shape))

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
    (new-scale shape (scale 0))
    (new-stretch shape scale)))

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
      (new-mirror-axes shape axes))))

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
      (new-reflect-axes shape axes))))

# TODO: should probably support the negative versions as well?
(def-flexible-fn mirror-plane [[axes] [shape]]
  {type/3d |(set-param shape $)
   :xz |(set-param axes [:x :z]) :zx |(set-param axes [:z :x])
   :yz |(set-param axes [:y :z]) :zy |(set-param axes [:z :y])
   :xy |(set-param axes [:x :y]) :yx |(set-param axes [:y :x])}
  (new-mirror-plane shape axes))

(def-flexible-fn mirror-space [[shape]]
  {type/3d |(set-param shape $)}
  (new-mirror-space shape))

(def-flexible-fn symmetry [[shape]]
  {type/3d |(set-param shape $)}
  (new-mirror-axes (new-mirror-space shape) "xyz"))

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
  (new-flip shape axes (if (neg? sign) (negate-other-axes axis))))

# --- surfacing ---

(def-flexible-fn color
  [[color type/vec3]
   [shape type/3d r3]
   [shine type/float 0.25]
   [gloss type/float 4]
   [ambient type/float 0.2]]
  {type/vec3 |(set-param color $)
   type/3d |(set-param shape $)
   :shine |(set-param shine $)
   :gloss |(set-param gloss $)
   :ambient |(set-param ambient $)}
  (new-blinn-phong shape color shine gloss ambient))

(def-flexible-fn flat-color
  [[color] [shape type/3d r3]]
  {type/vec3 |(set-param color $)
   type/3d |(set-param shape $)}
  (new-flat-color shape color))

(def-flexible-fn fresnel
  [[color type/vec3 [1 1 1]]
   [shape]
   [strength type/float 0.25]
   [exponent type/float 5]]
  {type/vec3 |(set-param color $)
   type/float |(set-param strength $)
   type/3d |(set-param shape $)
   :exponent |(set-param exponent $)}
  (new-fresnel shape color strength exponent))

(def-flexible-fn cel
  [[color type/vec3]
   [shape type/3d r3]
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
  (new-cel shape color shine gloss ambient steps))

# TODO: I don't love the name "resurface"
(def-flexible-fn resurface
  [[shape type/3d] [color type/3d]]
  {type/3d |(set-first [shape color] $)
   :shape |(set-param shape $)
   :color |(set-param shape $)}
  (new-resurface shape color))

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
