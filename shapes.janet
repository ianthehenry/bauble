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
  :compile (fn [{:dimensions dimensions :center center} _ coord]
    (string/format "s3d_box(%s, %s, %s)" coord (vec3 center) (vec3 dimensions)))
  })

(def- sphere-proto @{
  :compile (fn [{:radius radius :center center} _ coord]
    (string/format "s3d_sphere(%s, %s, %s)" coord (vec3 center) (float radius)))
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

(def- union-proto @{
  :compile (fn [self comp-state coord]
    (def {:exprs exprs} self)
    (if (= 1 (length exprs))
      (:compile (first exprs) comp-state coord)
      (:function comp-state self "union" coord (fn [coord]
        (string/join [
          (string/format "float d = %s;" (:compile (first exprs) comp-state coord))
          ;(->> exprs
              (drop 1)
              (map (fn [expr]
                (string/format "d = min(d, %s);" (:compile expr comp-state coord)))))
          "return d;"
        ] "\n")))))
  })

(def- smooth-union-proto @{
  :compile (fn [self comp-state coord]
    (def {:exprs exprs :size size} self)
    (if (= 1 (length exprs))
      (:compile (first exprs) comp-state coord)
      (:function comp-state self "smooth_union" coord (fn [coord]
        (string/join [
          (string/format "float a = %s;" (:compile (first exprs) comp-state coord))
          (string/format "float k = %s;" (float size))
          "float b, h;"
          ;(->> exprs
              (drop 1)
              (map (fn [expr]
                (string/format
                  "b = %s; h=clamp(0.5 + 0.5 * (b - a) / k, 0.0, 1.0); a = mix(b, a, h) - k * h * (1.0 - h);"
                  (:compile expr comp-state coord)))))
          "return a;"
        ] "\n")))))
  })

(def- unit-vec3 [1 1 1])
(def- zero-vec3 [0 0 0])

(defn box [&opt dimensions center]
  (default dimensions unit-vec3)
  (default center zero-vec3)
  (table/setproto @{:dimensions dimensions :center center} box-proto))

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

(defn union [& exprs]
  (table/setproto @{:exprs exprs} union-proto))

(defn smooth-union [size & exprs]
  (table/setproto @{:size size :exprs exprs} smooth-union-proto))

(def- *tau* (* 2 math/pi))

(defn tau [x] (* *tau* x))

(defn tau/ [x] (/ *tau* x))
