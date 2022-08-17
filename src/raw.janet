(use ./helpers)
(use ./internal-helpers)
(use ./axes)
(use ./glsl)

(defn- arg-kvps [args]
  (mapcat |[(keyword $) $] args))

(defn- arg-kvps-no-shape [args]
  (mapcat |(if (= $ 'shape) [] [(keyword $) $]) args))

(defmacro- def-constructor [name args proto]
  (let [$proto (gensym)]
    ~(def ,name
      (let [,$proto ,proto]
        (fn ,args (struct/with-proto ,$proto ,;(arg-kvps args)))))))

(defmacro- def-primitive [name args & compile-body]
  ~(def-constructor ,name ,args
    {:compile (fn [self comp-state coord] (let [,(struct ;(arg-kvps args)) self] ,;compile-body))
     :surface (fn [self comp-state coord] "0.5 * (1.0 + normal)")}))

# an input-operator can only change the input coordinates
# TODO: alter shouldn't be a function. should get the same
# arg-kvps treatment as everyone else.
(defmacro- def-input-operator [name args & alter-body]
  (let [$alter (gensym)]
    ~(def-constructor ,name ,args
      (let [,$alter (fn [self comp-state coord]
                      (let [,(struct ;(arg-kvps-no-shape args)) self] ,;alter-body))]
        {:compile (fn [self comp-state coord]
          (:compile (self :shape) comp-state (,$alter self comp-state coord)))
         :surface (fn [self comp-state coord]
          (:surface (self :shape) comp-state (,$alter self comp-state coord)))}))))

(defmacro- def-operator [name args & compile-body]
  ~(def-constructor ,name ,args
    {:compile (fn [self comp-state coord] (let [,(struct ;(arg-kvps args)) self] ,;compile-body))
     :surface (fn [self comp-state coord] (:surface (self :shape) comp-state coord))}))

(defmacro- def-surfacer [name args & surface-body]
  ~(def-constructor ,name ,args
    {:compile (fn [self comp-state coord] (:compile (self :shape) comp-state coord))
     :surface (fn [self comp-state coord] (let [,(struct ;(arg-kvps args)) self] ,;surface-body))}))

(def-input-operator translate [offset shape]
  (string/format "(%s - %s)" coord (vec3 offset)))

(def-operator offset [amount shape]
  (string/format "(%s - %s)" (:compile shape comp-state coord) (float amount)))

(def-operator onion [thickness shape]
  (string/format "(abs(%s) - %s)" (:compile shape comp-state coord) (float thickness)))

# TODO: "amount" is interpolated multiple times here
# TODO: also this should do something to surfaces and it doesn't
(def-operator scale [shape amount]
  (string/format "(%s * %s)"
    (:compile shape comp-state (string/format "(%s / %s)" coord (float amount)))
    (float amount)))

# TODO: "amount" is interpolated multiple times here
(def-operator stretch [shape amount]
  (string/format "(%s * abs(min3(%s)))"
    (:compile shape comp-state (string/format "(%s / %s)" coord (vec3 amount)))
    (vec3 amount)))

(def-primitive r3 [] "0.0")
(def r3 (r3))

(def-primitive box [size]
  (:function comp-state "float" :box "s3d_box"
    [coord (vec3 size)]
    ["vec3 p" "vec3 size"] `
    vec3 q = abs(p) - size;
    return length(max(q, 0.0)) + min(max(q.x, max(q.y, q.z)), 0.0);
    `))

(def-primitive cylinder [axis radius height]
  (:function comp-state "float" [:cylinder axis] "s3d_cylinder"
    [coord (float radius) (float height)]
    ["vec3 p" "float radius" "float height"]
    (string/format `
    vec2 d = abs(vec2(length(p.%s), p.%s)) - vec2(radius, height);
    return min(max(d.x, d.y), 0.0) + length(max(d, 0.0));
    ` (string-of-axes (other-axes axis)) (string-of-axis axis))))

(def-primitive cone [axis radius height upside-down]
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

(def-primitive sphere [radius]
  (string "(length("coord") - "(float radius)")"))

(def-primitive half-space [axis sign]
  (string (if (neg? sign) "" "-") coord "." (string-of-axes [axis])))

(def-primitive line [start end]
  (:function comp-state "float" :line "s3d_line"
    [coord (vec3 start) (vec3 end)]
    ["vec3 p" "vec3 a" "vec3 b"] `
    vec3 pa = p - a, ba = b - a;
    float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0);
    return length(pa - ba * h);
    `))

(def-primitive torus [axis major-radius minor-radius]
  (:function comp-state "float" [:torus axis] "s3d_torus"
    [coord (float major-radius) (float minor-radius)]
    ["vec3 p" "float major_radius" "float minor_radius"]
    (string `
      vec2 q = vec2(length(p.`(string-of-axes (other-axes axis))`) - major_radius, p.`(string-of-axis axis)`);
      return length(q) - minor_radius;
    `)))

(def-input-operator transform [matrix shape]
  (string/format "(%s * %s)" coord (mat3 matrix)))

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

(def-input-operator twist [shape axis rate]
  (def other-axes (other-axes axis))
  (defn select [axis]
    (cond
      (= axis (other-axes 0)) "transformed.x"
      (= axis (other-axes 1)) "transformed.y"
      (string "p." axis)))
  (:function comp-state "vec3" [:twist axis] (string "twist_" axis)
    [coord (float rate)]
    ["vec3 p" "float rate"]
    (string `
    float s = sin(rate * p.`axis`);
    float c = cos(rate * p.`axis`);
    mat2 m = mat2(c, -s, s, c);
    vec2 transformed = m * p.`(string-of-axes other-axes)`;
    return vec3(`(select :x)`, `(select :y)`, `(select :z)`);
    `)))

(def-input-operator swirl [shape axis rate]
  (def other-axes (other-axes axis))
  (defn select [axis]
    (cond
      (= axis (other-axes 0)) "transformed.x"
      (= axis (other-axes 1)) "transformed.y"
      (string "p." axis)))
  (:function comp-state "vec3" [:bend axis] (string "bend_" axis)
    [coord (float rate)]
    ["vec3 p" "float rate"]
    (string `
    float a = length(p.`(string-of-axes other-axes)`);
    float s = sin(rate * a);
    float c = cos(rate * a);
    mat2 m = mat2(c, -s, s, c);
    vec2 transformed = m * p.`(string-of-axes other-axes)`;
    return vec3(`(select :x)`, `(select :y)`, `(select :z)`);
    `)))

(def-input-operator bend [shape axis towards rate]
  (def rate-axis (other-axis axis towards))
  (def distortion-axes [towards rate-axis])
  (defn select [axis]
    (cond
      (= axis (distortion-axes 0)) "transformed.x"
      (= axis (distortion-axes 1)) "transformed.y"
      (string "p." axis)))
  (:function comp-state "vec3" [:bend axis] (string "bend_" axis)
    [coord (float rate)]
    ["vec3 p" "float rate"]
    (string `
    float s = sin(rate * p.`rate-axis`);
    float c = cos(rate * p.`rate-axis`);
    mat2 m = mat2(c, -s, s, c);
    vec2 transformed = m * p.`(string-of-axes distortion-axes)`;
    return vec3(`(select :x)`, `(select :y)`, `(select :z)`);
    `)))

(def-input-operator mirror-axes [shape axes]
  (if (= (length axes) 3)
    (string `abs(`coord`)`)
    (:function comp-state "vec3" [:abs axes] (string "abs_" axes)
      [coord]
      ["vec3 p"]
      (string `p.`axes` = abs(p.`axes`); return p;`))))

(def-input-operator mirror-plane [shape axes]
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
      `)))

(def-input-operator mirror-space [shape]
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
    `))

(def-input-operator flip [shape axes signs]
  (def axes (string-of-axes axes))
  (if (nil? signs)
    (string coord "." axes)
    (string `(`coord`.`axes` * `(vec3 signs)`)`)))

(def-input-operator reflect-axes [shape axes]
  (if (= (length axes) 3)
    (string `-` coord)
    (:function comp-state "vec3" [:neg axes] (string "neg_" axes)
      [coord]
      ["vec3 p"]
      (string `p.`axes` = -p.`axes`; return p;`))))

# TODO: all of the union/intersect/subtract operators evaluate
# every surface in their collection, even when it will not
# contribute at all to the result. we could optimize this by
# only evaluating the "nearest" surface, or surfaces with
# a blend coefficient greater than 0.
(def-constructor union [shapes]
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

(def-constructor intersect [shapes]
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

(def-constructor subtract [shapes]
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

(def-constructor smooth-union [size shapes]
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

(def-constructor smooth-intersect [size shapes]
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

(def-constructor smooth-subtract [size shapes]
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

(def-input-operator tile [shape offset limit]
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
        "return p - offset * clamp(round(p / offset), -min_limit, max_limit);"))))

(def-constructor morph [weight shape1 shape2]
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

(def-surfacer flat-color [shape color]
  (vec3 color))

(def-surfacer blinn-phong [shape color shine gloss ambient]
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

(def-surfacer cel [shape color shine gloss ambient steps]
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

(def-surfacer fresnel [shape color strength exponent]
  (:function comp-state "vec3" self "fresnel"
    [coord "world_p" "camera" "normal" "light_intensities" (vec3 color) (float strength) (float exponent)]
    ["vec3 p" "vec3 world_p" "vec3 camera" "vec3 normal" "float light_intensities[3]" "vec3 color" "float strength" "float exponent"]
    (string `
    vec3 view_dir = normalize(camera - world_p);
    float fresnel = pow(1.0 - dot(normal, view_dir), exponent);
    return `(:surface shape comp-state coord)` + color * strength * fresnel;
    `)))

(def-constructor resurface [shape color]
  {:compile (fn [{:shape shape} comp-state coord] (:compile shape comp-state coord))
    :surface (fn [{:color color} comp-state coord] (:surface color comp-state coord))})
