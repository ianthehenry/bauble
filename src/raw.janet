(use ./helpers)
(use ./internal-helpers)
(use ./axes)
(import ./globals)
(import ./glslisp/src/type)
(import ./glslisp/src/util :as glslisp/util)

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
    {:compile (fn [self comp-state] (let [,(struct ;(arg-kvps args)) self] ,;compile-body))
     :surface (fn [self comp-state] ~(* 0.5 (+ 1 ,globals/normal)))}))

(defmacro- def-input-operator [name args & expression]
  ~(def-constructor ,name ,args
    {:compile (fn [self comp-state]
      (let [,(struct ;(arg-kvps args)) self]
        ~(with ,globals/p ,(do ,;expression)
          ,(:compile shape comp-state))))
     :surface (fn [self comp-state]
      (let [,(struct ;(arg-kvps args)) self]
        ~(with ,globals/p ,(do ,;expression)
          ,(:surface shape comp-state))))}))

(defmacro- def-operator [name args & compile-body]
  ~(def-constructor ,name ,args
    {:compile (fn [self comp-state] (let [,(struct ;(arg-kvps args)) self] ,;compile-body))
     :surface (fn [self comp-state] (:surface (self :shape) comp-state))}))

(defmacro- def-surfacer [name args & surface-body]
  ~(def-constructor ,name ,args
    {:compile (fn [self comp-state] (:compile (self :shape) comp-state))
     :surface (fn [self comp-state] (let [,(struct ;(arg-kvps args)) self] ,;surface-body))}))

(defmacro- def-complicated [name args compile-body surface-body]
  ~(def-constructor ,name ,args
    {:compile (fn [self comp-state] (let [,(struct ;(arg-kvps args)) self] ,compile-body))
     :surface (fn [self comp-state] (let [,(struct ;(arg-kvps args)) self] ,surface-body))}))

(def-input-operator move [shape amount]
  ~(- ,globals/p ,amount))

(def-operator offset [amount shape]
  ~(- ,(:compile shape comp-state) ,amount))

(def-operator onion [thickness shape]
  ~(- (abs ,(:compile shape comp-state)) ,thickness))

(def-operator slow [shape rate]
  ~(* ,(:compile shape comp-state) ,rate))

(def-input-operator distort [shape expression]
  expression)

(def-complicated scale [shape amount]
  (let [temp (:temp-var comp-state type/float 'scale)]
    ~(with ,temp ,amount
      (with ,globals/p (/ ,globals/p ,temp)
        (* ,(:compile shape comp-state) ,temp))))
  ~(with ,globals/p (/ ,globals/p ,amount)
      ,(:surface shape comp-state)))

(def-complicated stretch [shape amount]
  (let [temp (:temp-var comp-state type/vec3 'scale)]
    ~(with ,temp ,amount
      (with ,globals/p (/ ,globals/p ,temp)
        (* ,(:compile shape comp-state) (min3 (abs ,temp))))))
  ~(with ,globals/p (/ ,globals/p ,amount)
      ,(:surface shape comp-state)))

(def-primitive r3 [] 0)
(def r3 (r3))

(def-primitive sphere [radius]
  ~(- (length ,globals/p) ,radius))

(def-primitive ground [y]
  (let [temp (:temp-var comp-state type/float 'height)
        # TODO: through should really depend on minimum hit distance, right?
        # but whatever this is just a little hack
        through -0.01]
  ~(with ,temp (- (. ,globals/p :y) ,y)
    (+ (* ,temp (step ,through ,temp)) (* 10000 (- 1 (step ,through ,temp)))))))

(def-primitive box [size]
  (:generate-function comp-state "float" :box "s3d_box"
    [globals/p ["vec3 size" size]]
    `vec3 q = abs(p) - size;
     return length(max(q, 0.0)) + min(max(q.x, max(q.y, q.z)), 0.0);
     `))

(def-primitive ellipsoid [size]
  (:generate-function comp-state "float" :ellipsoid "s3d_ellipsoid"
    [globals/p ["vec3 size" size]] `
    float k0 = length(p/size);
    float k1 = length(p/(size*size));
    return k0*(k0-1.0)/k1;
    `))

# TODO: this is unused; maybe scrap it
(def-primitive ellipsoid2 [axis size]
  (:generate-function comp-state "float" [:ellipsoid axis] (string "s3d_ellipsoid_" axis)
    [globals/p ["vec2 size" size]]
    (string `
      vec2 p2 = vec2(length(p.`(string-of-axes (other-axes axis))`), p.`(string-of-axis axis)`);
      p2 = abs(p2); if( p2.x > p2.y ) {p2=p2.yx;size=size.yx;}
      float l = size.y*size.y - size.x*size.x;
      float m = size.x*p2.x/l;      float m2 = m*m;
      float n = size.y*p2.y/l;      float n2 = n*n;
      float c = (m2+n2-1.0)/3.0; float c3 = c*c*c;
      float q = c3 + m2*n2*2.0;
      float d = c3 + m2*n2;
      float g = m + m*n2;
      float co;
      if(d<0.0) {
        float h = acos(q/c3)/3.0;
        float s = cos(h);
        float t = sin(h)*sqrt(3.0);
        float rx = sqrt( -c*(s + t + 2.0) + m2 );
        float ry = sqrt( -c*(s - t + 2.0) + m2 );
        co = (ry+sign(l)*rx+abs(g)/(rx*ry)- m)/2.0;
      } else {
        float h = 2.0*m*n*sqrt( d );
        float s = sign(q+h)*pow(abs(q+h), 1.0/3.0);
        float u = sign(q-h)*pow(abs(q-h), 1.0/3.0);
        float rx = -s - u - c*4.0 + 2.0*m2;
        float ry = (s - u)*sqrt(3.0);
        float rm = sqrt( rx*rx + ry*ry );
        co = (ry/sqrt(rm-rx)+2.0*g/rm-m)/2.0;
      }
      vec2 r = size * vec2(co, sqrt(1.0-co*co));
      return length(r-p2) * sign(p2.y-r.y);`)))

(def-primitive cylinder [axis radius height]
  (:generate-function comp-state "float" [:cylinder axis] "s3d_cylinder"
    [globals/p ["float radius" radius] ["float height" height]]
    (string `
      vec2 d = abs(vec2(length(p.`(string-of-axes (other-axes axis))`), p.`(string-of-axis axis)`)) - vec2(radius, height);
      return min(max(d.x, d.y), 0.0) + length(max(d, 0.0));
      `)))

(def-primitive cone [axis radius height upside-down]
  (:generate-function comp-state "float" [:cone axis upside-down] "s3d_cone"
    [globals/p ["float radius" radius] ["float height" height]]
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

# TODO: it might be easier to just make this an actually-primitive function
(def-primitive rounded-cone [axis radius height round upside-down]
  (def $round (:temp-var comp-state type/float 'r))
  (def $height (:temp-var comp-state type/float 'height))
  (def $radius (:temp-var comp-state type/float 'radius))
  (def $tip-offset (:temp-var comp-state type/float 'tip_offset))
  ~(with ,$height ,height
    (with ,$radius ,radius
      (with ,$round ,round
        (with ,$tip-offset (* ,$round (/ ,$height ,$radius))
          ,(:compile (offset $round
            (move
              (cone
                axis
                ~(- ,$radius ,$round)
                ~(- ,$height ,$tip-offset)
                upside-down)
              (axis-vec axis (if upside-down
                ~(- ,$tip-offset ,$round)
                $round)))) comp-state))))))

(def-primitive half-space [axis sign]
  (if (neg? sign)
    ~(. ,globals/p ,axis)
    ~(- (. ,globals/p ,axis))))

(def-primitive line [start end]
  (:generate-function comp-state "float" :line "s3d_line"
    [globals/p ["vec3 a" start] ["vec3 b" end]]
    `vec3 pa = p - a, ba = b - a;
     float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0);
     return length(pa - ba * h);
     `))

(def-primitive torus [axis major-radius minor-radius]
  (:generate-function comp-state "float" [:torus axis] "s3d_torus"
    [globals/p ["float major_radius" major-radius] ["float minor_radius" minor-radius]]
    (string `
      vec2 q = vec2(length(p.`(string-of-axes (other-axes axis))`) - major_radius, p.`(string-of-axis axis)`);
      return length(q) - minor_radius;
      `)))

# TODO: this is pretty inelegant, and stems from the
# fact that I don't really treat matrices as a first-
# class type. I should fix that, and this could probably
# be a helper to convert literal tuples to matrices?
(def-input-operator transform [shape matrix]
  ~(* ,globals/p ,(if (glslisp/util/vec? matrix)
      ~(mat3 ,;matrix)
      matrix)))

(def-input-operator twist [shape axis rate]
  (def other-axes (other-axes axis))
  (defn select [axis]
    (cond
      (= axis (other-axes 0)) "transformed.x"
      (= axis (other-axes 1)) "transformed.y"
      (string "p." axis)))
  (:generate-function comp-state "vec3" [:twist axis] (string "twist_" axis)
    [globals/p ["float rate" rate]]
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
  (:generate-function comp-state "vec3" [:swirl axis] (string "swirl_" axis)
    [globals/p ["float rate" rate]]
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
  (:generate-function comp-state "vec3" [:bend axis] (string "bend_" axis)
    [globals/p ["float rate" rate]]
    (string `
      float s = sin(rate * p.`rate-axis`);
      float c = cos(rate * p.`rate-axis`);
      mat2 m = mat2(c, -s, s, c);
      vec2 transformed = m * p.`(string-of-axes distortion-axes)`;
      return vec3(`(select :x)`, `(select :y)`, `(select :z)`);
      `)))

(def-input-operator biased-sqrt [shape r axes]
  (:generate-function comp-state "vec3" [:biased_sqrt axes] (string "biased_sqrt_" axes)
    [globals/p ["float r" r]]
    (string `p.`axes` = sqrt(p.`axes` * p.`axes` + r * r); return p;`)))

(def-input-operator mirror [shape axes]
  (if (= (length axes) 3)
    ~(abs ,globals/p)
    (:generate-function comp-state "vec3" [:abs axes] (string "abs_" axes)
      [globals/p]
      (string `p.`axes` = abs(p.`axes`); return p;`))))

(def-input-operator mirror-plane [shape axes]
  (def [axis1 axis2] axes)
  (defn select [axis]
    (cond
      (= axis1 axis) "hi"
      (= axis2 axis) "lo"
      (string `p.` axis)))
  (:generate-function comp-state "vec3" [:sort axes] (string "sort_" axis1 axis2)
    [globals/p]
    (string `
      float lo = min(p.`axis1`, p.`axis2`);
      float hi = max(p.`axis1`, p.`axis2`);
      return vec3(`(select :x)`, `(select :y)`, `(select :z)`);
      `)))

(def-input-operator mirror-space [shape] ~(sort3 ,globals/p))

(def-input-operator flip [shape axes signs]
  (let [axes (string-of-axes axes)]
    (if (nil? signs)
      ~(. ,globals/p ,axes)
      ~(* (. ,globals/p ,axes) ,signs))))

(def-input-operator reflect-axes [shape axes]
  (if (= (length axes) 3)
    ~(- ,globals/p)
    (:generate-function comp-state "vec3" [:neg axes] (string "neg_" axes)
      [globals/p]
      (string `p.`axes` = -p.`axes`; return p;`))))

(defn- fold-shapes [comp-state shapes &named preamble f return switch each-case]
  (default preamble [])

  (def lines @[])
  (var index 0)

  (def line (partial array/push lines))
  (def cat (partial array/concat lines))

  # TODO: so consider this:
  #
  # (union
  #   (box 50 | move :x -10)
  #   (sphere 70 | move :x 10))
  #
  # We compute an altered value of `p` for both the distance and
  # color, even though it's identical. And even though the color
  # doesn't actually use `p` -- we should be able to elide that.
  # But we don't. Because it's not smart enough.
  #
  # I'm not really sure how to reduce the duplication when we compile
  # distance and color in one block. But we definitely should be able
  # to do that... actually, changing the representation so that
  # input-operator doesn't apply itself *twice* would fix that, right?
  # Hmm.
  (defn proxy [shape type method]
    (fn [self]
      (def scope (:new-scope comp-state :export-free-vars true))
      (def [statements expression] (method scope shape))
      (if (empty? statements)
        expression
        (do
          (def new-name (string "_r" (++ index)))
          (line (string type` `new-name`;`))
          (line `{`)
          (cat statements)
          (line (string new-name` = ` expression`;`))
          (line `}`)
          new-name))))

  (defn proxy-shape [shape]
    {:compile (proxy shape "float" :compile-distance)
     :surface (proxy shape "vec3" :compile-color)})

  (cat preamble)

  (for i 0 (length shapes)
    (line (f i (proxy-shape (shapes i)))))

  (when switch
    (line (string `switch (`switch`) {`))
    (for i 0 (length shapes)
      (line (string `case `i`:`))
      (line (each-case (proxy-shape (shapes i))))
      (line `break;`))
    (line `}`))

  (line (string `return `return`;`))

  (string/join lines "\n"))

(def-complicated union [shapes]
  (:generate-function comp-state "float" [self :distance] "union" []
    (fn [comp-state]
      (fold-shapes comp-state shapes
        :f (fn [i $]
          (if (= i 0)
            (string `float d = `(:compile $)`;`)
            (string `d = min(d, `(:compile $)`);`)))
        :return "d")))
  (:generate-function comp-state "vec3" [self :color] "union_color" []
    (fn [comp-state]
      (fold-shapes comp-state shapes
        :f (fn [i $]
          (if (= i 0)
            (string `float d = `(:compile $)`; float d2; int surface = `i`; vec3 color;`)
            (string `d2 = `(:compile $)`; if (d2 < d) { d = d2; surface = `i`; }`)))
        :switch "surface"
        :each-case |(string `color = `(:surface $)`;`)
        :return "color"))))

(def-complicated intersect [shapes]
  (:generate-function comp-state "float" [self :distance] "intersect" []
    (fn [comp-state]
      (fold-shapes comp-state shapes
        :f (fn [i $]
          (if (= i 0)
            (string `float d = `(:compile $)`;`)
            (string `d = max(d, `(:compile $)`);`)))
        :return "d")))
  (:generate-function comp-state "vec3" [self :color] "intersect_color" []
    (fn [comp-state]
      (fold-shapes comp-state shapes
        :f (fn [i $]
          (if (= i 0)
            (string `float d = `(:compile $)`; float d2; int surface = `i`; vec3 color;`)
            (string `d2 = `(:compile $)`; if (d2 > d) { d = d2; surface = `i`; }`)))
        :switch "surface"
        :each-case |(string `color = `(:surface $)`;`)
        :return "color"))))

# TODO: can i write subtract as invert + intersect? i should be able to, right?
# will that work with surfaces?
(def-complicated subtract [shapes]
  (:generate-function comp-state "float" [self :distance] "subtract" []
    (fn [comp-state]
      (fold-shapes comp-state shapes
        :f (fn [i $]
          (if (= i 0)
            (string `float d = `(:compile $)`;`)
            (string `d = max(d, -`(:compile $)`);`)))
        :return "d")))
  (:generate-function comp-state "vec3" [self :color] "subtract_color" []
    (fn [comp-state]
      (fold-shapes comp-state shapes
        :f (fn [i $]
          (if (= i 0)
            (string `float d = `(:compile $)`; float d2; int surface = `i`; vec3 color;`)
            (string `d2 = -`(:compile $)`; if (d2 > d) { d = d2; surface = `i`; }`)))
        :switch "surface"
        :each-case |(string `color = `(:surface $)`;`)
        :return "color"))))

# TODO: All of the smooth boolean operations evaluate
# every surface in their collection, even when it will not
# contribute at all to the final result.
(def-complicated smooth-union [r shapes]
  (:generate-function comp-state "float" [self :distance] "union" [["float r" r]]
    (fn [comp-state]
      (fold-shapes comp-state shapes
        :preamble ["float b, h = 0.0;" "r = max(r, 0.0000000001);"]
        :f (fn [i $]
          (if (= i 0)
            (string `float a = `(:compile $)`;`)
            (string `
              b = `(:compile $)`;
              h = clamp(0.5 + 0.5 * (b - a) / r, 0.0, 1.0);
              a = mix(b, a, h) - r * h * (1.0 - h);
            `)))
        :return "a")))
  (:generate-function comp-state "vec3" [self :color] "union_color" [["float r" r]]
    (fn [comp-state]
      (fold-shapes comp-state shapes
        :preamble ["float b, h;" "r = max(r, 0.0000000001);"]
        :f (fn [i $]
          (if (= i 0)
            (string `float a = `(:compile $)`; vec3 color = `(:surface $)`;`)
            (string `
            b = `(:compile $)`;
            h = clamp(0.5 + 0.5 * (b - a) / r, 0.0, 1.0);
            a = mix(b, a, h) - r * h * (1.0 - h);
            color = mix(`(:surface $)`, color, h);
            `)))
        :return "color"))))

(def-complicated smooth-intersect [r shapes]
  (:generate-function comp-state "float" [self :distance] "intersect" [["float r" r]]
    (fn [comp-state]
      (fold-shapes comp-state shapes
        :preamble ["float b, h = 0.0;" "r = max(r, 0.0000000001);"]
        :f (fn [i $]
          (if (= i 0)
            (string `float a = `(:compile $)`;`)
            (string `
              b = `(:compile $)`;
              h = clamp(0.5 - 0.5 * (b - a) / r, 0.0, 1.0);
              a = mix(b, a, h) + r * h * (1.0 - h);
              `)))
        :return "a")))
  (:generate-function comp-state "vec3" [self :color] "intersect_color" [["float r" r]]
    (fn [comp-state]
      (fold-shapes comp-state shapes
        :preamble ["float b, h;" "r = max(r, 0.0000000001);"]
        :f (fn [i $]
          (if (= i 0)
            (string `float a = `(:compile $)`; vec3 color = `(:surface $)`;`)
            (string `
              b = `(:compile $)`;
              h = clamp(0.5 - 0.5 * (b - a) / r, 0.0, 1.0);
              a = mix(b, a, h) + r * h * (1.0 - h);
              color = mix(`(:surface $)`, color, h);
              `)))
        :return "color"))))

(def-complicated smooth-subtract [r shapes]
  (:generate-function comp-state "float" [self :distance] "subtract" [["float r" r]]
    (fn [comp-state]
      (fold-shapes comp-state shapes
        :preamble ["float b, h = 0.0;" "r = max(r, 0.0000000001);"]
        :f (fn [i $]
          (if (= i 0)
            (string `float a = `(:compile $)`;`)
            (string `
              b = `(:compile $)`;
              h = clamp(0.5 - 0.5 * (a + b) / r, 0.0, 1.0);
              a = mix(a, -b, h) + r * h * (1.0 - h);
              `)))
        :return "a")))
  (:generate-function comp-state "vec3" [self :color] "subtract_color" [["float r" r]]
    (fn [comp-state]
      (fold-shapes comp-state shapes
        :preamble ["float b, h;" "r = max(r, 0.0000000001);"]
        :f (fn [i $]
          (if (= i 0)
            (string `float a = `(:compile $)`; vec3 color = `(:surface $)`;`)
            (string `
              b = `(:compile $)`;
              h = clamp(0.5 - 0.5 * (a + b) / r, 0.0, 1.0);
              a = mix(a, -b, h) + r * h * (1.0 - h);
              color = mix(color, `(:surface $)`, h);
              `)))
        :return "color"))))

# TODO: this is so gross; i just can't handle the extreme copy-and-paste.
# i need to make a more general version of this...
(defmacro- make-tile-body [method]
  ~(let [$offset (:temp-var comp-state type/vec3 'offset)
         $index (:temp-var comp-state type/vec3 'index)
         index-expr ~(round (safe_div3 ,globals/p ,$offset))
         index-expr
           (if-let [limit limit
                    min-limit (map3 limit |(idiv (- $ 1) 2))
                    max-limit (map3 limit |(idiv $ 2))]
             ~(clamp ,index-expr (- ,min-limit) ,max-limit)
             index-expr)
        recenter (if (nil? limit) id (fn [expr]
          ~(with ,globals/p (- ,globals/p (* ,(map |(if (even? $) -0.5 0) limit) ,$offset)) ,expr)))
        shape (if (nil? shape) (f $index)
          (if (nil? f) shape (f shape $index)))]
      ~(with ,$offset ,offset
        ,(recenter
          ~(with ,$index ,index-expr
            (with ,globals/p (- ,globals/p (* ,$index ,$offset))
              ,(,method shape comp-state)))))))

(def-complicated tile [shape f offset limit]
  (make-tile-body :compile)
  (make-tile-body :surface))

(defn- angle-around [axis]
  (def axes
    (case axis
      :x [:y :z]
      :y [:z :x]
      :z [:x :y]))
  ~(atan2 (. ,globals/p ,(axes 0)) (. ,globals/p ,(axes 1))))

(defn- radial-translation-axis [axis]
  (case axis :x :z :y :x :z :y))

(defmacro- make-radial-body [method]
  ~(let [axes (other-axes axis)
         rotate (symbol "rotate_" axis)
         $angle (:temp-var comp-state type/float 'angle)
         $index (:temp-var comp-state type/float 'index)
         index-expr ~(floor (/ (mod (+ ,(angle-around axis) (* 0.5 ,$angle)) ,tau) ,$angle))
         shape (if (nil? shape) (f $index)
          (if (nil? f) shape (f shape $index)))]
      ~(with ,$angle ,angle
        (with ,$index ,index-expr
          (with ,globals/p
            (- (* ,globals/p (,rotate (* ,$index ,$angle))) ,(axis-vec (radial-translation-axis axis) radius))
              ,(,method shape comp-state))))))

(def-complicated radial [shape f angle radius axis]
  (make-radial-body :compile)
  (make-radial-body :surface))

(def-complicated morph [weight shape1 shape2]
  (let [distance1 (:compile shape1 comp-state)
        distance2 (:compile shape2 comp-state)]
    ~(mix ,distance1 ,distance2 ,weight))
  (let [color1 (:surface shape1 comp-state)
        color2 (:surface shape2 comp-state)]
    ~(mix ,color1 ,color2 ,weight)))

# TODO: we generate a separate version of this function for every
# use, but we should generate a separate version for every *unique signature*
# -- that is, the only thing that actually varies here is the array length. Can we use
# overloads to make this work?
(def-surfacer blinn-phong [shape color shine gloss]
  (:generate-function comp-state "vec3" :blinn-phong "blinn_phong"
    [globals/P
     globals/camera
     globals/normal
     globals/occlusion
     globals/lights
     ["vec3 color" color]
     ["float shine" shine]
     ["float gloss" gloss]]
    `vec3 view_dir = normalize(camera_origin - P);
     vec3 result = vec3(0.0);
     for (int i = 0; i < lights.length(); i++) {
       vec3 light_color = lights[i].color;
       vec3 light_dir = lights[i].direction;
       float diffuse;
       if (light_dir == vec3(0.0)) {
        diffuse = 1.0;
       } else {
         vec3 halfway_dir = normalize(light_dir + view_dir);
         float specular_strength = shine * pow(max(dot(normal, halfway_dir), 0.0), gloss * gloss);
         result += light_color * specular_strength;
         diffuse = max(0.0, dot(normal, light_dir));
       }
       result += color * diffuse * light_color;
     }
     return result;
     `))

(def-surfacer fresnel [shape color strength exponent]
  (def fresnel
    (:generate-function comp-state "vec3" :fresnel "fresnel"
      [globals/P
       globals/camera
       globals/normal
       ["vec3 color" color]
       ["float strength" strength]
       ["float exponent" exponent]]
      `vec3 view_dir = normalize(camera_origin - P);
       float fresnel = pow(1.0 - dot(normal, view_dir), exponent);
       return color * strength * fresnel;
       `))
  ~(+ ,(:surface shape comp-state) ,fresnel))

(def-surfacer flat-color [color] color)

(def-operator map-distance [shape f]
  (let [$d (:temp-var comp-state type/float 'distance)]
    ~(with ,$d ,(:compile shape comp-state)
      ,(f $d))))

(def-surfacer map-color [shape f]
  (let [$c (:temp-var comp-state type/vec3 'color)]
    ~(with ,$c ,(:surface shape comp-state)
      ,(f $c))))

(def-complicated resurface [shape color]
  (:compile shape comp-state)
  (:surface color comp-state))

(def-operator bound [shape boundary threshold]
  (:generate-function comp-state "float" self "bound"
    [["float threshold" threshold]]
    (fn [comp-state]
      (def [boundary-statements boundary-expression]
        (:compile-distance (:new-scope comp-state :export-free-vars true) boundary))
      (def [shape-statements shape-expression]
        (:compile-distance (:new-scope comp-state :export-free-vars true) shape))
      (string/join [
        ;boundary-statements
        (string `float boundary_distance = `boundary-expression`;`)
        "if (boundary_distance < threshold) {"
          ;shape-statements
          (string `return `shape-expression`;`)
        "} else {"
          "return boundary_distance;"
        "}"
        ] "\n"))))

(def-operator bounded [shape f magnitude threshold]
  (let [$magnitude (:temp-var comp-state type/float 'magnitude)
        boundary (offset $magnitude shape)
        distorted-shape (f shape $magnitude)]
    ~(with ,$magnitude ,magnitude
      ,(:compile (bound distorted-shape boundary threshold) comp-state))))

(def-complicated pivot [shape pivot-point f]
  (let [$pivot (:temp-var comp-state type/vec3 'pivot)]
    ~(with ,$pivot ,pivot-point
      ,(:compile (move (f (move shape ~(- ,$pivot))) $pivot) comp-state)))
  (let [$pivot (:temp-var comp-state type/vec3 'pivot)]
    ~(with ,$pivot ,pivot-point
      ,(:surface (move (f (move shape ~(- ,$pivot))) $pivot) comp-state))))

(def-surfacer apply-light [shape light]
  (:apply light shape comp-state))
