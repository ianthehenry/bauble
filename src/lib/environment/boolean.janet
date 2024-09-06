(use ./import)

(defn- boolean [shapes reduce-distances reduce-colors]
  (shape/merge shapes (fn [fields]
    (def distances (seq [{:distance d} :in fields :when d] d))
    (def colors (seq [{:color c :distance d} :in fields :when c] [c (@or d (jlsl/coerce-expr 0))]))
    {:distance
      (case (@length distances)
        0 nil
        1 (distances 0)
        (reduce-distances distances))
      :color (case (@length colors)
        0 nil
        1 ((colors 0) 0)
        (reduce-colors colors))})))

(defn- min-distance [distances]
  (jlsl/do "min-distance"
     (var nearest ,(first distances))
     ,;(seq [expr :in (drop 1 distances)]
       (jlsl/statement (set nearest (min nearest ,expr))))
     nearest))

(defn- max-distance [distances]
  (jlsl/do "max-distance"
     (var nearest ,(first distances))
     ,;(seq [expr :in (drop 1 distances)]
       (jlsl/statement (set nearest (max nearest ,expr))))
     nearest))

(defn- neg-max-distance [distances]
  (jlsl/do "max-distance"
     (var nearest ,(first distances))
     ,;(seq [expr :in (drop 1 distances)]
       (jlsl/statement (set nearest (max nearest (- ,expr)))))
     nearest))

(defn- smooth-min-distance [r distances]
  (jlsl/do "smooth-min-distance"
    (var r ,r)
    (var nearest ,(first distances))
    ,;(seq [expr :in (drop 1 distances)]
      (jlsl/statement
        (var dist ,expr)
        (var h (remap+ (clamp (/ (- nearest dist) r) -1 1)))
        (set nearest (- (mix nearest dist h) (* r h (- 1 h))))))
    nearest))

(defn- smooth-max-distance [r distances]
  (jlsl/do "smooth-min-distance"
    (var r ,r)
    (var nearest ,(first distances))
    ,;(seq [expr :in (drop 1 distances)]
      (jlsl/statement
        (var dist ,expr)
        (var h (- 1 (remap+ (clamp (/ (- nearest dist) r) -1 1))))
        (set nearest (+ (mix nearest dist h) (* r h (- 1 h))))))
    nearest))

(defn- smooth-neg-max-distance [r distances]
  (jlsl/do "smooth-min-distance"
    (var r ,r)
    (var nearest ,(first distances))
    ,;(seq [expr :in (drop 1 distances)]
      (jlsl/statement
        (var dist (- ,expr))
        (var h (- 1 (remap+ (clamp (/ (- nearest dist) r) -1 1))))
        (set nearest (+ (mix nearest dist h) (* r h (- 1 h))))))
    nearest))

(defn- sharp-union-color [colors]
  (def surface-id
    (jlsl/iife "union-color-index"
      (var nearest 1e20)
      (var nearest-index :-1)
      ,;(seq [[i [_ d]] :pairs (reverse colors)]
        (def i (keyword (- (@length colors) i 1)))
        (jlsl/statement
          (var d ,d)
          (if (< d 0) (return i))
          (if (< d nearest) (do
            (set nearest d)
            (set nearest-index i)))))
      nearest-index))

  (jlsl/iife "union-color"
    ,(jlsl/statement/case surface-id
      (seq [[i [c _]] :pairs colors] [(jlsl/coerce-expr (keyword i)) (jlsl/statement (return ,c))]))
    [0 0 0]))

(defn- smooth-union-color [r colors]
  (jlsl/do "smooth-union-color"
    (var r ,r)
    (var color [0 0 0])
    (var nearest 1e20)
    ,;(seq [[color-expr dist-expr] :in colors]
      (jlsl/statement
        (var dist ,dist-expr)
        (var contribution (- 1 (remap+ (clamp (/ (min dist (- dist nearest)) r) -1 1))))
        (set nearest (- (mix nearest dist contribution) (* r contribution (- 1 contribution))))
        (if (> contribution 0) (set color (mix color ,color-expr contribution)))
      ))
    color))
(defn- smooth-union-color-symmetric [r colors]
  (jlsl/do "smooth-union-color"
    (var r ,r)
    (var color [0 0 0])
    (var nearest 1e20)
    ,;(seq [[color-expr dist-expr] :in colors]
      (jlsl/statement
        (var dist ,dist-expr)
        (var contribution (remap+ (clamp (/ (- nearest dist) r) -1 1)))
        (set nearest (- (mix nearest dist contribution) (* r contribution (- 1 contribution))))
        (if (> contribution 0) (set color (mix color ,color-expr contribution)))
      ))
      color))

(defn- smooth-subtract-color-symmetric [r colors]
  (def [first-color first-dist] (first colors))
  (jlsl/do "smooth-subtract-color"
    (var r ,r)
    (var color first-color)
    (var nearest first-dist)
    ,;(seq [[color-expr dist-expr] :in (drop 1 colors)]
      (jlsl/statement
        (var dist (- ,dist-expr))
        (var contribution (- 1 (remap+ (clamp (/ (- nearest dist) r) -1 1))))
        (set nearest (+ (mix nearest dist contribution) (* r contribution (- 1 contribution))))
        (if (> contribution 0) (set color (mix color ,color-expr contribution)))
      ))
      color))

(defn- zero-to-nil [x] (if (= x 0) nil x))

(defnamed union [:?r :?rs :?distance :?color &shapes]
  ```
  Union two or more shapes together. Pass `:r` or `:rs` to produce a smooth union.

  `:r` and `:rs` combine color fields differently. `:rs` is a symmetric union, where
  the color field is based on the nearest shape, regardless of the order that they're
  specified. `:r` is an asymmetric union where the order matters, and later shapes will
  be considered "on top of" previous shapes.

  These produce identical colors on the surface, but different interior color fields.
  It's easy to see the difference in 2D, while in 3D the difference only matters if
  you're cutting into a shape, or transplanting the color field from one shape to another.

  You can also pass `:distance` or `:color` to specify a different smoothing radius for
  the separate fields. For example, to produce a smooth symmetric color union with a sharp
  distance field, pass `(union :rs 10 :distance 0 ;shapes)`.
  ```
  (assert (not (and r rs)) "you can only specify :r or :rs, not both")
  (def distance-roundness (zero-to-nil (or distance r rs)))
  (def color-roundness (zero-to-nil (or color r rs)))
  (boolean shapes
    (if distance-roundness
      (partial smooth-min-distance (typecheck distance-roundness jlsl/type/float))
      min-distance)
    (if color-roundness
      (partial
        (if rs smooth-union-color-symmetric smooth-union-color)
        (typecheck color-roundness jlsl/type/float))
      sharp-union-color)))

(defnamed intersect [:?r :?rs :?distance :?color &shapes]
  ```
  Intersect two or more shapes. The named arguments here produce a smooth intersection,
  and are similar to the arguments to `union`.

  If you're performing rounded intersections with surfaced shapes in 3D, the color
  field produced by `:rs` might give more intuitive results. This is because
  the color field of the first shape is only visible as a thin, two-dimensional
  surface, and as soon as you start to blend it with the second shape it will be
  overtaken.

  Meanwhile if you're working in 2D, or looking at the interior distance field of a 3D
  intersection (i.e. slicing into the intersection, or transplanting the color field),
  the asymmetric `:r` rounding will probably be more intuitive.
  ```
  (assert (not (and r rs)) "you can only specify :r or :rs, not both")
  (def distance-roundness (zero-to-nil (or distance r rs)))
  (def color-roundness (zero-to-nil (or color r rs)))
  (boolean shapes
    (if distance-roundness
      (partial smooth-max-distance (typecheck distance-roundness jlsl/type/float))
      max-distance)
    (if color-roundness
      (partial
        (if rs smooth-union-color-symmetric smooth-union-color)
        (typecheck color-roundness jlsl/type/float))
      sharp-union-color)
    ))

(defnamed subtract [:?r :?rs :?distance :?color &shapes]
  ```
  Subtract one or more shapes from a source shape. The named arguments
  here produce a smooth subtraction, and are similar to the arguments to `union`.

  If you're performing rounded subtractions with surfaced shapes in 3D, the color
  field produced by `:rs` might give more intuitive results. This is because
  the color field of the first shape is only visible as a thin, two-dimensional
  surface, and as soon as you start to blend it with the second shape it will be
  overtaken.

  Meanwhile if you're working in 2D, or looking at the interior distance field of a 3D
  subtraction (i.e. slicing into the subtracting shape), the asymmetric `:r` rounding
  will probably be more intuitive.

  ```
  (assert (not (and r rs)) "you can only specify :r or :rs, not both")
  (def distance-roundness (zero-to-nil (or distance r rs)))
  (def color-roundness (zero-to-nil (or color r rs)))
  (boolean shapes
    (if distance-roundness
      (partial smooth-neg-max-distance (typecheck distance-roundness jlsl/type/float))
      neg-max-distance)
    (if color-roundness
      (partial
        (if rs smooth-subtract-color-symmetric smooth-union-color)
        (typecheck color-roundness jlsl/type/float))
      sharp-union-color)))

(defn- get-coefficient [value] (typecheck value jlsl/type/float))
(deffn morph [& args]
  ````(morph shape1 amount shape2 [:distance amount] [:color amount])

  Morph linearly interpolates between two shapes.

  ```
  # 50% box, 50% sphere
  (box 50 | morph (sphere 50))

  # 75% box, 25% sphere
  (box 50 | morph 0.25 (sphere 50))
  ```

  Concretely this means that it returns a new shape whose individual fields
  are linear interpolations of its inputs. With an anonymous `amount` coefficient,
  both the distance and color fields will be interpolated with the same value.
  But you can also specify per-field overrides:

  ```
  # distance is a 50% blend, but the color is 90% red
  (box 50 | color [1 0 0] | morph :color 0.1 (sphere 50 | color [0 1 0]))
  ```
  ````
  (def field-coefficients @{})
  (var default-coefficient nil)
  (var key (next args))
  (def shapes @[])

  (while (not= key nil)
    (var arg (@in args key))
    (cond
      (shape/is? arg) (array/push shapes arg)
      (keyword? arg) (do
        (set key (next args key))
        (assertf (not= key nil) "no value for %q" arg)
        (put field-coefficients arg (get-coefficient (args key))))
      (if (= nil default-coefficient)
        (set default-coefficient (get-coefficient arg))
        (error "duplicate coefficient")))
    (set key (next args key)))
  (default default-coefficient (jlsl/coerce-expr 0.5))
  (shape/merge shapes (fn [fields]
    (merge-structs (fn [field a b]
      (mix a b (@in field-coefficients field default-coefficient))) fields))))
