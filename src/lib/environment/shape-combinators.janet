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

(defn- sharp-union-color-symmetric [colors]
  (def surface-id
    (jlsl/iife "union-color-index"
      (var nearest 1e20)
      (var nearest-index 0:u)
      ,;(seq [[i [_ d]] :pairs (reverse colors)]
        (def i (int/u64 (- (@length colors) i 1)))
        (jlsl/statement
          (var d ,d)
          (if (< d nearest) (do
            (set nearest d)
            (set nearest-index i)))))
      nearest-index))
  (jlsl/iife "union-color"
    ,(jlsl/statement/case surface-id
      (seq [[i [c _]] :pairs colors] [(jlsl/coerce-expr (int/u64 i)) (jlsl/statement (return ,c))]))
    [0 0 0]))

(defn- sharp-union-color-asymmetric [colors]
  (def surface-id
    (jlsl/iife "union-color-index"
      (var nearest 1e20)
      (var nearest-index 0:u)
      ,;(seq [[i [_ d]] :pairs (reverse colors)]
        (def i (int/u64 (- (@length colors) i 1)))
        (jlsl/statement
          (var d ,d)
          (if (< d 0) (return i))
          (if (< d nearest) (do
            (set nearest d)
            (set nearest-index i)))))
      nearest-index))
  (jlsl/iife "union-color"
    ,(jlsl/statement/case surface-id
      (seq [[i [c _]] :pairs colors] [(jlsl/coerce-expr (int/u64 i)) (jlsl/statement (return ,c))]))
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

# TODO: :distance and :color shouldn't be special-cased;
# these operations should work with any :&fields
(defnamed union [:?r :?s :?distance :?color &shapes]
  ````
  Union two or more shapes together. Pass `:r` or `:s` to produce a smooth union.

  ```example
  (union
    (ball 100 | shade red | move x -50)
    (ball 100 | shade sky | move x 50))
  ```

  There are two ways that `union` (and other boolean operations) can combine color fields.
  The default is to put later shapes "on top of" earlier shapes:

  ```example
  (union
    (circle 100 | move x -50 | color red)
    (circle 100 | move x +50 | color sky))
  ```

  And you can perform a smoothed version of this operation with `:r`:

  ```example
  (union :r 20
    (circle 100 | move x -50 | color red)
    (circle 100 | move x +50 | color sky))
  ```

  The other way to combine color fields is to simply pick the nearest
  color. This produces a symmetric color field where the order of arguments
  doesn't matter:

  ```example
  (union :s 20
    (circle 100 | move x -50 | color red)
    (circle 100 | move x +50 | color sky))
  ```

  (You can pass `:s 0` if you want a sharp symmetric color union.)

  In 3D, the difference is harder to see, because they both produce
  the same color field at the shape's surface:

  ```example
  (union
    (union :r 20
      (ball 100 | move x -50 | shade red)
      (ball 100 | move x +50 | shade sky)
    | move y 100)
    (union :s 20
      (ball 100 | move x -50 | shade red)
      (ball 100 | move x +50 | shade sky)
    | move y -100))
  ```

  But just as in 2D, they produce different colors inside the shapes:

  ```example
  (union
    (union :r 20
      (ball 100 | move x -50 | shade red)
      (ball 100 | move x +50 | shade sky)
    | move y 100)
    (union :s 20
      (ball 100 | move x -50 | shade red)
      (ball 100 | move x +50 | shade sky)
    | move y -100)
  | sliced z (sin t * 50))
  ```

  This is more relevant when using `subtract` or `intersect`, which will
  typically prefer the `:s` behavior.

  You can also pass `:distance` or `:color` to specify a different smoothing radius for
  the separate fields. For example, you can produce a smooth symmetric color union with a sharp
  distance field:

  ```example
  (union :s 30 :distance 0
    (ball 100 | move x -50 | shade red)
    (ball 100 | move x +50 | shade sky))
  ```

  Or a smooth distance field with a sharp transition in color:

  ```example
  (union :r 30 :color 0
    (ball 100 | move x -50 | shade red)
    (ball 100 | move x +50 | shade sky))
  ```

  Or any combination like that.
  ````
  (assert (not (and r s)) "you can only specify :r or :s, not both")
  (def distance-roundness (zero-to-nil (or distance r s)))
  (def color-roundness (zero-to-nil (or color r s)))
  (boolean shapes
    (if distance-roundness
      (partial smooth-min-distance (typecheck distance-roundness jlsl/type/float))
      min-distance)
    (if color-roundness
      (partial
        (if s smooth-union-color-symmetric smooth-union-color)
        (typecheck color-roundness jlsl/type/float))
      (if s sharp-union-color-symmetric sharp-union-color-asymmetric))))

(defnamed intersect [:?r :?s :?distance :?color &shapes]
  ````
  Intersect two or more shapes. The named arguments produce a smooth intersection;
  see `union` for a thorough description.

  ```example
  (intersect
    (ball 100 | move x -50 | shade red)
    (ball 100 | move x +50 | shade sky))
  ```

  Note that although it doesn't matter when doing a sharp intersection,
  you probably want to use `:s` to smooth over `:r`, or else the latter
  shape's color field will "take over" the earlier shape. Compare:

  ```example
  (intersect :r 30
    (ball 100 | move x -50 | shade red)
    (ball 100 | move x +50 | shade sky))
  ```
  ```example
  (intersect :s 30
    (ball 100 | move x -50 | shade red)
    (ball 100 | move x +50 | shade sky))
  ```

  This effect makes sense if you think about the shapes in 2D:

  ```example
  (intersect :r 30
    (circle 100 | move x -50 | color red)
    (circle 100 | move x +50 | color sky))
  ```

  The second shape was on top of the first shape, so the first
  shape's color field is only visible where it fades into the
  shape of the first. But with a symmetric intersection:

  ```example
  (intersect :s 30
    (circle 100 | move x -50 | color red)
    (circle 100 | move x +50 | color sky))
  ```

  This doesn't happen.
  ````
  (assert (not (and r s)) "you can only specify :r or :s, not both")
  (def distance-roundness (zero-to-nil (or distance r s)))
  (def color-roundness (zero-to-nil (or color r s)))
  (boolean shapes
    (if distance-roundness
      (partial smooth-max-distance (typecheck distance-roundness jlsl/type/float))
      max-distance)
    (if color-roundness
      (partial
        (if s smooth-union-color-symmetric smooth-union-color)
        (typecheck color-roundness jlsl/type/float))
      (if s sharp-union-color-symmetric sharp-union-color-asymmetric))))

(defnamed subtract [:?r :?s :?distance :?color &shapes]
  ````
  Subtract one or more shapes from a source shape. The named arguments
  here produce a smooth subtraction, and are similar to the arguments to `union`.

  ```example
  (subtract
    (ball 100 | move x -50 | shade red)
    (ball 100 | move x +50 | shade sky))
  ```

  Like `union` and `intersect`, you can perform a smooth subtraction with `:r` or `:s`:

  ```example
  (subtract :r 20
    (ball 100 | move x -50 | shade red)
    (ball 100 | move x +50 | shade sky))
  ```

  ```example
  (subtract :s 20
    (ball 100 | move x -50 | shade red)
    (ball 100 | move x +50 | shade sky))
  ```

  See the docs for `union` and `intersect` for a full explanation of these arguments
  and the difference between them.
  ````
  (assert (not (and r s)) "you can only specify :r or :s, not both")
  (def distance-roundness (zero-to-nil (or distance r s)))
  (def color-roundness (zero-to-nil (or color r s)))
  (boolean shapes
    (if distance-roundness
      (partial smooth-neg-max-distance (typecheck distance-roundness jlsl/type/float))
      neg-max-distance)
    (if color-roundness
      (partial
        (if s smooth-subtract-color-symmetric smooth-union-color)
        (typecheck color-roundness jlsl/type/float))
      (if s sharp-union-color-symmetric sharp-union-color-asymmetric))))

(defn- get-coefficient [value] (typecheck value jlsl/type/float))
(deffn morph [& args]
  ````(morph shape1 amount shape2 [:distance amount] [:color amount])

  Morph linearly interpolates between two shapes.

  ```
  # 50% box, 50% sphere
  (box 50 | morph (ball 50))

  # 75% box, 25% sphere
  (box 50 | morph 0.25 (ball 50))
  ```

  Concretely this means that it returns a new shape whose individual fields
  are linear interpolations of its inputs. With an anonymous `amount` coefficient,
  both the distance and color fields will be interpolated with the same value.
  But you can also specify per-field overrides:

  ```
  # distance is a 50% blend, but the color is 90% red
  (box 50 | color [1 0 0] | morph :color 0.1 (ball 50 | color [0 1 0]))
  ```
  ````
  (def field-coefficients @{})
  (var default-coefficient nil)
  (var key (next args))
  (def shapes @[])

  (while (not= key nil)
    (var arg (@in args key))
    (cond
      (shape? arg) (array/push shapes arg)
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
