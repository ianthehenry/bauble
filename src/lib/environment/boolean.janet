(use ./import)

(defn- make-boolean [reduce-distances reduce-colors]
  (fn [& shapes]
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
          (reduce-colors colors))}))))

(defn- min-distance [distances]
  (jlsl/do "union-distance"
     (var nearest ,(first distances))
     ,;(seq [expr :in (drop 1 distances)]
       (jlsl/statement (set nearest (min nearest ,expr))))
     nearest))

(defn- sharp-union-color [colors]
  (def surface-id
    (jlsl/iife "union-color-index"
      (var nearest 1e10)
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

(def- sharp-union (make-boolean min-distance sharp-union-color))

# TODO: we should probably have a way to do this
(defn- symmetric-color-union [r fields]
  (def colors (seq [{:color c :distance d} :in fields :when c] [c (@or d (jlsl/coerce-expr 0))]))
  (case (@length colors)
    0 nil
    1 ((colors 0) 0)
    (jlsl/do "smooth-union-color"
      (var r ,r)
      (var color [0 0 0])
      (var nearest 1000000)
      ,;(seq [[color-expr dist-expr] :in colors]
        (jlsl/statement
          (var dist ,dist-expr)
          (var contribution (remap+ (clamp (/ (- nearest dist) r) -1 1)))
          (set nearest (- (mix nearest dist contribution) (* r contribution (- 1 contribution))))
          (if (> contribution 0) (set color (mix color ,color-expr contribution)))
        ))
      color)))

(defn- asymmetric-color-union [r fields]
  (def colors (seq [{:color c :distance d} :in fields :when c] [c (@or d (jlsl/coerce-expr 0))]))
  (case (@length colors)
    0 nil
    1 ((colors 0) 0)
    (jlsl/do "smooth-union-color"
      (var r ,r)
      (var color [0 0 0])
      (var nearest 1000000)
      ,;(seq [[color-expr dist-expr] :in colors]
        (jlsl/statement
          (var dist ,dist-expr)
          # TODO: wait, what, do we need h here, what is happening
          (var contribution (- 1 (remap+ (clamp (/ (min dist (- dist nearest)) r) -1 1))))
          (var h (remap+ (clamp (/ (- nearest dist) r) -1 1)))
          (set h contribution)
          (set nearest (- (mix nearest dist h) (* r h (- 1 h))))
          (if (> contribution 0) (set color (mix color ,color-expr contribution)))
        ))
      color)))

(defn smooth-union [r & shapes]
  (def r (typecheck (jlsl/coerce-expr r) jlsl/type/float))
  (shape/merge shapes (fn [fields]
    (def distances (seq [{:distance d} :in fields :when d] d))
    {:distance (case (@length distances)
       0 nil
       1 (distances 0)
       (jlsl/do "smooth-union-distance"
         (var r ,r)
         (var nearest ,(first distances))
         ,;(seq [expr :in (drop 1 distances)]
           (jlsl/statement
             (var dist ,expr)
             (var h (remap+ (clamp (/ (- nearest dist) r) -1 1)))
             (set nearest (- (mix nearest dist h) (* r h (- 1 h))))))
         nearest))

     # TODO
     #:color (symmetric-color-union r shapes)
     :color (asymmetric-color-union r fields)}
    )))

# TODO: this should have an optional radius argument
(defn union
  "Join 'em up. Do it to it."
  [& shapes]
  (sharp-union ;shapes))

(defn- get-coefficient [value] (typecheck (jlsl/coerce-expr value) jlsl/type/float))
(def morph
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
  (fn morph [& args]
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
        (mix a b (@in field-coefficients field default-coefficient))) fields)))))
