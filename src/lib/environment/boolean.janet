(use ./import)

(defn- sharp-union [& shapes]
  (field-set/merge shapes (fn [fields]
    (def distances (seq [{:distance d} :in fields :when d] d))
    (def colors (seq [{:color c :distance d} :in fields :when c] [c (@or d (jlsl/coerce-expr 0))]))

    {:distance (case (@length distances)
       0 nil
       1 (distances 0)
       (jlsl/do "union-distance"
         (var nearest ,(first distances))
         ,;(seq [expr :in (drop 1 distances)]
           (jlsl/statement (set nearest (min nearest ,expr))))
         nearest))

     :color (case (@length colors)
       0 nil
       1 ((colors 0) 0)
       (jlsl/iife "union-color"
         ,;(seq [[c d] :in (reverse (drop 1 colors))]
           (jlsl/statement
             (if (< ,d 0)
               (return ,c))))
         ,((colors 0) 0)
         ))}
    )))

# TODO: we should probably have a way to do this
(defn- symmetric-color-union [r fields]
  (def colors (seq [{:color c :distance d} :in fields :when c] [c (@or d (jlsl/coerce-expr 0))]))
  (case (@length colors)
    0 nil
    1 ((colors 0) 0)
    (jlsl/do "smooth-union-color"
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
  (field-set/merge shapes (fn [fields]
    (def distances (seq [{:distance d} :in fields :when d] d))
    {:distance (case (@length distances)
       0 nil
       1 (distances 0)
       (jlsl/do "smooth-union-distance"
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
