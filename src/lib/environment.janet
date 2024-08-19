(defmacro view
  "A shorthand for `(set subject _)` that fits nicely into pipe notation, e.g. `(sphere 50 | view)`."
  [subject]
  ~(set subject ,subject))

(var subject
  "A variable that determines what Bauble will render.\n\nYou can set this variable explicitly to change your focus, or use the `view` macro to change your focus. If you don't set a subject, Bauble will render the last expression in your script that it knows how to render."
  nil)

(import ../jlsl)
(import ./dynvars :prefix "" :export true)
(use ./util)
(import ./syntax)
(import ./field-set)
(import ../jlsl/prelude :prefix "" :export true)

(defn- typecheck [expr expected]
  (def actual (jlsl/expr/type expr))
  (assertf (= actual expected)
    "type mismatch: expected %q, got %q"
    (jlsl/show-type expected)
    (jlsl/show-type actual))
  expr)

(defmacro- defshape [name bindings docstring & body]
  (assert (string? docstring))
  ~(defn ,name ,docstring ,bindings
    ,;(seq [param :in bindings]
      ~(def ,param (,jlsl/coerce-expr ,param)))
    (field-set/distance-2d (jlsl/do
      ,;(syntax/expand body)))))

(defmacro- deftransform [name bindings docstring & body]
  (assert (string? docstring))
  ~(defn ,name ,docstring ,bindings
    ,;(seq [param :in (drop 1 bindings)]
      ~(def ,param (,jlsl/coerce-expr ,param)))
    ,;(syntax/expand body)))

(defshape circle [r]
  "it a circle"
  (- (length q) r))

(defshape rect [size]
  "it a box"
  (var d (- (abs q) (vec2 ,size)))
  (+ (length (max d 0)) (min (max d.x d.y) 0)))

# TODO: this should either modify p or q, depending on the dimension of the field-set
(deftransform move [field-set offset]
  "translate"
  (typecheck offset (field-set/type field-set))
  (field-set/map field-set (fn [expr]
    (jlsl/with "move" [q (- q offset)] ,expr))))

(deftransform color [field-set color-expression]
  "color"
  (typecheck color-expression jlsl/type/vec3)
  (field-set/with field-set :color color-expression))

(defmacro .
  "Behaves like `.` in GLSL, for accessing components of a vector or struct. Can be combined with swizzling."
  [expr field]
  [jlsl/expr/dot [jlsl/coerce-expr expr] ['quote field]])

(defn- sharp-union [& shapes]
  (def type (get-unique field-set/type shapes (fn [types]
    (if (empty? types)
      (error "empty union expression")
      (error "cannot union 2D and 3D shapes")))))
  (def distances (seq [{:distance d} :in shapes :when d] d))
  (def colors (seq [{:color c :distance d} :in shapes :when c] [c (@or d (jlsl/coerce-expr 0))]))

  (field-set/new type
    :distance (case (@length distances)
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
        ))
    ))

# TODO: this should be called remap+ but JLSL doesn't
# sanitize properly
# remap -1 to +1 into 0 to 1
(jlsl/jlsl/defn :float remap-plus [:float x]
  (return (+ 0.5 (* 0.5 x))))

# TODO: we should probably have a way to do this
(defn- symmetric-color-union [r shapes]
  (def colors (seq [{:color c :distance d} :in shapes :when c] [c (@or d (jlsl/coerce-expr 0))]))
  (case (@length colors)
    0 nil
    1 ((colors 0) 0)
    (jlsl/do "smooth-union-color"
      (var color [0 0 0])
      (var nearest 1000000)
      ,;(seq [[color-expr dist-expr] :in colors]
        (jlsl/statement
          (var dist ,dist-expr)
          (var contribution (remap-plus (clamp (/ (- nearest dist) r) -1 1)))
          (set nearest (- (mix nearest dist contribution) (* r contribution (- 1 contribution))))
          (if (> contribution 0) (set color (mix color ,color-expr contribution)))
        ))
      color)))

(defn- asymmetric-color-union [r shapes]
  (def colors (seq [{:color c :distance d} :in shapes :when c] [c (@or d (jlsl/coerce-expr 0))]))
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
          (var contribution (- 1 (remap-plus (clamp (/ (min dist (- dist nearest)) r) -1 1))))
          (var h (remap-plus (clamp (/ (- nearest dist) r) -1 1)))
          (set h contribution)
          (set nearest (- (mix nearest dist h) (* r h (- 1 h))))
          (if (> contribution 0) (set color (mix color ,color-expr contribution)))
        ))
      color)))

(defn smooth-union [r & shapes]
  (def type (get-unique field-set/type shapes (fn [types]
    (if (empty? types)
      (error "empty union expression")
      (error "cannot union 2D and 3D shapes")))))
  (def r (typecheck (jlsl/coerce-expr r) jlsl/type/float))

  (def distances (seq [{:distance d} :in shapes :when d] d))

  (field-set/new type
    :distance (case (@length distances)
      0 nil
      1 (distances 0)
      (jlsl/do "smooth-union-distance"
        (var nearest ,(first distances))
        ,;(seq [expr :in (drop 1 distances)]
          (jlsl/statement
            (var dist ,expr)
            (var h (remap-plus (clamp (/ (- nearest dist) r) -1 1)))
            (set nearest (- (mix nearest dist h) (* r h (- 1 h))))))
        nearest))

    # TODO
    #:color (symmetric-color-union r shapes)
    :color (asymmetric-color-union r shapes)
    ))

# TODO: this should have an optional radius argument
(defn union
  "Join 'em up. Do it to it."
  [& shapes]
  (sharp-union ;shapes))
