(use ./import)
(use ./rotation)

(steal ./rotation rotation-matrix-2d)
(steal ./rotation rotation-matrix-3d)

(defn- rotate-shape [shape args]
  (case (shape/type shape)
    jlsl/type/vec2 (transform shape "rotate" q (* ,(rotation-matrix-2d -1 args) q))
    jlsl/type/vec3 (transform shape "rotate" p (* ,(rotation-matrix-3d -1 args) p))))

(defn- rotate-vector [vector args]
  (def v (jlsl/coerce-expr vector))
  (case (jlsl/expr/type v)
    jlsl/type/vec2 (* (rotation-matrix-2d 1 args) v)
    jlsl/type/vec3 (* (rotation-matrix-3d 1 args) v)
    (errorf "I don't know how to rotate %q" (jlsl/expr/to-sexp v))))

(defn- rotate-camera [camera args]
  # this is goofy, but all arguments to functions in jlsl
  # notation must be jlsl expressions, and args is not
  (sugar (gl/let [camera camera]
    (def new-direction (rotate-vector camera.direction args))
    (gl/do
      (set camera.direction new-direction)
      camera))))

(defn- align-shape [shape from to]
  (transform shape "align" p (* (alignment-matrix to from) p)))

(defn- align-vector [vector from to]
  (def v (typecheck vector jlsl/type/vec3))
  jlsl/type/vec3 (* (alignment-matrix from to) v))

(defn rotate
  ````
  Rotate a shape or a vector. Positive angles are counter-clockwise rotations.

  In 3D, the arguments should be pairs of `axis angle`. For example:

  ```example
  (rotate (box 100) x t y (sin t))
  ```

  All `axis` arguments must be unit vectors. There are built-in axis variables `x`/`+y`/`-z`
  for the cardinal directions, and these produce optimized rotation matrices. But you can
  rotate around an arbitrary axis:

  ```example
  (rotate (box 100) [1 1 1 | normalize] t)
  ```

  The order of the arguments is significant, as rotations are not commutative.

  The first argument to `rotate` can be a shape, vector, or camera.

  In 2D, the arguments should just be angles; no axis is allowed.

  You can use `rotate` to make lots of cool effects. By varying the angle
  of rotation, you can create twists:

  ```example
  (box [50 100 50]
  | rotate y (p.y / 100 * (cos+ t)))
  ```

  Twirls:

  ```example
  (box [100 50 100]
  | rotate y (length p.xz / 50 * (cos+ t)))
  ```

  And bends:

  ```example
  (box [50 100 100]
  | rotate y (p.z / 100 * (cos+ t)))
  ```

  Or any number of other cool effects!

  ```example
  (box [50 100 50]
  | rotate y (sin (p.y / 10) * sin t * 0.2))
  ```
  ````
  [subject & args]
  (assert (> (@length args) 0) "not enough arguments")
  (cond
    (shape? subject) (rotate-shape subject args)
    (camera? subject) (rotate-camera subject args)
    (rotate-vector subject args)))

(defn align
  ````
  Align a shape or a vector to another vector. Both the `from` and `to` vectors must have unit length.

  This function is useful for "pointing" one shape towards another. For example:

  ```example
  (def pos
    [(sin (t * 1.0) * 100)
     (sin (t * 1.5) * 100)
     (cos (t * 2.0) * 100)])
  (union
    (cone y 10 100 | align y (normalize pos))
    (box 10 | move pos))
  ```

  If `from` = `(- to)`, the result is undefined: there are infinitely many rotation matrices that reverse
  a vector's direction.
  ````
  [target from to]
  (if (shape? target)
    (align-shape target from to)
    (align-vector target from to)))

# This is a minor convenience that lets us use the 3D vector
# x/y/-x/-y vectors as arguments to move
# TODO: should we just truncate the vector instead? That's... easier and maybe better?
(defn- coerce-axis-vector [type vector]
  (typecheck
    (if (= type jlsl/type/vec2)
      (case vector
        x [1 0]
        y [0 1]
        -x [-1 0]
        -y [0 -1]
        vector)
      vector)
    type))

# TODO: maybe this should be, like, a vector sum, but you peek forward
# each time to see if the next argument is a scalar? would that be better?
# it's a backwards-compatible change...
(defn- sum-scaled-vectors [dimension args]
  (reduce2 + (seq [[direction scale] :in (partition 2 args)]
    (* (coerce-axis-vector dimension direction) (jlsl/coerce-expr (@or scale 1))))))

(defn move
  ````
  Translate a shape. You can pass a vector offset:

  ```example
  (move (box 50) [0 (sin t * 100) 0])
  ```

  Or a vector and a scalar:

  ```example
  (move (box 50) y (sin t * 100))
  ```

  Which is the same as `(move (box 50) (y * 100))`.

  If you provide multiple vector-scalar pairs, their sum is the final offset:

  ```example
  (move (box 50)
    x (sin t * 100)
    y (cos t * 100)
    -z (sin t * 100))
  ```

  `move` can take a shape, a vector, or a camera.

  If you vary the amount of movement by the current position in space,
  you can distort shapes in various ways:

  ```example
  (box 100 | move x (sin (p.y / 100 * pi) * 30))
  ```

  ```example
  (cylinder y 100 10
  | move y (atan p.x p.z * 10 | sin * (length p | ss 10 100 0 10)))
  ```

  ```example
  (box [100 10 100] | move y (p.xz / 20 | pow 2 | sum) | slow 0.5)
  ```
  ````
  [subject & args]
  (def subject (if (shape? subject) subject (jlsl/coerce-expr subject)))
  (def dimension (cond
   (shape? subject) (shape/type subject)
   (camera? subject) jlsl/type/vec3
   (jlsl/expr/type subject)))
  (def offset (sum-scaled-vectors dimension args))
  (cond
    (shape? subject) (case dimension
      jlsl/type/vec2 (transform subject "move" q (- q offset))
      jlsl/type/vec3 (transform subject "move" p (- p offset))
      (error "BUG"))
    (camera? subject) (sugar (gl/do
      (var camera subject)
      (+= camera.origin offset)
      camera))
    (+ subject offset)))

(defn- map-axes [shape axes f]
  (def mask @[false false false])
  (each axis axes (case axis
    x (put mask 0 true)
    y (put mask 1 true)
    z (put mask 2 true)
    (errorf "unknown axis %q" axis)))
  (if (= (shape/type shape) jlsl/type/vec2)
    (transform shape "map-axes" q ,(vec2
      (if (mask 0) (f (. q x)) (. q x))
      (if (mask 1) (f (. q y)) (. q y))))
    (transform shape "map-axes" p ,(vec3
      (if (mask 0) (f (. p x)) (. p x))
      (if (mask 1) (f (. p y)) (. p y))
      (if (mask 2) (f (. p z)) (. p z))))))

(defnamed mirror [shape :?r &axes]
  ````
  Mirror a shape across one or more axes. Normally this takes the absolute value
  of the coordinates, but if you supply `:r` it will take a biased square root to
  give a smooth mirror effect.

  ```example
  (box 50 | rotate x t y t
  | move x 50
  | mirror x :r (sin t * 20 | max 0))
  ```
  ````
  (if r
    (gl/let [r (typecheck r jlsl/type/float)]
      (map-axes shape axes (fn [x] (sqrt (+ (* x x) (* r r))))))
    (map-axes shape axes abs)))

(defn- product-scaled-vectors [dimension args]
  (reduce2 * (seq [[axis scale] :in (partition 2 args)]
    (if (nil? scale)
      (jlsl/coerce-expr axis) # actually a scalar
      (do
        (def axis (coerce-axis-vector dimension axis))
        (+ (- 1 axis) (* axis (typecheck (@or scale 1) jlsl/type/float))))))))

(defnamed scale [shape &args]
  ````
  Scale a shape. If the scale factor is a float, this will produce an exact
  distance field.

  ```example
  (rect 50 | scale 2)
  ```

  If the scale factor is a vector, space will be distorted by the smallest
  component of that vector, and produce an approximate distance field:

  ```example
  (rect 50 | scale [2 1])
  ```

  With an even number of arguments, `scale` expects `axis amount` pairs.
  Unlike `rotate`, it won't work with arbitrary axes -- you must give it
  a cardinal axis.

  ```example
  (rect 50 | scale x 0.5 y 2)
  ```
  ````
  (def dimension (shape/type shape))
  (def factor (product-scaled-vectors dimension args))
  (def uniform? (= (jlsl/expr/type factor) jlsl/type/float))
  (gl/let [factor factor]
    (map-distance
      (if (= dimension jlsl/type/vec2)
        (transform shape "scale" q (/ q factor))
        (transform shape "scale" p (/ p factor)))
      (if uniform?
        |(* (abs factor) $)
        |(* (min (abs factor)) $)))))

(def pivot
  :macro
  ````(pivot (operation subject & args) point)

  Apply a transformation with a different pivot point. You can combine this with any
  operation, but it's probably most useful with `rotate` and `scale`.

  This is a syntactic transformation, so it requires a particular kind of invocation.
  It's designed to fit into a pipeline, immediately after the operation you want to apply:

  ```example
  # rotate around one corner
  (rect 50 | rotate t | pivot [50 50])
  ```

  This essentially rewrites its argument to:

  ```example
  (gl/let [$pivot [50 50]]
    (rect 50 | move (- $pivot) | rotate t | move $pivot))
  ```
  ````
  (fn pivot [transformation point]
    (assertf (and (ptuple? transformation) (>= (@length transformation) 2)) "%q does not look like something I can transform")
    (def [op subject & args] transformation)
    (with-syms [$pivot]
      ~(as-macro ,gl/let [,$pivot ,point]
        (,move (,op (,move ,subject (- ,$pivot)) ,;args) ,$pivot)))))

(deftransform elongate [shape size]
  ````
  Stretch the center of a shape, leaving the sides untouched.

  ```example
  (cone y 50 100 | elongate [(osc t 3 50) 0 (osc t 6 100)])
  ```

  ```example
  (torus x 50 20 | elongate [(sin+ t * 50) 100 0])
  ```

  ```example
  (rhombus [100 (gl/if (< q.y 0) 100 50)] | elongate [0 (osc t 2 0 20)])
  ```
  ````
  (def size (typecheck size (shape/type shape)))
  (case (shape/type shape)
    jlsl/type/vec2
      (shape/map shape (fn [expr]
        (jlsl/do "elongate"
          (var q-prime (abs q - size))
          (+ (with [q (max q-prime 0 * sign q)] expr)
             (min (max q-prime) 0)))))
    jlsl/type/vec3
      (shape/map shape (fn [expr]
        (jlsl/do "elongate"
          (var p-prime (abs p - size))
          (+ (with [p (max p-prime 0 * sign p)] expr)
             (min (max p-prime) 0)))))
    (error "BUG")))

(defn shell
  ````
  Returns a hollow version of the provided shape (the absolute value of the distance field).

  ```example
  (circle 100 | shell 5)
  ```

  In 3D, it's hard to see the effect without cutting into the result:

  ```example
  (ball 100 | shell 5 | intersect (plane x (osc t 3 0 100)))
  ```
  ````
  [shape &opt thickness]
  (def thickness (typecheck (or thickness 0) jlsl/type/float))
  (shape/map-distance shape (fn [expr] (- (abs expr) (* thickness 0.5)))))

(defn expand
  ````
  Expands the provided shape, rounding corners in the process.

  This is the same as subtracting `amount` from the distance field.
  It's more accurate to say that this "moves between isosurfaces," so
  it may not actually round anything if the provided shape is not an
  exact distance field.

  For example, this produces a nicely expanded shape:

  ```example
  (rect 90 | expand (sin+ t * 30))
  ```

  But this does something weird, because subtraction does not produce
  an exact distance field:

  ```example
  (rect 90
  | subtract (rect 100 | move x 150)
  | expand (sin+ t * 30))
  ```
  ````
  [shape amount]
  (def amount (typecheck amount jlsl/type/float))
  (shape/map-distance shape (fn [expr] (- expr amount))))

(defn slow
  ````
  Scales the shape's distance field, causing the raymarcher to converge more slowly.
  This is useful for raymarching distance fields that vary based on `p` -- shapes
  that don't actually provide an accurate distance field unless you are very close
  to their surfaces. Compare the following examples, with and without `slow`:

  ```example
  (box 100
  | rotate y (p.y / 30)
  | rotate x t)
  ```

  ```example
  (box 100
  | rotate y (p.y / 30)
  | rotate x t
  | slow 0.5)
  ```

  Note however that `slow` will also affect the behavior of anything that depends on a shape's
  distance field, such as smooth boolean operations, morphs, soft shadows, and so on. A future
  version of Bauble may mitigate these effects, but it is the way that it is right now.

  ```example
  # slowing the distance field introduces asymmetry
  # into the smooth union
  (union :r 50
    (ball 100 | move x 75)
    (ball 100 | move x -75 | slow (osc t 4 0.25 1)))
  ```
  ````
  [shape amount]
  (def amount (typecheck amount jlsl/type/float))
  (shape/map-distance shape (fn [expr] (* expr amount))))
