(use ./import)

(defmacro view
  "A shorthand for `(set subject _)` that fits nicely into pipe notation, e.g. `(sphere 50 | view)`."
  [subject]
  ~(set subject ,subject))

(thunk ~(var subject
  "A variable that determines what Bauble will render.\n\nYou can set this variable explicitly to change your focus, or use the `view` macro to change your focus. If you don't set a subject, Bauble will render the last shape in your script."
  nil))

(thunk ~(var aa-grid-size ```
The size of the grid used to sample a single pixel. The total samples per pixel will
be the square of this number. The default value is 1 (no anti-aliasing).
```
  nil))

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
  Translate a shape. Usually you'd use this with a vector offset:

  ```
  (move (box 50) [0 100 0])
  ```

  But you can also provide a vector and a scalar:

  ```
  (move (box 50) y 100)
  ```

  Which is the same as `(move (box 50) (y * 100))`.

  If you provide multiple vector-scalar pairs, their sum is the final offset:

  ```
  (move (box 50) x 100 y 100 -z 20)
  ```

  That is the same as `(move (box 50) (+ (x * 100) (y * 100) (-z * 100)))`.
  ````
  [shape & args]
  (def shape (if (shape/is? shape) shape (jlsl/coerce-expr shape)))
  (def dimension (if (shape/is? shape) (shape/type shape) (jlsl/expr/type shape)))
  (def offset (sum-scaled-vectors dimension args))
  (if (shape/is? shape)
    (case dimension
      jlsl/type/vec2 (transform shape "move" q (- q offset))
      jlsl/type/vec3 (transform shape "move" p (- p offset))
      (error "BUG"))
    (+ shape offset)))

(defn shell
  ```
  Returns a hollow version of the provided shape (the absolute value of the distance field).
  ```
  [shape &opt thickness]
  (default thickness 0)
  (shape/map-distance shape (fn [expr] (- (abs expr) (* thickness 0.5)))))

(defn offset
  ```
  Offsets the provided shape, rounding corners in the process.

  This is the same as subtracting `amount` from the distance. It's more accurate
  to say that this "moves between isosurfaces," so it may not actually
  round anything if the provided shape is not an exact distance field.
  ```
  [shape amount]
  (shape/map-distance shape (fn [expr] (- expr amount))))

(defn slow
  ```
  Scales distances around `shape`, causing the raymarcher to converge more slowly.

  This is useful for raymarching distance fields that vary based on `p` -- shapes
  that don't actually provide an accurate distance field unless you are very close
  to the surface.

  Values larger than 1 will give weird results, and this will slow the render down.
  ```
  [shape amount]
  (shape/map-distance shape (fn [expr] (* expr amount))))

(defn map-distance [shape f]
  ```
  Apply a function `f` to the shape's distance field. `f` should take and return an expression.

  The returned shape has the same dimensions as the input.
  ```
  (shape/map-distance shape (fn [expr]
    (jlsl/do "map-distance"
      (var dist expr)
      (f dist)))))

(deftransform elongate [shape size]
  "Stretch a shape."
  (def size (typecheck size (shape/type shape)))
  (case (shape/type shape)
    jlsl/type/vec2
      (shape/map shape (fn [expr]
        (jlsl/do "elongate"
          (var q-prime (abs q - size))
          (+ (with [q (max q-prime 0)] expr)
             (min (max q-prime) 0)))))
    jlsl/type/vec3
      (shape/map shape (fn [expr]
        (jlsl/do "elongate"
          (var p-prime (abs p - size))
          (+ (with [p (max p-prime 0)] expr)
             (min (max p-prime) 0)))))
    (error "BUG")))

(defhelper :float sum [:vec2 v]
  "Add the components of a vector."
  (return (+ v.x v.y)))
(overload :float sum [:vec3 v] (return (+ v.x v.y v.z)))
(overload :float sum [:vec4 v] (return (+ v.x v.y v.z v.w)))

(defhelper :float product [:vec2 v]
  "Multiply the components of a vector."
  (return (* v.x v.y)))
(overload :float product [:vec3 v] (return (* v.x v.y v.z)))
(overload :float product [:vec4 v] (return (* v.x v.y v.z v.w)))

(def pi "I think it's around three.\n\nNote that there are also values like `pi/4` and `pi/6*5` and related helpers all the way up to `pi/12`. They don't show up in autocomplete because they're annoying, but they're there." math/pi)
(def tau "Bigger than six, but smaller than seven.\n\nNote that there are also values like `tau/4` and `tau/6*5` and related helpers all the way up to `tau/12`.  They don't show up in autocomplete because they're annoying, but they're there." (* 2 math/pi))
(loop [i :range-to [2 12]]
  (put (curenv) (symbol "pi/" i) @{:value (/ pi i)})
  (put (curenv) (symbol "tau/" i) @{:value (/ tau i)}))
(loop [i :in [3 4 6 8 12] j :range [2 i]]
  (put (curenv) (symbol "pi/" i "*" j) @{:value (* (/ pi i) j)})
  (put (curenv) (symbol "tau/" i "*" j) @{:value (* (/ tau i) j)}))

(def r2 (shape/2d (jlsl/coerce-expr 0)))
(def r3 (shape/3d (jlsl/coerce-expr 0)))

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
  ```
  Mirror a shape across one or more axes. Normally this takes the absolute value
  of the coordinates, but if you supply `:r` it will take a biased square root to
  give a smooth mirror effect.
  ```
  (if r
    (gl/let [r (typecheck r jlsl/type/float)]
      (map-axes shape axes (fn [x] (sqrt (+ (* x x) (* r r))))))
    (map-axes shape axes abs)))

(defnamed scale [shape factor]
  ```
  Scale a shape. If the scale factor is a float, this will produce an exact
  distance field. If it's a vector, space will be distorted by the smallest
  component of the vector.
  ```
  (def factor (jlsl/coerce-expr factor))
  (def uniform? (= (jlsl/expr/type factor) jlsl/type/float))
  (gl/let [factor factor]
    (map-distance
      (if (= (shape/type shape) jlsl/type/vec2)
        (transform shape "scale" q (/ q factor))
        (transform shape "scale" p (/ p factor)))
      (if uniform?
        |(* (abs factor) $)
        |(* (min (abs factor)) $)))))

(deftransform color [shape color]
  "Set a shape's color field."
  (shape/with shape :color (typecheck color jlsl/type/vec3)))

(def pivot
  :macro
  ````(pivot (operation subject & args) point)

  Apply a transformation with a different pivot point. You can combine this with any
  operation, but it's probably most useful with `rotate` and `scale`.

  This is a syntactic transformation, so it requires a particular kind of invocation.
  It's designed to fit into a pipeline, immediately after the operation you want to apply:

  ```
  # rotate around one corner
  (rect 30 | rotate t | pivot [30 30])
  ```

  This essentially rewrites its argument to:
  
  ```
  (gl/let [$pivot [30 30]]
    (rect 30 | move (- $pivot) | rotate t | move $pivot))
  ```
  ````
  (fn pivot [transformation point]
    (assertf (and (ptuple? transformation) (>= (@length transformation) 2)) "%q does not look like something I can transform")
    (def [op subject & args] transformation)
    (with-syms [$pivot]
      ~(as-macro ,gl/let [,$pivot ,point]
        (,move (,op (,move ,subject (- ,$pivot)) ,;args) ,$pivot)))))

(setdyn '@+ (table/getproto (dyn '+)))
(setdyn '@- (table/getproto (dyn '-)))
(setdyn '@* (table/getproto (dyn '*)))
(setdyn '@/ (table/getproto (dyn '/)))

(defn ss
  ````
  This is a wrapper around `smoothstep` with a different argument order, which also
  allows the input edges to occur in descending order.

  There are several overloads:

  ```
  (ss x)
  # becomes
  (smoothstep 0 1 x)
  ```

  ```
  (ss x [from-start from-end])
  # becomes
  (if (< from-start from-end)
    (smoothstep from-start from-end x)
    (1 - smoothstep from-end from-start x))
  ``` 

  ```
  (ss x from [to-start to-end])
  # becomes
  (ss x from * (- to-end to-start) + to-start)
  ```
  ````
  [x &opt from-range to-range]
  (when to-range
    (def [to-start to-end] to-range)
    (break
      (cond
        (@and (number? to-start) (= to-start 0)) (sugar (ss x from-range * to-end))
        (@and (number? to-start) (number? to-end))
          (sugar (ss x from-range * (to-end - to-start) + to-start))
        (gl/let [to-start to-start]
          (sugar (ss x from-range * (to-end - to-start) + to-start))))))
  (when from-range
    (def [from-start from-end] from-range)
    (break (if (@and (number? from-start) (number? from-end))
      # if these are both constants, we can decide right here what to do
      (if (< from-start from-end)
        (smoothstep from-start from-end x)
        (- 1 (smoothstep from-end from-start x)))
      (gl/let [from-start from-start from-end from-end]
        (jlsl/do
          (if (< from-start from-end)
            (smoothstep from-start from-end x)
            (- 1 (smoothstep from-end from-start x))))))))
  (smoothstep 0 1 x))

(defn sin+ "Like `sin`, but returns a number in the range `0` to `1`." [x] (remap+ (sin x)))
(defn sin- "Like `sin`, but returns a number in the range `0` to `-1`." [x] (remap- (sin x)))
(defn cos+ "Like `cos`, but returns a number in the range `0` to `1`." [x] (remap+ (cos x)))
(defn cos- "Like `cos`, but returns a number in the range `0` to `-1`." [x] (remap- (cos x)))

(sugar (defn- oscillate [trig x period from to]
  (def sig (trig (tau * x / period)))
  (if (number? from)
    (to - from * sig + from)
    (gl/let [from from] (to - from * sig + from)))))

(def osc ````(osc &opt period lo hi)

Returns a number that oscillates with the given period. There are several overloads:

```
# 0 to 1 to 0 every second
(osc t)

# 0 to 1 to 0 every 10 seconds
(osc t 10)

# 0 to 100 to 0 every 10 seconds
(osc t 10 100)

# 50 to 100 to 50 every 10 seconds
(osc t 10 50 100)
```
````
  (fn osc [x & args]
    (case (@length args)
      0 (osc x 1 0 1)
      1 (osc x (args 0) 0 1)
      2 (osc x (args 0) 0 (args 1))
      3 (oscillate (fn [theta] (- 1 (cos+ theta))) x ;args)
      (error "too many arguments"))
    ))

(def oss ````(oss &opt period lo hi)

Like `osc`, but uses a sine wave instead of a cosine wave,
so the output begins halfway between `lo` and `hi`.
````
  (fn oss [x & args]
    (case (@length args)
      0 (oss x 1 0 1)
      1 (oss x (args 0) 0 1)
      2 (oss x (args 0) 0 (args 1))
      3 (oscillate sin+ x ;args)
      (error "too many arguments"))
    ))
