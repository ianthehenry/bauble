(use ./import)
(use ./transforms)

# values too large create bizarre results
# on my iphone
(def- VERY_LARGE_DISTANCE 1e6)

(defhelper- :vec3 safe-div [:vec3 a :vec3 b]
  (return [
    (if (= b.x 0) 0 (a.x / b.x))
    (if (= b.y 0) 0 (a.y / b.y))
    (if (= b.z 0) 0 (a.z / b.z))
    ]))

(overload :vec2 safe-div [:vec2 a :vec2 b]
  (return [
    (if (= b.x 0) 0 (a.x / b.x))
    (if (= b.y 0) 0 (a.y / b.y))
    ]))

(defmacro- make-tiler [name variable & loop-body]
  ~(defn- ,name [$index distance-field other-field size limit sample-from sample-to]
    (sugar (if limit
      (jlsl/do
        (var size size)
        (var base-index (,variable | safe-div size | round))
        (var look-direction (,variable - (size * base-index) | sign))
        (var limit limit)
        (var limit-start (limit - 1 / 2 | floor -))
        (var limit-end (limit / 2 | floor))
        (var start-logical (sample-from * sign size * look-direction + base-index))
        (var end-logical (sample-to * sign size * look-direction + base-index))
        (set start-logical (clamp start-logical limit-start limit-end))
        (set end-logical (clamp end-logical limit-start limit-end))
        (var start (min start-logical end-logical))
        (var end (max start-logical end-logical))
        ,;loop-body)
      (jlsl/do
        (var size size)
        (var base-index (,variable | safe-div size | round))
        (var look-direction (,variable - (size * base-index) | sign))
        (var start-logical (sample-from * sign size * look-direction + base-index))
        (var end-logical (sample-to * sign size * look-direction + base-index))
        (var start (min start-logical end-logical))
        (var end (max start-logical end-logical))
        ,;loop-body)))))

(make-tiler tile-distance-2d q
  (var nearest VERY_LARGE_DISTANCE)
  (for (var y start.y) (<= y end.y) (++ y)
    (for (var x start.x) (<= x end.x) (++ x)
      (with [$index [x y] q (- q (size * $index))]
        (set nearest (min nearest distance-field)))))
  nearest)

(make-tiler tile-distance-3d p
  (var nearest VERY_LARGE_DISTANCE)
  (for (var z start.z) (<= z end.z) (++ z)
    (for (var y start.y) (<= y end.y) (++ y)
      (for (var x start.x) (<= x end.x) (++ x)
        (with [$index [x y z] p (- p (size * $index))]
          (set nearest (min nearest distance-field))))))
  nearest)

(make-tiler tile-other-2d q
  (var nearest VERY_LARGE_DISTANCE)
  (var nearest-index [0 0])
  (for (var y start.y) (<= y end.y) (++ y)
    (for (var x start.x) (<= x end.x) (++ x)
      (with [$index [x y] q (- q (size * $index))]
        (var dist distance-field)
        (if (< dist nearest) (do
          (set nearest dist)
          (set nearest-index $index))))))
  (with [$index nearest-index q (- q (size * $index))]
    other-field))

(make-tiler tile-other-3d p
  (var nearest VERY_LARGE_DISTANCE)
  (var nearest-index [0 0 0])
  (for (var z start.z) (<= z end.z) (++ z)
    (for (var y start.y) (<= y end.y) (++ y)
      (for (var x start.x) (<= x end.x) (++ x)
        (with [$index [x y z] p (- p (size * $index))]
          (var dist distance-field)
          (if (< dist nearest) (do
            (set nearest dist)
            (set nearest-index $index)))))))
  (with [$index nearest-index p (- p (size * $index))]
    other-field))

(sugar (defn- simple-tile [coord $index expr size limit]
  (gl/let [size size]
    (def index-expr (let [index (coord | safe-div size | round)]
        (if limit
          (gl/let [limit limit]
            (index | clamp (limit - 1 / 2 | floor -) (limit / 2 | floor)))
          index)))
      (gl/with [$index index-expr
                coord (- coord (* size $index))]
        expr))))

(defn- tile-aux [shape $index size limit oversample sample-from sample-to]
  (def size (coerce-to-domain size shape))
  (def limit (if limit (coerce-to-domain limit shape)))
  (def [coord tile-distance tile-other]
    (case (shape/type shape)
      jlsl/type/vec2 [q tile-distance-2d tile-other-2d]
      jlsl/type/vec3 [p tile-distance-3d tile-other-3d]))
  (def sample-from (coerce-to-domain (or sample-from 0) shape))
  (def sample-to (coerce-to-domain (or sample-to 1) shape))

  (if oversample
    (shape/map-fields shape (fn [name expr]
      (case name
        :distance (tile-distance $index expr nil size limit sample-from sample-to)
        (tile-other $index (shape/distance shape) expr size limit sample-from sample-to))))
    (shape/map shape (fn [expr]
      (simple-tile coord $index expr size limit)))))

(defnamed tile [shape size :?limit :?oversample :?sample-from :?sample-to]
  ````
  Repeat the region of space `size` units around the origin. Pass `:limit` to constrain
  the number of repetitions. See `tile:` or `tile*` if you want to produce a shape that
  varies as it repeats.

  To repeat space only along some axes, pass `0`. For example, to only tile in the `y` direction:

  ```example
  (tile (ball 50) [0 100 0])
  ```

  If you're repeating a shape that is not symmetric, you can use `:oversample true` to evaluate
  multiple instances at each pass, essentially considering the distance not only to this
  tile, but also to neighboring tiles. Compare these two distance fields:

  ```example
  (rect 30 | rotate 0.3 | tile [80 80] :oversample false)
  ```
  ```example
  (rect 30 | rotate 0.3 | tile [80 80] :oversample true)
  ```

  The default oversampling is `:sample-from 0` `:sample-to 1`, which means looking at one adjacent
  tile, asymmetrically based on the location of the point (so when evaluating a point near
  the right edge of a tile, it will look at the adjacent tile to the right, but not the tile
  to the left). By passing `:sample-from -1`, you can also look at the tile to the left.
  By passing `:sample-from 0 :sample-to [2 1 1]`, it will look at two tiles to the right in the
  `x` direction, and one tile up/down/in/out.

  This can be useful when raymarching a 3D space where each tile is quite different, but note
  that it's very costly to increase these values. If you're tiling a 3D shape in all directions,
  the default `:oversample` parameters will do 8 distance field evaluations;
  `:sample-from -1` `:sample-to 1` will do 27.
  ````
  (def $index (jlsl/variable/new "tile-index" (shape/type shape)))
  (tile-aux shape $index size limit oversample sample-from sample-to))

(defnamed tile* [size get-shape :?limit :?oversample :?sample-from :?sample-to]
  ````
  Like `tile`, but the shape is a result of invoking `get-shape` with one argument,
  a GLSL variable referring to the current index in space. Unlike `tile`, `size` must
  be a vector that determines the dimension of the index variable.

  ```example
  (tile* [10 10] (fn [$i] 
    (circle 5 
    | color (hsv (hash $i) 0.5 1))))
  ```

  You can use this to generate different shapes or colors at every sampled tile. The
  index will be a vector with integral components that represents the current tile
  being evaluated. So in 3D, the shape at the origin has an index of `[0 0 0]` and
  the shape above it has an index of `[0 1 0]`.

  See also `tile:`, which is a more convenient macro version of this function.
  ````
  (def size (jlsl/coerce-expr size))
  (def $index (jlsl/variable/new "tile-index" (jlsl/expr/type size)))
  (def shape (get-shape $index))
  (tile-aux shape $index size limit oversample sample-from sample-to))

(defmacro tile:
  ````
  Like `tile*`, but its first argument should be a form that will
  become the body of the function. Basically, it's a way to create
  a repeated shape where each instance of the shape varies, and it's
  written in a way that makes it conveniently fit into a pipeline:

  ```example
  (circle 5 
  | color (hsv (hash $i) 0.5 1) 
  | tile: $i [10 10])
  ```
  ````
  [shape $i & args]
  ~(,tile* ,;args (fn [,$i] ,shape)))

(sugar (defn- simple-radial [coord plane rotate $index expr count]
  (gl/let [count count angular-size (tau / count)]
    (gl/with [$index (atan2 plane + (angular-size * 0.5) | mod tau / angular-size | floor)
              coord (rotate coord (* -1 angular-size $index))]
      expr))))

(defmacro- make-radiator [name & loop-body]
  ~(defn- ,name [$index plane rotate distance-field other-field count sample-from sample-to]
    (sugar (jlsl/do
      (var count count)
      (var angular-size (tau / count))
      (var angle (atan2 plane + (angular-size * 0.5) | mod tau / angular-size))
      (var base-index (floor angle))
      (var look-direction (angle | fract - 0.5 | sign))
      (var start-logical (sample-from * look-direction + base-index))
      (var end-logical (sample-to * look-direction + base-index))
      (var start (min start-logical end-logical))
      (var end (max start-logical end-logical))
      ,;loop-body))))

(make-radiator radial-distance-2d
  (var nearest VERY_LARGE_DISTANCE)
  (for (var i start) (<= i end) (++ i)
    (with [$index i q (rotate q (* -1 angular-size i))]
      (set nearest (min nearest distance-field))))
  nearest)

(make-radiator radial-other-2d
  (var nearest VERY_LARGE_DISTANCE)
  (var nearest-index 0)
  (for (var i start) (<= i end) (++ i)
    (with [$index i q (rotate q (* -1 angular-size i))]
      (var dist distance-field)
      (if (< dist nearest) (do
        (set nearest dist)
        (set nearest-index $index)))))
  (with [$index (mod nearest-index count) q (rotate q (* -1 angular-size $index))]
    other-field))

(make-radiator radial-distance-3d
  (var nearest VERY_LARGE_DISTANCE)
  (for (var i start) (<= i end) (++ i)
    (with [$index i p (rotate p (* -1 angular-size i))]
      (set nearest (min nearest distance-field))))
  nearest)

(make-radiator radial-other-3d
  (var nearest VERY_LARGE_DISTANCE)
  (var nearest-index 0)
  (for (var i start) (<= i end) (++ i)
    (with [$index i p (rotate p (* -1 angular-size i))]
      (var dist distance-field)
      (if (< dist nearest) (do
        (set nearest dist)
        (set nearest-index $index)))))
  (with [$index (mod nearest-index count) p (rotate p (* -1 angular-size $index))]
    other-field))

(defn- split-axis [axis]
  (sugar (case axis
    x [y p.yz]
    y [z p.zx]
    z [x p.xy]
    (errorf "unknown axis %q" (jlsl/show axis)))))

(defn- radial-aux [shape axis $index count offset oversample sample-from sample-to]
  (def [coord plane offset count rotate radial-distance radial-other]
    (case (shape/type shape)
      jlsl/type/vec2 (do
        (def actual-count (if (nil? axis) count axis))
        (def actual-offset (if (nil? axis) nil (typecheck count jlsl/type/float)))
        (assert (nil? offset) "cannot supply an axis with a 2D shape")
        [q q (if actual-offset [actual-offset 0]) actual-count rotate radial-distance-2d radial-other-2d])
      jlsl/type/vec3 (do
        (def [offset-axis other-axes] (split-axis axis))
        [p other-axes (if offset (* (typecheck offset jlsl/type/float) offset-axis)) count
         (fn [p angle] (rotate p axis angle)) radial-distance-3d radial-other-3d])))
  (def count (typecheck count jlsl/type/float))
  (def sample-from (typecheck (or sample-from 0) jlsl/type/float))
  (def sample-to (typecheck (or sample-to 1) jlsl/type/float))

  (def shape (if offset (move shape offset) shape))

  (if oversample
    (shape/map-fields shape (fn [name expr]
      (case name
        :distance (radial-distance $index plane rotate expr nil count sample-from sample-to)
        (radial-other $index plane rotate (shape/distance shape) expr count sample-from sample-to))))
    (shape/map shape (fn [expr]
      (simple-radial coord plane rotate $index expr count)))))

(defnamed radial [shape ?axis count ?offset :?oversample :?sample-from :?sample-to]
  ````
  Repeat an angular slice of space `count` times around the given axis.

  ```example
  (torus x 100 1 | radial y 24)
  ```

  With an offset argument, you can translate the shape away from the origin first:

  ```example
  (torus x 100 1 | radial y 24 (osc t 5 0 120))
  ```

  If you're repeating a shape that is not symmetric, you can use `:oversample true` to evaluate
  multiple instances at each pass, essentially considering the distance not only to this
  slice, but also to neighboring slices. Compare these two distance fields:

  ```example
  (triangle [50 100] | radial 12 100)
  ```
  ```example
  (triangle [50 100] | radial 12 100 :oversample true)
  ```

  The default oversampling is `:sample-from 0` `:sample-to 1`, which means looking at one adjacent
  slice, asymmetrically based on the location of the point (so when evaluating a point near
  the right edge of a slice, it will look at the slice to the right, but not the slice
  to the left). By passing `:sample-from -1`, you can also look at the "far" slice.
  By passing `:sample-from 0 :sample-to 2`, you can look at two slices in the direction of
  the nearest slice.

  This can be useful when raymarching a 3D space where each slice produces a different shape, or
  where the shape you're marching doesn't fit into a single slice. For example:

  ```example
  (cone y 25 100 :r 1
  | radial z 12 100 :oversample true :sample-from -1)
  ```
  ````
  (def $index (jlsl/variable/new "radial-index" jlsl/type/float))
  (radial-aux shape axis $index count offset oversample sample-from sample-to))

(defnamed radial* [?axis count ?offset get-shape :?oversample :?sample-from :?sample-to]
  ````
  Like `radial`, but the shape is a result of invoking `get-shape` with one argument,
  a GLSL variable referring to the current slice of space.

  ```example
  (radial* z 12 100 (fn [$i]
    (ball 50
    | color (hsv (hash $i) 0.5 1))))
  ```

  You can use this to generate different shapes or colors at every sampled slice of space.
  The index will be a `float` with integral components that represents the current slice
  being considered.

  See also `radial:`, which is a more convenient macro version of this function.
  ````
  (def $index (jlsl/variable/new "radial-index" jlsl/type/float))
  (def shape (get-shape $index))
  (radial-aux shape axis $index count offset oversample sample-from sample-to))

(defmacro radial:
  ````
  Like `radial*`, but its first argument should be a form that will
  become the body of the function. Basically, it's a way to create
  a repeated shape where each instance of the shape varies, and it's
  written in a way that makes it conveniently fit into a pipeline:

  ```example
  (ball 50
  | color (hsv (hash $i) 0.5 1)
  | radial: $i z 12 100)
  ```
  ````
  [shape $i & args]
  ~(,radial* ,;args (fn [,$i] ,shape)))
