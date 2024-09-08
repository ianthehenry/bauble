(use ./import)

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

(defmacro- make-tiler [name variable loop-body]
  ~(defn- ,name [$index distance-field size limit sample-from sample-to] (sugar (jlsl/do
  (var size size)
  (var base-index (,variable | safe-div size | round))
  (var look-direction (,variable - (size * base-index) | sign))
  ,['unquote ['splice ~(if limit [(jlsl/statement (var limit limit))] [])]]
  (var start-logical (sample-from * look-direction + base-index))
  (var end-logical (sample-to * look-direction + base-index))
  ,['unquote ['splice ~(if limit
    [(jlsl/statement (set start-logical (clamp start-logical (- limit) limit)))
     (jlsl/statement (set end-logical (clamp end-logical (- limit) limit)))]
    [])]]

  (var start (min start-logical end-logical))
  (var end (max start-logical end-logical))

  (var result 1e20)
  ,loop-body
  result))))

(make-tiler tile-distance-2d q
  (for (var y start.y) (<= y end.y) (++ y)
    (for (var x start.x) (<= x end.x) (++ x)
      (with [$index [x y] q (- q (size * $index))]
        (set result (min result distance-field))))))

(make-tiler tile-distance-3d p
  (for (var z start.z) (<= z end.z) (++ z)
    (for (var y start.y) (<= y end.y) (++ y)
      (for (var x start.x) (<= x end.x) (++ x)
        (with [$index [x y z] p (- p (size * $index))]
          (set result (min result distance-field)))))))

(defn- simple-tile [coord $index expr size limit]
  (sugar (gl/let [size size]
    (gl/with [$index (let [index (coord | safe-div size | round)]
                        (if limit
                          (gl/let [limit limit]
                            (index | clamp (- limit) limit))
                          index))
              coord (- coord (size * $index))]
      expr))))

# TODO: oversample should be a flag; you shouldn't have to pass a boolean?
# TODO: callback
# TODO: mapping function
(defnamed tile [shape size :?limit :?oversample :?sample-from :?sample-to]
  ```
  Repeat the region of space `size` units around the origin. Pass `:limit` to constrain
  the number of repetitions.

  `shape` can be a single shape or a callback that returns a shape.

  The callback takes the original shape and `index`, which is a vector of the same dimension
  as the shape, i.e. in 3D, the shape at the origin has an index of `[0 0 0]` and the shape
  above it has an index of `[0 1 0]`.

  (Protip: you can use `(hash index)` in the callback to produce pseudorandom variations of shapes.)

  To repeat space only along some axes, pass `0`. For example, `(tile (sphere 50) [0 100 0])` will
  only tile in the `y` direction.

  If you're repeating a shape that is not symmetric, you can use `:oversample true` to evaluate
  multiple instances at each pass, essentially considering the distance not only to this
  tile, but also to neighboring tiles.

  The default oversampling is `[0 0 0]` `[1 1 1]`, which means looking at one adjacent
  tile, asymmetrically based on the location of the point (so when evaluating a point near
  the right edge of a tile, it will look at the adjacent tile to the right, but not the tile
  to the left). By passing `:sample-from [-1 -1 -1]`, you can also look at the tile to the left.
  By passing `:sample-from [0 0 0] :sample-to [2 2 2]`, it will look at two tiles to the right.

  This can be useful when raymarching a 3D space where each tile is quite different, but note
  that it's very costly to increase these values. If you're tiling a 3D shape in all directions,
  the default `:oversample` parameters will do 8 distance field evaluations;
  `:sample-from [-1 -1 -1]` `:sample-to [1 1 1]` will do 27.
  ```
  (def size (typecheck size (shape/type shape)))
  (def limit (if limit (typecheck limit (shape/type shape))))
  (def [coord tile-function default-sample-from default-sample-to]
    (case (shape/type shape)
      jlsl/type/vec2 [q tile-distance-2d [0 0] [1 1]]
      jlsl/type/vec3 [p tile-distance-3d [0 0 0] [1 1 1]]))
  (default sample-from default-sample-from)
  (default sample-to default-sample-to)

  (def $index (jlsl/variable/new "index" (shape/type shape)))

  (if oversample
    (shape/map-fields shape (fn [name expr]
      (case name
        :distance (tile-function $index expr size limit sample-from sample-to)
        (simple-tile coord $index expr size limit))))
    (shape/map shape (fn [expr]
      (simple-tile coord $index expr size limit)))))
