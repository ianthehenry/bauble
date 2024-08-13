(use ./constants)

# it's faster to allocate an array than a tuple, and look, i'm not
# happy about this either
(defn vec+ [[x1 y1 z1] [x2 y2 z2]] @[(+ x1 x2) (+ y1 y2) (+ z1 z2)])
(defn vec- [[x1 y1 z1] [x2 y2 z2]] @[(- x1 x2) (- y1 y2) (- z1 z2)])
(defn vec* [[x1 y1 z1] [x2 y2 z2]] @[(* x1 x2) (* y1 y2) (* z1 z2)])
(defn vec*s [[x1 y1 z1] s] @[(* x1 s) (* y1 s) (* z1 s)])
(defn vec/ [[x1 y1 z1] [x2 y2 z2]] @[(/ x1 x2) (/ y1 y2) (/ z1 z2)])

# like for, but unrolled at compile time
(defmacro for! [sym start end & body]
  (assert (and (number? start) (number? end)))
  ['upscope
    ;(catseq [i :range [start end]]
      (prewalk |(if (= $ sym) i $) body))])

(defmacro blshift! [x y]
  ~(comptime (blshift ,x ,y)))

(defmacro bor= [sym expr]
  ~(set ,sym (,bor ,sym ,expr)))

(defn lerp [left right amount-right]
  (+ (* left (- 1 amount-right))
     (* right amount-right)))

(defn vertex-interpolate [[p1 v1] [p2 v2]]
  # so this looks pretty good. but we could also consider
  # keeping these samples in their center, then translating
  # each point along their normals. but that could cause us to
  # generate self-intersecting geometry...
  (vec+ p1 (vec*s (vec- p2 p1) (/ (- v1) (- v2 v1)))))

# vertices is an array of 3d points
# triangles is an array of indexes into the vertices array
#
# TODO: we should actually really keep a global lookup table of
# *absolute* edge positions into the vertices array, so that
# even across different cubes we can reference the same vertex.
# we basically need something like that for reusing samples anyway?
(defn process-cube [sample vertices triangles]
  (var cube-index 0)
  (for! corner 0 8
    (when (>= ((sample corner) 1) 0)
      (bor= cube-index (blshift! 1 corner))))
  (def triangle-table (in triangle-table cube-index))
  (when (empty? triangle-table) (break))

  (def edge-mask (in cube-table cube-index))

  (def edge-pos-sparse (array/new-filled 12 nil))
  (for! edge 0 12
    (when (band edge-mask (blshift! 1 edge))
      (def [start-corner end-corner] (in edge-to-corners edge))
      (put edge-pos-sparse edge
        (vertex-interpolate (sample start-corner) (sample end-corner)))))

  (def edge-to-vertex-index (array/new-filled 12 nil))

  (loop [edge :in triangle-table :unless (in edge-to-vertex-index edge)]
    (array/push vertices (in edge-pos-sparse edge))
    (put edge-to-vertex-index edge (length vertices)))

  (def len (length triangle-table))
  (var i 0)
  (while (< i len)
    (array/push triangles
      @[(edge-to-vertex-index (triangle-table i))
        (edge-to-vertex-index (triangle-table (+ i 1)))
        (edge-to-vertex-index (triangle-table (+ i 2)))])
    (+= i 3))
)

# the SDF for a sphere of radius 4 centered around the origin
(defn sample [[x y z]]
  (- (math/sqrt (+ (* x x) (* y y) (* z z))) 4))

# the SDF for two interlocking tori of radius 4 centered around the origin
(defn sample [[x y z]]
  (def torus-one (do
    (def one (- (math/sqrt (+ (* x x) (* y y))) 3))
    (- (math/sqrt (+ (* one one) (* z z))) 1.5)
    ))

  (def x (- x 3))
  (def torus-two (do
    (def one (- (math/sqrt (+ (* x x) (* z z))) 3))
    (- (math/sqrt (+ (* one one) (* y y))) 1.5)
    ))

  (def r 0.4)
  (def h (max (min (+ 0.5 (* (/ (- torus-two torus-one) r) 0.5)) 1) 0))
  (- (lerp torus-two torus-one h) (* r h (- 1 h)))
  )

(defn sample [[x y z]]
  (def s (/ 2 3))
  (def sphere (- (math/sqrt (+ (* x x) (* y y) (* z z))) 14.5))
  (def gyroid (let [x (* x s) y (* y s) z (* z s)]
    (- (+ (* (math/cos x) (math/sin y)) (* (math/cos y) (math/sin z)) (* (math/cos z) (math/sin x))) -1)))

  (def r 0.25)
  (def h (max (min (- 0.5 (* (/ (- gyroid sphere) r) 0.5)) 1) 0))
  (+ (lerp gyroid sphere h) (* r h (- 1 h)))
  )

(defn main [&]
  (def samples @[])

  # this is in the domain of the function
  #(def bounds [[-5 -5 -5] [8 5 5]])
  (def bounds [[-15 -15 -15] [15 15 15]])
  (def slices 120)
  #(def slices [(* slices (/ 13 10)) slices slices])
  (def slices [slices slices slices])
  (def size (vec- (bounds 1) (bounds 0)))
  # TODO: can step-size be non-rectangular?
  (def step-size (vec/ size slices))

  # this is in indices of cube corners
  # TODO: assert that these are integers
  (def bounds [(vec/ (bounds 0) step-size) (vec/ (bounds 1) step-size)])
  (def [xs ys zs] slices)
  (assert (and (>= xs 2) (>= ys 2) (>= zs 2)) "cannot slice without any cubes")

  (eprintf "bounds=%q step-size=%q slices=%q" bounds step-size slices)

  (defn absolute-corner-index [[x y z]]
    (+ x (* y xs) (* z xs ys)))

  # TODO: actually not sure how my grid is aligned...
  (defn relative-corner-index [[x y z]]
    (+ x (* 2 y) (* 4 z)))

  (defn corner-position [p]
    (vec* step-size (vec+ (bounds 0) p)))

  (def vertices @[])
  (def triangles @[])

  (var top-slice (array/new (* (inc xs) (inc ys))))
  (var bottom-slice (array/new (* (inc xs) (inc ys))))

  (defn fill-bottom-slice [z]
    (eprin ".")
    (loop [x :range-to [0 xs]
           y :range-to [0 ys]]
      (def p (corner-position [x y z]))
      (put bottom-slice (+ x (* xs y)) [p (sample p)])))
  (defn promote-slice []
    (for i 0 (length bottom-slice)
      (put top-slice i (in bottom-slice i))))
  (fill-bottom-slice 0)

  (defn get-sample [x y relative-corner]
    (def [x-off y-off z-off] relative-corner)
    (def active-slice (if (= z-off 0) top-slice bottom-slice))
    (in active-slice (+ (+ x x-off) (* (+ y y-off) xs))))

  # xyz are always positive
  (loop [z :range [1 zs]
         :before (do (promote-slice) (fill-bottom-slice z))
         y :range [0 ys]
         x :range [0 xs]]
      (process-cube
        (map |(get-sample x y $) corner-offsets)
        vertices
        triangles))
  (eprint "")
  (each [x y z] vertices
    (printf "v %f %f %f" x y z))
  (each [a b c] triangles
    (printf "f %d// %d// %d//" a b c))

  (eprintf "%d vertices, %d faces" (length vertices) (length triangles)))
