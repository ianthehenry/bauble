(use ./constants)
(use ./util)
(use ../cli/helpers)

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
    (+= i 3)))

(use ./samples)

(defn write [&]
  (def grid-size 30)
  (def grid-size [grid-size grid-size grid-size])
  (def bounds [[-15 -15 -15] [15 15 15]])
  (def slices (map inc grid-size))
  (def size (vec- (bounds 1) (bounds 0)))
  # TODO: can step-size be non-rectangular?
  (def step-size (vec/ size grid-size))

  # this is in indices of cube corners
  # TODO: assert that these are integers
  (def bounds [(vec/ (bounds 0) step-size) (vec/ (bounds 1) step-size)])
  (def [xs ys zs] slices)
  (assert (and (>= xs 2) (>= ys 2) (>= zs 2)) "cannot slice without any cubes")

  (eprintf "bounds=%q step-size=%q slices=%q" bounds step-size slices)

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

(import jaylib)
(import cmd)
(import ../lib :as bauble)

(cmd/main (cmd/fn
  [infile (required :file)
   outfile (optional :file)
   --size (optional :number 64)]
  (def source (slurp infile))
  (def env (bauble/evaluator/evaluate source))
  (def [shader-source dimension animated? has-custom-camera?] (bauble/shade/compile-shape nil env "330"))
  (init-jaylib)
  (if outfile
    (with-dyns [*out* (file/open outfile :w)]
      (write))
    (write))
  #(def image (render-image shader-source))
  ))
