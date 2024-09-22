(use ./constants)
(use ./util)
(import ../lib/shape)
(import ../jlsl)
(import ../lib/syntax)

(defmacro sugar [expr] (syntax/expand expr))

(defn vertex-interpolate [p1 v1 p2 v2]
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
(defn process-cube [cube-origin cube-size space-origin sample vertices triangles]
  (var cube-index 0)
  (for! corner 0 8
    (when (>= (sample corner) 0)
      (bor= cube-index (blshift! 1 corner))))
  (def triangle-table (in triangle-table cube-index))
  (when (empty? triangle-table) (break))

  (def edge-mask (in cube-table cube-index))

  (def edge-pos-sparse (array/new-filled 12 nil))
  (for! edge 0 12
    (when (band edge-mask (blshift! 1 edge))
      (def [start-corner end-corner] (in edge-to-corners edge))
      (put edge-pos-sparse edge
        (vertex-interpolate
          (vec+ space-origin (vec* cube-size (vec+ cube-origin (corner-offsets start-corner))))
          (sample start-corner)
          (vec+ space-origin (vec* cube-size (vec+ cube-origin (corner-offsets end-corner))))
          (sample end-corner)))))

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

(defn march [origin cube-size [xs ys zs]] (coro
  (def vertices @[])
  (def triangles @[])

  (var top-slice (array/new (* xs ys)))
  (var bottom-slice (array/new (* xs ys)))

  (defn fill-bottom-slice [z]
    (set bottom-slice (yield z)))
  (defn promote-slice []
    (for i 0 (length bottom-slice)
      (put top-slice i (in bottom-slice i))))
  (fill-bottom-slice 0)

  (defn get-sample [x y [dx dy dz]]
    (def active-slice (if (= dz 0) top-slice bottom-slice))
    (in active-slice (+ (+ x dx) (* (+ y dy) xs))))

  # xyz are always positive
  (loop [z :range [1 (dec zs)]
         :before (do (promote-slice) (fill-bottom-slice z))
         y :range [0 (dec ys)]
         x :range [0 (dec xs)]]
      (process-cube
        [x y z]
        cube-size
        origin
        (map |(get-sample x y $) corner-offsets)
        vertices
        triangles))
  [vertices triangles]))

(use ../lib/environment/prelude)

(defn get-program [env]
  (def stdenv (table/getproto env))
  (def subject (get-var stdenv 'subject))
  (def nearest-distance (get-env stdenv 'nearest-distance))
  (assert (shape/shape? subject) "subject is not a shape")
  (case (shape/type subject)
    jlsl/type/vec2 (error "cannot export 2D shapes... yet...?")
    jlsl/type/vec3 3
    (error "BUG"))

  # even though we only care about a single value,
  # raylib always creates RGBA render buffers
  (def program (sugar (jlsl/program/new
    (precision highp float)
    (uniform :vec3 cube-size)
    (uniform ,t)
    (uniform :int z)
    (uniform :vec3 origin)
    (out :vec4 frag-color)
    (implement :float nearest-distance [] (return ,(shape/distance subject)))

    (defn :void main []
      (var pixel (floor gl-frag-coord.xy))
      (var logical [pixel (float z)])
      (with [P (logical * cube-size + origin) p P]
        (set frag-color [(vec3 (nearest-distance)) 1])))))))
