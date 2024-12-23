(import jaylib)
(use sh)
(use ../src/cli/helpers)
(import ../src/lib :as bauble)

(init-jaylib)

(def physical-resolution [128 128])
(def display-resolution [256 256])

(def zoom 0.75)

(defn string/remove-suffix [str suffix]
  (if (string/has-suffix? suffix str)
    (string/slice str 0 (- (length str) (length suffix)))
    (errorf "suffix not found in %s" str)))

(def test-cases @{
  "morph" `(morph (box 100) (ball 100) 0.5)`

  "wiggler" `
  (torus z 60 30
  | rotate y (p.y * 0.07)
  | move x 50
  | mirror :r 10 x
  | slow 0.25)
  `

  "balloon" `
  (ball [50 100 100]
  | union :r 50 (cylinder y 25 50 | move [0 -100 0])
  | scale y (ss p.y -100 100 1 0.8)
  | radial y 20
  | color normal+
  | tint [1 1 1] (fresnel 5))
  (set camera (camera/perspective [0 -40 384] :dir -z :fov 45))
  `

  "spatial noise" `(ball (perlin p 20 * 10 + 100) | slow 0.9)`
  "spatial noise extreme" `(ball (perlin p 20 * 30 + 80) | slow 0.5)`

  "shell 3d" `(ball 100 | shell 5 | intersect (plane z))`
  "cylinder" `(box 50 | subtract (cylinder z 30 100))`
  "rotations" `(subtract :r 30 (box 50 | rotate y tau/8 z tau/8) (ball 50 | move x 50))`
  "cone rounded" `(cone z 50 100 :r 10)`
  "line" `(union :r 50 (line [-50 0 0] [50 0 0] 10) (ball 50 | move x 100 | mirror x))`

  "union" `(union (circle 75 | move [-30 0]) (rect 60 | move [30 0]))`
  "union smooth" `(union :r 10 (circle 75 | move [-30 0]) (rect 60 | move [30 0]))`
  "union order is significant when combining color fields" `
    (union
      (union (circle 50 | color [1 1 0.5] | move [-30 0]) (circle 50 | color [1 0.5 1] | move [30 0])
      | move [0 60])
      (union (circle 50 | color [1 0.5 1] | move [30 0]) (circle 50 | color [1 1 0.5] | move [-30 0])
      | move [0 -60])
      )
  `
  "union order is significant when combining color fields even with smooth union" `
    (union
      (union :r 10 (circle 50 | color [1 1 0.5] | move [-30 0]) (circle 50 | color [1 0.5 1] | move [30 0])
      | move [0 60])
      (union :r 10 (circle 50 | color [1 0.5 1] | move [30 0]) (circle 50 | color [1 1 0.5] | move [-30 0])
      | move [0 -60])
      )
  `
  "union smooth can combine non-overlapping shapes" `
    (union :r 20
      (circle 40 | move [41 0] | color [0.5 1 1])
      (circle 60 | move [-61 0] | color [1 1 0.5]))
  `

  "smooth union more than two color fields" `
    (def shapes
      [(circle 25 | move [-15 15] | color [1 0.5 0.25])
      (circle 25 | move [-15 -15] | color [0.5 1 0.25])
      (circle 25 | move [15 -15] | color [0.5 0.25 1])
      (circle 25 | move [15 15] | color [0.5 1 1])])
    (var i 0)
    (union
      (union :s 5 ;shapes)
      ;(seq [x :in [-75 75] y :in [-75 75] :let [offset [x y]] :after (++ i)]
        (union :r 5 ;(drop i shapes) ;(take i shapes)
        | move offset)))
  `

  "union a shape with no color field inherits the other color field" `
  (union
    (union
      (circle 40 | move [41 0])
      (circle 60 | move [-61 0] | color [1 1 0.5])
    | move [0 60])
    (union
      (circle 40 | move [41 0] | color [0.5 1 1])
      (circle 60 | move [-61 0])
    | move [0 -60])
  )
  `

  "default gradient" `(circle 100)`
  "move shape" `(circle 50 | move [50 0])`
  "move 3d" `(box 50 | move [50 0 0])`
  "move takes axis, scale pairs" `(box 50 | move x 100 y 100)`
  "you can pass 3d axis vectors to move 2d even though you shouldn't be able to" `(rect 50 | move x 50 -y 50)`
  # this doesn't really demonstrate anything...
  "color field" `(circle 100 | color [(q.x / 100 * 0.5 + 0.5) 1 0.5])`
  "color before move" `(circle 100 | color [(q.x / 100 * 0.5 + 0.5) 1 0.5] | move [50 0])`
  "color after move" `(circle 100 | move [50 0] | color [(q.x / 100 * 0.5 + 0.5) 1 0.5])`
  "rect" `(rect 75)`
  "rotation is counter-clockwise" `(rect 70 | rotate 0.1)`
  "perlin2" `(ball 100 | color (vec3 (perlin+ p.xy 10)))`
  "perlin3" `(ball 100 | color (vec3 (perlin+ p 10)))`
  "perlin4" `(ball 100 | color (vec3 (perlin+ [p 0] 10)))`

  "worley-2d" `(ball 100 | color (vec3 (worley p.xy 30)))`
  "worley2-2d" `(ball 100 | color (vec3 0 (worley2 p.xy 30)))`
  "worley-3d" `(ball 100 | color (vec3 (worley p 30)))`
  "worley2-3d" `(ball 100 | color (vec3 0 (worley2 p 30)))`

  "extrude" `(rect 30 | union (circle 30 | move x 30) | extrude x 100)`
  "extrude defaults to zero" `(rect 30 | union (circle 30 | move x 30) | extrude z)`
  "extrude inf" `(rect 30 | union (circle 30 | move x 30) | extrude y inf)`

  "revolve" `(rect 30 | union (circle 30 | move x 30) | revolve x 100)`
  "revolve defaults to zero" `(rect 30 | union (circle 30 | move x 30) | revolve z)`

  "slice" `(box 50 | rotate [1 1 1 | normalize] 1 | slice z)`
  "slice with offset" `(box 50 | rotate [1 1 1 | normalize] 1 | slice y 10)`

  "3d rotation" `
  (union
    (box 40 | rotate y 0.2 | move [-80 0 80])
    (box 40 | rotate y 0.2 x 0.5 | move [80 0 -80])
    (box 40 | rotate x 0.3 y 0.5 | move [80 0 80])
    (box 40 | rotate (normalize [1 0 1]) 1 | move [-80 0 -80]))`

  "round rect same" `(rect 80 :r 20)`
  "round rect different" `(rect [60 80] :r [10 20 30 40])`

  "lines and rects" `
  (union
    (rect [100 10])
    (oriented-rect [0 -30] [100 -30] 20)
    (line [0 30] [100 30] 20)

    (rect [100 10])
    (oriented-rect [-100 -40] [100 -80] 20)
    (line [-100 40] [100 80] 20))
  `

  "triangles" `
  (union
    (triangle 30 | move [-50 50])
    (triangle [40 40] | move [50 50])
    (triangle [-20 -5] [45 17] [-16 -77]))
  `

  "gons and grams" `
  (union
    (pentagon 30 | move [-50 -50])
    (hexagon 30 | move [-50 50])
    (octagon 30 | move [50 -50])
    (hexagram 30 | move [50 50])
    (star 30 15))
  `

  "capsule 2d" `(capsule-2d 39 32 37)`
  "pie" `(union ;(seq [i :range [1 5]] (pie 30 (tau / i - 0.6) | move y (i * 40 - 100))))`
  "cut disk" `
  (union
    (cut-disk 50 -40 | move y 60)
    (cut-disk 50 20 | move y -60))
  `
  "arc and ring" `
  (union
    (arc 80 tau/4*3 20 | move [-40 0])
    (ring 80 tau/4*3 20 | move [0 0])
    (arc 80 tau/4 20 | move [40 -40])
    (ring 80 tau/4 20 | move [40 -80]))
  `

  "shell" `
  (union
    (rect 50 | shell | move [50 50])
    (rect 50 | shell 10)
  | move [-25 -25])
  `
  "expand" `(rect 50 | expand 10)`
  "map-distance" `(rect 50 | map-distance (fn [d] (abs d - 10)))`
  "elongate 2d" `(circle 10 | elongate [50 60])`
  "elongate 3d" `(ball 10 | elongate [50 60 70])`
  "asymmetric elongate" `(rect 100 :r [10 20 30 40] | elongate [20 20])`

  "torus" `
  (union
    (torus x 100 10)
    (torus y 100 10)
    (torus z 100 10))
  `

  "cone" `
  (union
    (cone x 40 160)
    (cone y 40 160)
    (cone z 40 -160))
  `

  "ellipsoid" `
  (union
    (ball [100 40 40])
    (ball [40 40 100])
    (ball [40 100 40]))
  `

  "align" `
  (def target [-80 101 52])
  (union
    (cone y 10 80 | align y (normalize target))
    (box 10 | move target)
    (ball 10 | move (align [0 0 100] z (normalize target))))
  `

  "octahedron" `(octahedron 120 | rotate x pi/4)`

  "blinn phong" `
  (box 60 :r 10
  | shade [0.25 1 0.25] :s 0.5 :g 10
  | with-lights
    (light/point [1 1 1] [-200 100 200])
    (light/point [1 0.1 0.1] [200 200 200]))
  `

  "union 3d color fields" `
  (union
    (box 50 | color [0.9 0.1 0.1] | move [0 -50 0])
    (box 30 | color [0.9 0.9 0.1] | move [0 30 0]))
  `

  "2d color fields that extend outside of their shape"
  `
  (def sharp-circles (union
    (circle 10 | move [-10 0] | color [1 0.1 0.1])
    (circle 20 | move [10 0] | color [0.1 1 0.1])))

  (def smooth-circles (union :r 5
    (circle 10 | move [-10 0] | color [1 0.1 0.1])
    (circle 20 | move [10 0] | color [0.1 1 0.1])))

  (defn make [circles]
    (union
      (circles | expand 10 | move [0 80])
      (circles | move [0 20])
      (rect 40
      | color circles
      | move [0 -50])))
  (union
    (make sharp-circles
    | move [-50 0])
    (make smooth-circles
    | move [50 0]))
  `

  "shadow radius"
  `
  (defn grid [size xs zs & shapes]
    (union
      ;(seq [[i shape] :pairs shapes]
        (def x (div i zs - (xs - 1 / 2)))
        (def z (mod i zs - (zs - 1 / 2)))
        (shape | move [(* size x 2) 0 (* size z 2)]))))
  (defn make []
    (union
      (box [50 5 50])
      (box 20 | move y 30 | expand 1)
    | shade [1 0.1 0.1]))
  (grid 60 2 2
    ((make) | with-lights (light/directional [0.9 0.9 0.9] [-1 -5 1] 100 :shadow 0.25))
    ((make) | with-lights (light/directional [0.9 0.9 0.9] [-1 -5 1] 100 :shadow 0))
    ((make) | with-lights (light/directional [0.9 0.9 0.9] [-1 -5 1] 100 :shadow 0.5))
    ((make) | with-lights (light/directional [0.9 0.9 0.9] [-1 -5 1] 100 :shadow 1))
    )
  `

  "shadows are always computed in a global coordinate space"
  `
  (defn grid [size xs zs & shapes]
    (union
      ;(seq [[i shape] :pairs shapes]
        (def x (div i zs - (xs - 1 / 2)))
        (def z (mod i zs - (zs - 1 / 2)))
        (shape | move [(* size x 2) 0 (* size z 2)]))))
  (defn make []
    (union
      (box [50 5 50])
      (box 20 | move y 30 | expand 1)
    | shade [1 0.1 0.1]))
  (def point-light (light/point [0.9 0.9 0.9] [0 200 0] :shadow 0.25))
  (grid 60 2 2 (make) (make) (make) (make)
  | with-lights point-light)
  `

  "ambient lights and other weird lights" `
  (defn grid [size xs zs & shapes]
    (union
      ;(seq [[i shape] :pairs shapes]
        (def x (div i zs - (xs - 1 / 2)))
        (def z (mod i zs - (zs - 1 / 2)))
        (shape | move [(* size x 2) 0 (* size z 2)]))))
  (defn make []
    (union
      (box [50 5 50])
      (box 20 | move y 30 | expand 1)
    | shade [1 0.1 0.1] :g 5))
  (grid 60 2 2
    ((make) | with-lights)
    ((make) | with-lights (light/ambient [0.5 0.5 0.5]))
    ((make) | with-lights (light/point [0.9 0.9 0.9] (+ P normal [0 10 0]) :shadow 0.25))
    ((make) | with-lights (light/point [0.9 0.9 0.9] (+ P normal) :shadow 0.25)))
  `

  "mirror 2d"
  `
  (circle 50 | move x 20 y 20 | mirror x y)
  `

  "mirror 3d"
  `
  (ball 50 | move [20 20 20] | mirror x z)
  `

  "morph"
  `
  (box 50
  | shade [1 0 0]
  | morph :color 0.1 :distance 0.25
    (ball 50 | shade [0 1 0]))
  `

  "round 2d shapes"
  `
  (union
    (rect 30 :r 5 | move [-100 100])
    (hexagon 30 :r 5 | move [0 100])
    (hexagram 30 :r 5 | move [100 100])
    (star 30 20 :r 10 | move [-100 0]))
  `

  "round 3d shapes"
  `
  (union
    (box 50 :r 10 | move [-99 0 66])
    (box-frame 50 10 :r 5 | move [53 0 -100]))
  `

  "boolean union"
  `
  (def green-sphere (ball 40 | shade [0.05 0.95 0.05]))
  (def red-box (box 30 | shade [0.95 0.05 0.05]))
  (union
    (union green-sphere red-box | move [-40 0 0])
    (union red-box green-sphere | move [+40 0 0])
    (union :r 10 green-sphere red-box | move [-40 0 -80])
    (union :r 10 red-box green-sphere | move [+40 0 -80])
    (union :r 10 :distance 0 green-sphere red-box  | move [-120 0 -80])
    (union :r 10 :distance 0 red-box green-sphere | move [+120 0 -80])
    (union :s 10 green-sphere red-box | move [-40 0 +80])
    (union :s 10 red-box green-sphere | move [+40 0 +80])
    (union :s 10 :distance 0 green-sphere red-box  | move [-120 0 +80])
    (union :s 10 :distance 0 red-box green-sphere | move [+120 0 +80])
  | intersect (box [1000 80 1000] | move y -66))
  `

  "boolean intersect"
  `
  (def green-sphere (ball 40 | shade [0.05 0.95 0.05]))
  (def red-box (box 30 | shade [0.95 0.05 0.05]))
  (union
    (intersect green-sphere red-box | move [-40 0 0])
    (intersect red-box green-sphere | move [+40 0 0])
    (intersect :r 10 green-sphere red-box | move [-40 0 -80])
    (intersect :r 10 red-box green-sphere | move [+40 0 -80])
    (intersect :r 10 :distance 0 green-sphere red-box  | move [-120 0 -80])
    (intersect :r 10 :distance 0 red-box green-sphere | move [+120 0 -80])
    (intersect :s 10 green-sphere red-box | move [-40 0 +80])
    (intersect :s 10 red-box green-sphere | move [+40 0 +80])
    (intersect :s 10 :distance 0 green-sphere red-box  | move [-120 0 +80])
    (intersect :s 10 :distance 0 red-box green-sphere | move [+120 0 +80])
  | intersect (box [1000 80 1000] | move y -66))
  `

  "boolean subtract"
  `
  (def green-sphere (ball 40 | shade [0.05 0.95 0.05]))
  (def red-box (box 30 | shade [0.95 0.05 0.05]))
  (union
    (subtract green-sphere red-box | move [-40 0 0])
    (subtract red-box green-sphere | move [+40 0 0])
    (subtract :r 10 green-sphere red-box | move [-40 0 -80])
    (subtract :r 10 red-box green-sphere | move [+40 0 -80])
    (subtract :r 10 :distance 0 green-sphere red-box  | move [-120 0 -80])
    (subtract :r 10 :distance 0 red-box green-sphere | move [+120 0 -80])
    (subtract :s 10 green-sphere red-box | move [-40 0 +80])
    (subtract :s 10 red-box green-sphere | move [+40 0 +80])
    (subtract :s 10 :distance 0 green-sphere red-box  | move [-120 0 +80])
    (subtract :s 10 :distance 0 red-box green-sphere | move [+120 0 +80])
  | intersect (box [1000 80 1000] | move y -66))
  `

  "boolean union interior color fields" `
  (def green-sphere (ball 44 | color [0.05 0.95 0.05]))
  (def red-box (box 40 | color [0.95 0.05 0.05]))
  (union
    (union :r 10 green-sphere red-box | move [-50 -50 0])
    (union :r 10 red-box green-sphere | move [50 -50 0])
    (union :s 10 green-sphere red-box | move [-50 50 0])
    (union :s 10 red-box green-sphere | move [50 50 0])
  | slice z 0)
  (set camera (camera/perspective [0 0 384] :fov 45))
  `

  "boolean intersect interior color fields" `
  (def green-sphere (ball 44 | color [0.05 0.95 0.05]))
  (def red-box (box 40 | color [0.95 0.05 0.05]))
  (union
    (intersect :r 10 green-sphere red-box | move [-50 -50 0])
    (intersect :r 10 red-box green-sphere | move [50 -50 0])
    (intersect :s 10 green-sphere red-box | move [-50 50 0])
    (intersect :s 10 red-box green-sphere | move [50 50 0])
  | slice z 0)
  (set camera (camera/perspective [0 0 384] :fov 45))
  `

  "shadow banding artifacts" `
  (union
    (ball 50 | shade [1 1 1] | with-lights (light/point [1 1 1] [500 -50 0] :shadow 0.25) | move y -50)
    (ball 50 | shade [1 1 1] | with-lights (light/directional [1 1 1] [-1 0 0] 500 :shadow 0.25) | move y 50))
  (set camera (camera/perspective [0 0 384] :fov 45))
  `

  "raymarcher tries not to penetrate shape 1" `
  (ball 50
    | morph 2 (box 50)
    | shade [1 1 1])
  `
  "raymarcher tries not to penetrate shape 2" `
  (ball 50
    | morph 2 (box 50)
    | shade [1 1 1])
  (set camera (camera/perspective [0 0 384] :fov 45))
  `

  "scale" `
  (union
    (rect 30)
    (rect 30 | scale [0.5 1] | move x -67)
    (rect 30 | scale [2 0.5] | move y 65)
    (rect 30 | scale 0.75 | move x 71)
    (rect 30 | scale 0.5 | pivot [30 30] | move y -70))
  `

  "gl helpers" `
  (union
    (gl/let [foo [50 0]] (rect 30 | move foo))
    (gl/with [q (- q [-50 0])] (rect 30))
    )
  `

  "gl with color fields" `
  (union
    (gl/with :color [normal (normal + (perlin p * 0.1))]
      (ball 100 | shade [1 0 0] | move [-50 0 0]))
    (ball 100 | shade [1 0 0] | move [50 0 0]
    | gl/with :color [normal (normal + (perlin p * 0.1))] _)
  )
  `

  "tile" `
  (ball 30 | tile [80 80 80] :limit [2 3 4])
  `

  "tile 2d with asymmetric shape, no oversampling" `
  (rect 30 | rotate 0.3 | tile [80 80])
  `
  "tile 2d with asymmetric shape, oversampling" `
  (rect 30 | rotate 0.3 | tile [80 80] :oversample true)
  `

  "tile 3d with asymmetric shape, no oversampling" `
  (cone y 30 60
  | shade [1 0 0]
  | rotate x -1 y 2 z 3
  | tile 60 :limit 3
  )
  `
  "tile 3d with asymmetric shape, oversampling" `
  (cone y 30 60
  | shade [1 0 0]
  | rotate x -1 y 2 z 3
  | tile [60 60 60] :limit 3 :oversample true
  )
  `
  "tile 3d works with oversample and zero axes" `
  (ball 10
  | move y (sin+ (hash $i * 5 * tau) | ss * 25)
  | shade (hsv (hash $i / 2 + 0.3) 1 1)
  | tile: $i [25 0 25] :oversample true
  | union (plane y -10 | shade [1 1 1])
  )
  `

  "tile-colon" `
  (circle 6 | color (hsv (hash $i) 0.5 1) | tile: $i [12 12])
  `

  "tile with oversampling picks the nearest color" `
  (tile* [20 20] :oversample true :sample-from [-1 -1] :sample-to [1 1] (fn [$i]
    (triangle [10 31]
      | move (hash2 $i * 10)
      | rotate (hash $i)
      | color (hsv (hash $i) 0.8 1))))
  `

  "ss overloads" `
  (union
    (rect 30
    | color [(ss q.x -30 30) 0.1 0.1]
    | move [-40 40]
    )
    (rect 30
    | color [(ss q.x 30 -30) 0.1 0.1]
    | move [-40 -40]
    )
    (rect 30
    | color [(ss q.x -30 30 0.5 2) 0.1 0.1]
    | move [40 40]
    )
    (rect 30
    | color [(ss q.x 30 -30 2 0.5) 0.1 0.1]
    | move [40 -40]
    ))
  `

  "custom background color" `
  (set background-color
    [(Frag-Coord.x | floor | mod 10 | step 3 _)
     (Frag-Coord.y | floor | mod 10 | step 8 _)
      (abs frag-coord | max | pow 2 * 2)])
  `

  "explicit isolines" `
  (rect 60 | color [1 0.5 0.5])
  (set background-color isolines)
  `

  "unhoisted lights can refer to tile indices" `
  (ball 20
  | shade [1 1 1]
  | with-lights (light/point (hsv (hash $i) 1 1) [200 200 200] :shadow 0.50 :hoist false)
  | tile: $i :limit 3 [70 70 70])
  `

  "occlusion" `
  (union
    (ball 30
      | union (box [40 10 40] | move y -30)
      | color (vec3 (occlusion))
    | move [-100 0 0])
    (ball 30
      | union (box [40 10 40] | move y -30)
      | color (vec3 (occlusion :steps 32 :dist 50))
    | move [0 0 0])
    (ball 30
      | union (box [40 10 40] | move y -30)
      | color (vec3 (occlusion :dir (normalize (perlin P * 0.5 + normal))))
    | move [100 0 0]))
  `

  "radial 2D" `
  (rect 20 | move x 100 | radial 12)
  `
  "radial 2D offset" `
  (rect 20 | radial 12 100)
  `
  "radial 2D no oversample" `
  (rect 20 | rotate 0.25 | move x 100 | radial 12 :oversample false)
  `
  "radial 2D oversample" `
  (rect 20 | rotate 0.25 | move x 100 | radial 12 :oversample true)
  `
  "radial 2D colors" `
  (circle 12 | color (hsv (hash $i) 0.5 1) | move x 100 | radial: $i 24)
  `
  "radial 2D colors asymmetric" `
    (triangle [10 73] | color (hsv (hash $i) 0.5 1) | move x 89 | radial: $i 12 :oversample true :sample-from -1 :sample-to 1)
  `

  "radial 3D axis - no oversample" `
  (union
    (box 20 | move x 100 | radial z 12)
    (box 20 | move z 100 | radial y 12)
    (box 20 | move y 100 | radial x 12))
  `
  "radial 3D axis - oversample" `
  (union
    (box 20 | move x 100 | radial z 12 :oversample true)
    (box 20 | move z 100 | radial y 12 :oversample true)
    (box 20 | move y 100 | radial x 12 :oversample true))
  `

  "radial 3D" `
  (box 20 | move x 100 | radial z 12)
  `
  "radial 3D offset" `
  (box 20 | radial y 12 100)
  `
  "radial 3D no oversample" `
  (box 20 | rotate [1 1 1 | normalize] 0.25 | move x 100 | radial z 12 :oversample false)
  `
  "radial 3D oversample" `
  (box 20 | rotate [1 1 1 | normalize] 0.25 | move x 100 | radial z 12 :oversample true)
  `
  "radial 3D colors" `
  (box 12 | shade (hsv (hash $i) 0.5 1) | move x 100 | radial: $i z 24)
  `
  "radial 3D colors asymmetric" `
    (cone y 50 100 | shade (hsv (hash $i) 0.5 1) | move x 89 | radial: $i z 12 :oversample true :sample-from -1 :sample-to 1)
  `

  "shadowing subject does nothing - def" `
  (ball 100)
  (def subject 123)
  `
  "shadowing subject does nothing - var" `
  (ball 100)
  (var subject 123)
  `
  "you can refer to hoisted expressions from any built-in var" `
  (gl/def foo [1 0.5 1])
  (gl/def bar [1 0.5 0.5])
  (set background-color (foo * 0.5))
  (set default-3d-color bar)
  (sphere 100)
  `
  "background color supports alpha channel" `
  (ball 100)
  (set background-color [1 1 0 (gl/if (< frag-coord.x 0) 0 1)])
  `
  "more alpha test" `
  (ball 100)
  (set background-color [1 1 0 (ss frag-coord.x -1 1)])
  `
  "bump" `
  (ball 100 | color default-3d-color | bump (worley (p / 30) | pow 4))
  `
  "orthographic camera" `
  (box 50 | tile [200 0 200])
  (set camera (camera/orthographic [1 1 1 | normalize * 512]))
  `
  "rotation actually works the way it's supposed to" `
  (def point [100 50 70])
  (def theta 2)
  (union
    (line [0 0 0] point 5 | rotate x theta y theta z theta)
    (ball 10 | move (rotate point x theta y theta z theta)))
  `
  "3d 3d bezier extrude" `
  (cone y 20 50
  | bezier [-100 0 0] [100 200 -100] [100 0 0])
  `
  "2d 3d bezier extrude" `
  (trapezoid 30 5 20
  | bezier [-100 0 0] [100 200 -100] [100 0 0])
  `
  "float 3d bezier extrude" `
  (20
  | bezier [-100 0 0] [100 200 -100] [100 0 0])
  `
  "2d 3d bezier extrude parameterized" `
  (rect (mix 10 30 $t)
  | bezier: $t [-100 0 0] [100 200 -100] [100 0 0])
  `
  "3d 3d bezier extrude parameterized" `
  (cone y 20 (mix 10 50 $t)
  | bezier: $t [-100 0 0] [100 200 -100] [100 0 0])
  `
  "float 3d bezier extrude parameterized" `
  ($t * 20 + 5
  | bezier: $t [-100 0 0] [100 200 -100] [100 0 0])
  `
  "2d 2d bezier extrude" `
  (triangle [20 50]
  | bezier [-100 0] [0 50] [100 0])
  `
  "float 2d bezier extrude" `
  (20
  | bezier [-100 0] [0 50] [100 0])
  `
  "2d 2d bezier extrude parameterized" `
  (rect (mix 10 30 $t)
  | bezier: $t [-100 0] [0 50] [100 0])
  `
  "float 2d bezier extrude parameterized" `
  ($t * 20 + 5
  | bezier: $t [-100 0] [0 50] [100 0])
  `
})

(each filename (os/dir "./cases")
  (def name (string/remove-suffix filename ".janet"))
  (put test-cases name (slurp (string "./cases/" filename))))

(def out-buffer @"")
(setdyn *out* out-buffer)

(print `<!DOCTYPE html>`)
(print `<html>`)
(print `<head>`)
(print `<meta charset="utf-8" />`)
(print `<style type="text/css">
* {
  margin: 0;
  padding: 0;
}
html {
  background-color: #333;
  color: #eee;
  padding: 1em;
}
body {
  max-width: 1200px;
  margin: 0 auto;
}
img {
  image-rendering: pixelated;
}
.test-case {
  display: grid;
  grid-template-columns: 1fr 1fr auto;
}
.test-case pre {
  overflow: auto;
  height: `(display-resolution 1)`px;
}
.test-case > h1, div.test-case > .stats {
  grid-column: span 3;
}
.shader-source, .error {
  grid-column: 2;
}
.images {
  display: flex;
}
.images .new {
  position: relative;
}
.images .new .underlay {
  position: absolute;
}
.images .new:hover .underlay {
  visibility: hidden;
}
.images .new:active .underlay {
  visibility: visible;
  mix-blend-mode: difference;
}
</style>`)
(print `</head>`)
(print `<body>`)

(defn html-escape [x] (->> x
  (string/replace-all "&" "&amp;")
  (string/replace-all "<" "&lt;")
  (string/replace-all ">" "&gt;")))

(def current-refs @{})
(def previous-refs (os/dir "refs"))

(defn img [filename &opt class]
  (printf `<img%s src="%s" width="%d" height="%d" />`
    (if class (string ` class="` class `"`) "")
    filename (display-resolution 0) (display-resolution 1)))

(defn ms [start end]
  (string/format "%.1fms" (* (- end start) 1000)))

(def default-render-type 0)
(defn render [name program]
  (gccollect)
  (var success true)
  (try (do
    (def before-eval (os/clock :monotonic))
    (def env (bauble/evaluator/evaluate program))
    (def after-eval-before-compile (os/clock :monotonic))
    (def [shader-source dimension animated? has-custom-camera?]
      (bauble/compile-to-glsl default-render-type env "330"))
    (def after-compile-before-render (os/clock :monotonic))
    (def image (render-image shader-source
      :resolution physical-resolution
      :orbit [0.125 -0.125]
      :zoom 0.75))
    (def after-render-before-export (os/clock :monotonic))
    (def temporary-file-name "snapshots/tmp.png")
    (jaylib/export-image image temporary-file-name)
    (def after-export-before-hash (os/clock :monotonic))
    (def hash (string/slice ($<_ shasum -ba 256 snapshots/tmp.png) 0 32))
    (def after-hash (os/clock :monotonic))
    (def new-file-name (string "snapshots/" hash ".png"))
    (def ref-name (string (string/replace-all " " "-" name) ".png"))
    (put current-refs ref-name true)
    (def ref-path (string "refs/" ref-name))
    (if (os/stat new-file-name)
      (do
        (eprin ".")
        (os/rm temporary-file-name))
      (do
        (eprin ":")
        (os/rename temporary-file-name new-file-name)
        (set success false)))

    (def old-file-name (try
      (slice ($<_ git show HEAD:tests/ ^ ,ref-path >[stderr :null]) 3)
      ([_ _] nil)))
    ($ ln -fs (string "../" new-file-name) ,ref-path)

    (def changed? (and old-file-name (not= old-file-name new-file-name)))

    (printf `<pre class="shader-source">%s</pre>` (html-escape shader-source))
    (printf `<div class="images">`)
      (when changed?
        (printf `<div class="old">`)
        (img new-file-name)
        (printf `</div>`))
      (printf `<div class="new">`)
        (when changed?
          (img old-file-name "underlay"))
        (img new-file-name (if changed? "overlay"))
      (printf `</div>`)
    (printf `</div>`)

    (printf `<div class="stats">%s eval, %s compile, %s render, %s export, %s hash</div>`
      (ms before-eval after-eval-before-compile)
      (ms after-eval-before-compile after-compile-before-render)
      (ms after-compile-before-render after-render-before-export)
      (ms after-render-before-export after-export-before-hash)
      (ms after-export-before-hash after-hash))
    )

    ([e fib]
      (set success false)
      (eprin "!")
      (def buf @"")
      (with-dyns [*err* buf]
        (debug/stacktrace fib e (string name ":")))
      (printf `<pre class="error">%s</pre>` (html-escape buf))))
  success)

(var failing-tests 0)
(each [name program] (sort (pairs test-cases) (fn [[name1 _] [name2 _]] (< name1 name2)))
  (def program (string/trim program))

  (print `<div class="test-case">`)
  (printf `<h1>%s</h1>` (html-escape name))
  (printf `<pre>%s</pre>` (html-escape program))
  (def success
    (render name (string program "\n(set aa-grid-size 3)")))
  (unless success (++ failing-tests))
  (print `</div>`)
  )

(print `</body>`)
(print `</html>`)

(eprint)
(when (> failing-tests 0)
  (eprintf "%d tests failed" failing-tests))

(loop [ref-name :in previous-refs :unless (in current-refs ref-name)]
  (eprintf "deleting symlink %s" ref-name)
  (os/rm (string "refs/" ref-name)))

(spit "summary.html" out-buffer)
