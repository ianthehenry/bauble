(import jaylib)
(use sh)
(use ../src/cli/helpers)

(init-jaylib)

(def physical-resolution [128 128])
(def display-resolution [256 256])

(def zoom 0.75)

(def ortho-z [(map |(* $ zoom) [0 0 512]) [0 0 0]])
(def isometric [
  (map |(* $ zoom) [256 362 256])
  [(* 0.125 math/pi 2) (* -0.125 math/pi 2) 0]])

(defn string/remove-suffix [str suffix]
  (if (string/has-suffix? suffix str)
    (string/slice str 0 (- (length str) (length suffix)))
    (errorf "suffix not found in %s" str)))

(def test-cases @{
  "morph" `(morph (box 100) (sphere 100) 0.5)`

  "wiggler" `
  (torus :z 60 30
  | twist :y 0.07
  | move :x 50
  | mirror :r 10 :x
  | fresnel
  | slow 0.25)
  `

  "expressions" `
  (cone :y 100 (+ 100 (* 10 (cos (/ p.x 5)))))
  `

  "map-color" `(sphere 100 | map-color (fn [col] (+ [1 0 0] col)))`
  "map-color 2" `(sphere 100 | map-color (fn [col] (pow col 2)))`

  "explicit fresnel" `
    (sphere 100 | color (let [view-dir (normalize (- camera P))]
      (1.0 - dot normal view-dir | pow 5 + c)))
  `

  "union" `
  (def green-box (shade green (box 75 :r 5) :gloss 12 :shine 1))
  (def red-sphere (shade red (sphere 100)))
  (union green-box red-sphere)
  `
  "smooth union" `
  (def green-box (shade green (box 75 :r 5) :gloss 12 :shine 1))
  (def red-sphere (shade red (sphere 100)))
  (union :r 5 green-box red-sphere)
  `

  "intersect" `
  (def green-box (shade green (box 75 :r 5) :gloss 12 :shine 1))
  (def red-sphere (shade red (sphere 100)))
  (intersect green-box red-sphere)
  `

  "smooth intersect" `
  (def green-box (shade green (box 75 :r 5) :gloss 12 :shine 1))
  (def red-sphere (shade red (sphere 100)))
  (intersect :r 5 green-box red-sphere)
  `

  "smooth subtract" `
  (def green-box (shade green (box 75 :r 5) :gloss 12 :shine 1))
  (def red-sphere (shade red (sphere 100)))
  (subtract :r 5 green-box red-sphere)
  `

  "resurface1" `
  (def green-box (shade green (box 75 :r 5) :gloss 12 :shine 1))
  (def red-sphere (shade red (sphere 100)))
  (resurface
    (union :r 5 green-box red-sphere)
    (union green-box red-sphere))
  `
  "resurface2" `
  (def green-box (shade green (box 75 :r 5) :gloss 12 :shine 1))
  (def red-sphere (shade red (sphere 100)))
  (resurface
    green-box
    (union green-box red-sphere))
  `

  "balloon" [ortho-z `
  (ellipsoid [100 100 50]
  | union :r 50 (cylinder :y 25 50 | move [0 -100 0])
  | scale :y (ss p.y -100 100 1 0.8)
  | radial :y 20 0
  | move [0 40 0]
  | fresnel 1)
  `]

  "normal colors" `(sphere 100 | shade (remap+ normal))`
  "perlin noise 2d" `(box 80 :r 10 | color [0 (perlin+ (* 0.1 p.xz)) 0])`
  "perlin noise 3d" `(box 80 :r 10 | color [0 (perlin+ (* 0.1 p)) 0])`
  "perlin noise 4d" `(box 80 :r 10 | color [0 (perlin+ (* 0.1 p.xyzx)) 0])`

  "spatial noise" `(sphere (perlin (p * 0.05) * 10 + 100) | slow 0.9)`
  "spatial noise extreme" `(sphere (perlin (p * 0.05) * 30 + 80) | slow 0.5)`

  "onion" `(sphere 100 | onion 5 | intersect (half-space :-z))`
  "cylinder" `(box 50 | subtract (cylinder :z 30 100))`
  "rotations" `(subtract :r 30 (box 50 | rotate :y tau/8 :z tau/8) (sphere 50 | move :x 50))`
  "cone" `(cone :x 50 100)`
  "cone negative" `(cone :-x 50 100)`
  "cone rounded" `(cone :-z 50 100 :r 10)`
  "mirror" `(cone :x 50 100 | rotate :y pi/4 | mirror :x :z)`
  "line" `(union :r 50 (line [-50 0 0] [50 0 0] 10) (sphere 50 | move :x 100 | mirror :x))`
  "repetition" `(sphere 20 | tile [50 50 50] :limit [3 8 2])`
  "radial" `(cone :x 50 100 | radial :y 24)`
  "scale" `(box 40 | scale [1 2 0.5])`
  "torus" `(torus :y 100 25)`
  "twist" `(box 80 | twist :y 0.010 | slow 0.5)`
  "bend" `(box [80 10 80] | bend :x :y 0.010 | slow 0.5)`
  "swirl" `(box 80 | swirl :y 0.040 | slow 0.5)`

  "!union" `(union (circle 75 | move [-30 0]) (rect 60 | move [30 0]))`
  "!union smooth" `(union :r 10 (circle 75 | move [-30 0]) (rect 60 | move [30 0]))`
  "!union order is significant when combining color fields" `
    (union
      (union (circle 50 | color [1 1 0.5] | move [-30 0]) (circle 50 | color [1 0.5 1] | move [30 0])
      | move [0 60])
      (union (circle 50 | color [1 0.5 1] | move [30 0]) (circle 50 | color [1 1 0.5] | move [-30 0])
      | move [0 -60])
      )
  `
  "!union order is significant when combining color fields even with smooth union" `
    (union
      (union :r 10 (circle 50 | color [1 1 0.5] | move [-30 0]) (circle 50 | color [1 0.5 1] | move [30 0])
      | move [0 60])
      (union :r 10 (circle 50 | color [1 0.5 1] | move [30 0]) (circle 50 | color [1 1 0.5] | move [-30 0])
      | move [0 -60])
      )
  `
  "!union smooth can combine non-overlapping shapes" `
    (union :r 20
      (circle 40 | move [41 0] | color [0.5 1 1])
      (circle 60 | move [-61 0] | color [1 1 0.5]))
  `

  "!smooth union more than two color fields" `
    (def shapes
      [(circle 25 | move [-15 15] | color [1 0.5 0.25])
      (circle 25 | move [-15 -15] | color [0.5 1 0.25])
      (circle 25 | move [15 -15] | color [0.5 0.25 1])
      (circle 25 | move [15 15] | color [0.5 1 1])])
    (var i 0)
    (union
      (union :rs 5 ;shapes)
      ;(seq [x :in [-75 75] y :in [-75 75] :let [offset [x y]] :after (++ i)]
        (union :r 5 ;(drop i shapes) ;(take i shapes)
        | move offset)))
  `

  "!union a shape with no color field inherits the other color field" `
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

  "!default gradient" `(circle 100)`
  "!move shape" `(circle 50 | move [50 0])`
  "!move 3d" `(box 50 | move [50 0 0])`
  "!move takes axis, scale pairs" `(box 50 | move x 100 y 100)`
  "!you can pass 3d axis vectors to move 2d even though you shouldn't be able to" `(rect 50 | move x 50 -y 50)`
  # this doesn't really demonstrate anything...
  "!color field" `(circle 100 | color [(q.x / 100 * 0.5 + 0.5) 1 0.5])`
  "!color before move" `(circle 100 | color [(q.x / 100 * 0.5 + 0.5) 1 0.5] | move [50 0])`
  "!color after move" `(circle 100 | move [50 0] | color [(q.x / 100 * 0.5 + 0.5) 1 0.5])`
  "!rect" `(rect 75)`
  "!rotation is counter-clockwise" `(rect 70 | rotate 0.1)`
  "!perlin2" `(sphere 100 | color (vec3 (perlin+ (p.xy / 10))))`
  "!perlin3" `(sphere 100 | color (vec3 (perlin+ (p / 10))))`
  "!perlin4" `(sphere 100 | color (vec3 (perlin+ (vec4 (p / 10) 0))))`

  "!worley-2d" `(sphere 100 | color (vec3 (worley (p.xy / 30))))`
  "!worley2-2d" `(sphere 100 | color (vec3 0 (worley2 (p.xy / 30))))`
  "!worley-3d" `(sphere 100 | color (vec3 (worley (p / 30))))`
  "!worley2-3d" `(sphere 100 | color (vec3 0 (worley2 (p / 30))))`

  "!extrude" `(rect 30 | union (circle 30 | move x 30) | extrude x 100)`
  "!extrude defaults to zero" `(rect 30 | union (circle 30 | move x 30) | extrude z)`
  "!extrude inf" `(rect 30 | union (circle 30 | move x 30) | extrude y inf)`

  "!revolve" `(rect 30 | union (circle 30 | move x 30) | revolve x 100)`
  "!revolve defaults to zero" `(rect 30 | union (circle 30 | move x 30) | revolve z)`

  "!slice" `(box 50 | rotate (normalize [1 1 1]) 1 | slice z)`
  "!slice with offset" `(box 50 | rotate (normalize [1 1 1]) 1 | slice y 10)`

  "!3d rotation" `
  (union
    (box 40 | rotate y 0.2 | move [-80 0 80])
    (box 40 | rotate y 0.2 x 0.5 | move [80 0 -80])
    (box 40 | rotate x 0.3 y 0.5 | move [80 0 80])
    (box 40 | rotate (normalize [1 0 1]) 1 | move [-80 0 -80]))`

  "!round rect same" `(round-rect 80 20)`
  "!round rect different" `(round-rect [60 80] [10 20 30 40])`

  "!lines and rects" `
  (union
    (rect [100 5])
    (oriented-rect [0 -30] [100 -30] 10)
    (line [0 30] [100 30] 10)

    (rect [100 5])
    (oriented-rect [-100 -40] [100 -80] 10)
    (line [-100 40] [100 80] 10))
  `

  "!triangles" `
  (union
    (equilateral-triangle 30 | move [-50 50])
    (isosceles-triangle [40 40] | move [50 50])
    (triangle [-20 -5] [45 17] [-16 -77]))
  `

  "!gons and grams" `
  (union
    (pentagon 30 | move [-50 -50])
    (hexagon 30 | move [-50 50])
    (octagon 30 | move [50 -50])
    (hexagram 30 | move [50 50])
    (star 30 15))
  `

  "!uneven capsule" `(uneven-capsule 39 32 37)`
  "!pie" `(union ;(seq [i :range [1 5]] (pie 30 (math/pi / i - 0.3) | move y (i * 40 - 100))))`
  "!cut disk" `
  (union
    (cut-disk 50 -40 | move y 60)
    (cut-disk 50 20 | move y -60))
  `
  "!arc and ring" `
  (union
    (arc 60 2.3 10 | move [-30 0])
    (ring 60 2.3 10 | move [0 0])
    (arc 60 0.8 10 | move [30 -30])
    (ring 60 0.8 10 | move [30 -60]))
  `

  "!shell" `
  (union
    (rect 50 | shell | move [50 50])
    (rect 50 | shell 10)
  | move [-25 -25])
  `
  "!offset" `(rect 50 | offset 10)`
  "!map-distance" `(rect 50 | map-distance (fn [d] (abs d - 10)))`
  "!elongate 2d" `(circle 10 | elongate [50 60])`
  "!elongate 3d" `(sphere 10 | elongate [50 60 70])`

  "!torus" `
  (union
    (torus x 100 10)
    (torus y 100 10)
    (torus z 100 10))
  `

  "!cone" `
  (union
    (cone x 40 160)
    (cone y 40 160)
    (cone z 40 -160))
  `

  "!ellipsoid" `
  (union
    (ellipsoid [100 40 40])
    (ellipsoid [40 40 100])
    (ellipsoid [40 100 40]))
  `

  "!align" `
  (def target [-80 101 52])
  (union
    (cone y 10 80 | align y (normalize target))
    (box 10 | move target)
    (sphere 10 | move (align [0 0 100] z (normalize target))))
  `

  "!octahedron" `(octahedron 120 | rotate x pi/4)`

  "!blinn phong" `
  (box 60 :r 10
  | blinn-phong [0.25 1 0.25] :s 0.5 :g 10
  | with-lights
    (light/point [1 1 1] [-200 100 200])
    (light/point [1 0.1 0.1] [200 200 200]))
  `

  "!union 3d color fields" `
  (union
    (box 50 | color [0.9 0.1 0.1] | move [0 -50 0])
    (box 30 | color [0.9 0.9 0.1] | move [0 30 0]))
  `

  "!2d color fields that extend outside of their shape"
  `
  (def sharp-circles (union
    (circle 10 | move [-10 0] | color [1 0.1 0.1])
    (circle 20 | move [10 0] | color [0.1 1 0.1])))

  (def smooth-circles (union :r 5
    (circle 10 | move [-10 0] | color [1 0.1 0.1])
    (circle 20 | move [10 0] | color [0.1 1 0.1])))

  (defn make [circles]
    (union
      (circles | offset 10 | move [0 80])
      (circles | move [0 20])
      (rect 40
      | recolor circles
      | move [0 -50])))
  (union
    (make sharp-circles
    | move [-50 0])
    (make smooth-circles
    | move [50 0]))
  `

  "!shadow radius"
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
      (box 20 | move y 30 | offset 1)
    | blinn-phong [1 0.1 0.1]))
  (grid 60 2 2
    ((make) | with-lights (light/directional [0.9 0.9 0.9] [-1 -5 1] 100))
    ((make) | with-lights (light/directional [0.9 0.9 0.9] [-1 -5 1] 100 0))
    ((make) | with-lights (light/directional [0.9 0.9 0.9] [-1 -5 1] 100 0.5))
    ((make) | with-lights (light/directional [0.9 0.9 0.9] [-1 -5 1] 100 1))
    )
  `

  "!shadows are always computed in a global coordinate space"
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
      (box 20 | move y 30 | offset 1)
    | blinn-phong [1 0.1 0.1]))
  (def point-light (light/point [0.9 0.9 0.9] [0 200 0]))
  (grid 60 2 2 (make) (make) (make) (make)
  | with-lights point-light)
  `

  "!ambient lights and other weird lights"
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
      (box 20 | move y 30 | offset 1)
    | blinn-phong [1 0.1 0.1]))
  (grid 60 2 2
    ((make) | with-lights)
    ((make) | with-lights (light/ambient [0.5 0.5 0.5]))
    ((make) | with-lights (light/point [0.9 0.9 0.9] (+ P normal [0 10 0])))
    ((make) | with-lights (light/point [0.9 0.9 0.9] (+ P normal))))
  `

  "!mirror 2d"
  `
  (circle 50 | move x 20 y 20 | mirror x y)
  `

  "!mirror 3d"
  `
  (sphere 50 | move [20 20 20] | mirror x z)
  `

  "!morph"
  `
  (box 50
  | blinn-phong [1 0 0]
  | morph :color 0.1 :distance 0.25
    (sphere 50 | blinn-phong [0 1 0]))
  `

  "!round 2d shapes"
  `
  (union
    (rect 30 :r 5 | move [-100 100])
    (hexagon 30 :r 5 | move [0 100])
    (hexagram 30 :r 5 | move [100 100])
    (star 30 20 :r 10 | move [-100 0]))
  `

  "!round 3d shapes"
  `
  (union
    (box 50 :r 10 | move [-99 0 66])
    (box-frame 50 10 :r 5 | move [53 0 -100]))
  `

  "!boolean union"
  `
  (def green-sphere (sphere 40 | blinn-phong [0.05 0.95 0.05]))
  (def red-box (box 30 | blinn-phong [0.95 0.05 0.05]))
  (union
    (union green-sphere red-box | move [-40 0 0])
    (union red-box green-sphere | move [+40 0 0])
    (union :r 10 green-sphere red-box | move [-40 0 -80])
    (union :r 10 red-box green-sphere | move [+40 0 -80])
    (union :r 10 :distance 0 green-sphere red-box  | move [-120 0 -80])
    (union :r 10 :distance 0 red-box green-sphere | move [+120 0 -80])
    (union :rs 10 green-sphere red-box | move [-40 0 +80])
    (union :rs 10 red-box green-sphere | move [+40 0 +80])
    (union :rs 10 :distance 0 green-sphere red-box  | move [-120 0 +80])
    (union :rs 10 :distance 0 red-box green-sphere | move [+120 0 +80])
  | intersect (box [1000 80 1000] | move y -66))
  `

  "!boolean intersect"
  `
  (def green-sphere (sphere 40 | blinn-phong [0.05 0.95 0.05]))
  (def red-box (box 30 | blinn-phong [0.95 0.05 0.05]))
  (union
    (intersect green-sphere red-box | move [-40 0 0])
    (intersect red-box green-sphere | move [+40 0 0])
    (intersect :r 10 green-sphere red-box | move [-40 0 -80])
    (intersect :r 10 red-box green-sphere | move [+40 0 -80])
    (intersect :r 10 :distance 0 green-sphere red-box  | move [-120 0 -80])
    (intersect :r 10 :distance 0 red-box green-sphere | move [+120 0 -80])
    (intersect :rs 10 green-sphere red-box | move [-40 0 +80])
    (intersect :rs 10 red-box green-sphere | move [+40 0 +80])
    (intersect :rs 10 :distance 0 green-sphere red-box  | move [-120 0 +80])
    (intersect :rs 10 :distance 0 red-box green-sphere | move [+120 0 +80])
  | intersect (box [1000 80 1000] | move y -66))
  `

  "!boolean subtract"
  `
  (def green-sphere (sphere 40 | blinn-phong [0.05 0.95 0.05]))
  (def red-box (box 30 | blinn-phong [0.95 0.05 0.05]))
  (union
    (subtract green-sphere red-box | move [-40 0 0])
    (subtract red-box green-sphere | move [+40 0 0])
    (subtract :r 10 green-sphere red-box | move [-40 0 -80])
    (subtract :r 10 red-box green-sphere | move [+40 0 -80])
    (subtract :r 10 :distance 0 green-sphere red-box  | move [-120 0 -80])
    (subtract :r 10 :distance 0 red-box green-sphere | move [+120 0 -80])
    (subtract :rs 10 green-sphere red-box | move [-40 0 +80])
    (subtract :rs 10 red-box green-sphere | move [+40 0 +80])
    (subtract :rs 10 :distance 0 green-sphere red-box  | move [-120 0 +80])
    (subtract :rs 10 :distance 0 red-box green-sphere | move [+120 0 +80])
  | intersect (box [1000 80 1000] | move y -66))
  `

  "!boolean union interior color fields"
  [ortho-z `
  (def green-sphere (sphere 44 | color [0.05 0.95 0.05]))
  (def red-box (box 40 | color [0.95 0.05 0.05]))
  (union
    (union :r 10 green-sphere red-box | move [-50 -50 0])
    (union :r 10 red-box green-sphere | move [50 -50 0])
    (union :rs 10 green-sphere red-box | move [-50 50 0])
    (union :rs 10 red-box green-sphere | move [50 50 0])
  | slice z 0)
  `]

  "!boolean intersect interior color fields"
  [ortho-z `
  (def green-sphere (sphere 44 | color [0.05 0.95 0.05]))
  (def red-box (box 40 | color [0.95 0.05 0.05]))
  (union
    (intersect :r 10 green-sphere red-box | move [-50 -50 0])
    (intersect :r 10 red-box green-sphere | move [50 -50 0])
    (intersect :rs 10 green-sphere red-box | move [-50 50 0])
    (intersect :rs 10 red-box green-sphere | move [50 50 0])
  | slice z 0)
  `]

  "!shadow banding artifacts"
  [ortho-z `
  (union
    (sphere 50 | blinn-phong [1 1 1] | with-lights (light/point [1 1 1] [500 -50 0]) | move y -50)
    (sphere 50 | blinn-phong [1 1 1] | with-lights (light/directional [1 1 1] [-1 0 0] 500) | move y 50))
  `]

  "!raymarcher tries not to penetrate shape 1" `
  (sphere 50
    | morph 2 (box 50)
    | blinn-phong [1 1 1])
  `
  "!raymarcher tries not to penetrate shape 2" [ortho-z `
  (sphere 50
    | morph 2 (box 50)
    | blinn-phong [1 1 1])
  `]

  "!scale" `
  (union
    (rect 30)
    (rect 30 | scale [0.5 1] | move x -67)
    (rect 30 | scale [2 0.5] | move y 65)
    (rect 30 | scale 0.75 | move x 71)
    (rect 30 | scale 0.5 | pivot [30 30] | move y -70))
  `

  "!gl helpers" `
  (union
    (gl/let [foo [50 0]] (rect 30 | move foo))
    (gl/with [q (- q [-50 0])] (rect 30))
    )
  `

  "!gl with color fields" `
  (union
    (gl/with :color [normal (normal + (perlin p * 0.1))]
      (sphere 100 | blinn-phong [1 0 0] | move [-50 0 0]))
    (sphere 100 | blinn-phong [1 0 0] | move [50 0 0]
    | gl/with :color [normal (normal + (perlin p * 0.1))] _)
  )
  `

  "!tile" `
  (sphere 30 | tile [80 80 80] :limit [1 1 1])
  `

  "!tile 2d with asymmetric shape, no oversampling" `
  (rect 30 | rotate 0.3 | tile [80 80])
  `
  "!tile 2d with asymmetric shape, oversampling" `
  (rect 30 | rotate 0.3 | tile [80 80] :oversample true)
  `

  "!tile 3d with asymmetric shape, no oversampling" `
  (cone y 30 60
  | blinn-phong [1 0 0]
  | rotate x -1 y 2 z 3
  | tile [60 60 60] :limit [1 1 1]
  )
  `
  "!tile 3d with asymmetric shape, oversampling" `
  (cone y 30 60
  | blinn-phong [1 0 0]
  | rotate x -1 y 2 z 3
  | tile [60 60 60] :limit [1 1 1] :oversample true
  )
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
.images .new .underlay:hover {
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

(import ../src/lib :as bauble)
(defn compile-shader-new [source]
  (let [env (bauble/evaluator/evaluate source)
        [animated? shader-source] (bauble/renderer/render env "330")]
    shader-source))

(defn img [filename &opt class]
  (printf `<img%s src="%s" width="%d" height="%d" />`
    (if class (string ` class="` class `"`) "")
    filename (display-resolution 0) (display-resolution 1)))

(defn render [name program camera-origin camera-orientation compile-function &opt suffix]
  (var success true)
  (try (do
    (def before-compile (os/time))
    (def shader-source (compile-function program))
    (def after-compile-before-render (os/time))
    (def image (render-image shader-source
      :resolution physical-resolution
      :camera-origin camera-origin
      :camera-orientation camera-orientation
      ))
    (def after-render-before-export (os/time))
    (def temporary-file-name "snapshots/tmp.png")
    (jaylib/export-image image temporary-file-name)
    (def after-export-before-hash (os/time))
    (def hash (string/slice ($<_ shasum -ba 256 snapshots/tmp.png) 0 32))
    (def after-hash (os/time))
    (def new-file-name (string "snapshots/" hash ".png"))
    (def ref-name (string (string/replace-all " " "-" name) suffix ".png"))
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

    # (os/time) returns an integer number of seconds, so...
    # (printf `<div class="stats">%.3f compile, %.3f render, %.3f export, %.3f hash</div>`
    #   (- after-compile-before-render before-compile)
    #   (- after-render-before-export after-compile-before-render)
    #   (- after-export-before-hash after-render-before-export)
    #   (- after-hash after-export-before-hash))
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
  (def [[camera-origin camera-orientation] program]
    (if (tuple? program) program [isometric program]))

  (def program (string/trim program))

  (print `<div class="test-case">`)
  (printf `<h1>%s</h1>` (html-escape name))
  (printf `<pre>%s</pre>` (html-escape program))
  (def success (render name program camera-origin camera-orientation compile-shader))
  (def success2
    (if (string/has-prefix? "!" name)
      (render name program camera-origin camera-orientation compile-shader-new "-new")
      true))
  (unless (and success success2) (++ failing-tests))
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
