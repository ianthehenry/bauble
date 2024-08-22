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
  "!union smooth" `(smooth-union 10 (circle 75 | move [-30 0]) (rect 60 | move [30 0]))`
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
      (smooth-union 10 (circle 50 | color [1 1 0.5] | move [-30 0]) (circle 50 | color [1 0.5 1] | move [30 0])
      | move [0 60])
      (smooth-union 10 (circle 50 | color [1 0.5 1] | move [30 0]) (circle 50 | color [1 1 0.5] | move [-30 0])
      | move [0 -60])
      )
  `
  "!union smooth can combine non-overlapping shapes" `
    (smooth-union 20
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
      ;(seq [x :in [-50 50] y :in [-50 50] :let [offset [x y]] :after (++ i)]
        (smooth-union 5 ;(drop i shapes) ;(take i shapes)
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

  "!3d rotation" `
  (union
    (box 40 | rotate y 0.2 | move [-80 0 80])
    (box 40 | rotate y 0.2 x 0.5 | move [80 0 -80])
    (box 40 | rotate x 0.3 y 0.5 | move [80 0 80])
    (box 40 | rotate (normalize [1 0 1]) 1 | move [-80 0 -80]))`
})

(each filename (os/dir "./cases")
  (def name (string/remove-suffix filename ".janet"))
  (put test-cases name (slurp (string "./cases/" filename))))

(def out-buffer @"")
(setdyn *out* out-buffer)

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
    (def final-file-name (string "snapshots/" hash ".png"))
    (def ref-name (string (string/replace-all " " "-" name) suffix ".png"))
    (put current-refs ref-name true)
    (def ref-path (string "refs/" ref-name))
    (if (os/stat final-file-name)
      (do
        (eprin ".")
        (os/rm temporary-file-name))
      (do
        (eprin ":")
        (os/rename temporary-file-name final-file-name)
        (set success false)))
    ($ ln -fs (string "../" final-file-name) ,ref-path)

    (printf `<pre class="shader-source">%s</pre>` (html-escape shader-source))
    (printf `<img src="%s" width="%d" height="%d" />` final-file-name (display-resolution 0) (display-resolution 1))

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
