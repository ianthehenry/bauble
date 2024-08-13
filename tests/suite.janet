(import jaylib)
(use sh)
(use ../cli/helpers)

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
div.test-case {
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
</style>`)
(print `</head>`)
(print `<body>`)

(defn html-escape [x] (->> x
  (string/replace-all "&" "&amp;")
  (string/replace-all "<" "&lt;")
  (string/replace-all ">" "&gt;")))

(var failing-tests 0)
(each [name program] (sort (pairs test-cases) (fn [[name1 _] [name2 _]] (< name1 name2)))
  (def [[camera-origin camera-orientation] program]
    (if (tuple? program) program [isometric program]))

  (def program (string/trim program))

  (print `<div class="test-case">`)
  (printf `<h1>%s</h1>` (html-escape name))
  (printf `<pre>%s</pre>` (html-escape program))

  (var success true)
  (try (do
    (def before-compile (os/time))
    (def shader-source (compile-shader program))
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
    (def symlink-name (string "refs/" (string/replace-all " " "-" name) ".png"))
    (if (os/stat final-file-name)
      (do
        (eprin ".")
        (os/rm temporary-file-name))
      (do
        (eprin ":")
        (os/rename temporary-file-name final-file-name)
        (set success false)))
    ($ ln -fs (string "../" final-file-name) ,symlink-name)

    (printf `<pre>%s</pre>` (html-escape shader-source))
    (printf `<img src="%s" width="%d" height="%d" />` symlink-name (display-resolution 0) (display-resolution 1))
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
      (printf `<pre>%s</pre>` (html-escape buf))

      ))

  (unless success (++ failing-tests))
  (print `</div>`)


  )

(print `</body>`)
(print `</html>`)

(eprint)
(when (> failing-tests 0)
  (eprintf "%d tests failed" failing-tests))

(spit "summary.html" out-buffer)

# (when (> failing-tests 0)
#   ($ open ./summary.html))
