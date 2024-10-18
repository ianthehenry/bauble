(use ./import)

(defhelper :vec3 hsv [:float hue :float saturation :float value]
  ```
  Returns a color.
  ```
  (var c (hue * 6 + [0 4 2] | mod 6 - 3 | abs))
  (return (value * (mix (vec3 1) (c - 1 | clamp 0 1) saturation))))

(defhelper :vec3 hsl [:float hue :float saturation :float lightness]
  ```
  Returns a color.
  ```
  (var c (hue * 6 + [0 4 2] | mod 6 - 3 | abs))
  (return (* saturation (c - 1 | clamp 0 1 - 0.5) (1 - abs (2 * lightness - 1)) + lightness)))

(defmacro- defcolor [name color]
  (def docstring (string ````
  ```example
  (set background-color ````name````)
  (ball 100 | shade ````name````)
  ```
  ````))
  ~(def ,name ,docstring ,color))

(defcolor red      (hsv (/ 0.0 6) 0.98 1))
(defcolor orange   (hsv (/ 0.5 6) 0.98 1))
(defcolor yellow   (hsv (/ 1.0 6) 0.98 1))
(defcolor lime     (hsv (/ 1.5 6) 0.98 1))
(defcolor green    (hsv (/ 2.0 6) 0.98 1))
(defcolor teal     (hsv (/ 2.5 6) 0.98 1))
(defcolor cyan     (hsv (/ 3.0 6) 0.98 1))
(defcolor sky      (hsv (/ 3.5 6) 0.98 1))
(defcolor blue     (hsv (/ 4.0 6) 0.98 1))
(defcolor purple   (hsv (/ 4.5 6) 0.98 1))
(defcolor magenta  (hsv (/ 5.0 6) 0.98 1))
(defcolor hot-pink (hsv (/ 5.5 6) 0.98 1))

(defcolor white      [1    1    1])
(defcolor light-gray [0.75 0.75 0.75])
(defcolor gray       [0.5  0.5  0.5])
(defcolor dark-gray  [0.25 0.25 0.25])
(defcolor black      [0.03 0.03 0.03])
(def transparent ````

This is a `vec4`, not a `vec3`, so you
can basically only use it as a background color.

```example
(set background-color transparent)
```
```` (vec4 0))

(defhelper :vec3 ok/mix [:vec3 from :vec3 to :float by]
  ````
  Linearly interpolate between two RGB colors using the Oklab color space. This is the
  same as converting them to the Oklab color space, mixing them, and then converting
  back to RGB, but it's more efficient.

  ```example
  (union 
    (rect [200 50] | color (ok/mix red blue (q.x / 200 | remap+)) | move y 50)
    (rect [200 50] | color (mix    red blue (q.x / 200 | remap+)) | move y -50))
  ```
  ````
  (def to-lms-mat (mat3
     0.4121656120 0.2118591070 0.0883097947
     0.5362752080 0.6807189584 0.2818474174
     0.0514575653 0.1074065790 0.6302613616))
  (def of-lms-mat (mat3
     4.0767245293 -1.2681437731 -0.0041119885
    -3.3072168827  2.6093323231 -0.7034763098
     0.2307590544 -0.3411344290  1.7068625689))

  (var from-lms (pow (to-lms-mat * from) (/ 3)))
  (var to-lms (pow (to-lms-mat * to) (/ 3)))
  (var lms (mix from-lms to-lms by))
  (return (of-lms-mat * (* lms lms lms))))

(defhelper :vec3 ok/of-rgb [:vec3 rgb]
  ````
  Convert an Oklab color to a linear RGB color. You can use this,
  along with `ok/to-rgb`, to perform color blending in the Oklab
  color space.

  In these examples, Oklab is on the left, and linear RGB mixing is
  on the right:

  ```example
  (union
    (morph (osc t 10 | ss 0.01 0.99)
      (ball 50 | shade yellow | map-color ok/of-rgb)
      (box 50 | shade blue | map-color ok/of-rgb)
    | map-color ok/to-rgb
    | move [-60 0 60])
    (morph (osc t 10 | ss 0.01 0.99)
      (ball 50 | shade yellow)
      (box 50 | shade blue)
    | move [60 0 -60]))
  ```

  ```example
  (union
  (union :r (osc t 5 0 30)
    (box 50 | shade red | map-color ok/of-rgb)
    (ball 40 | shade blue | map-color ok/of-rgb | move y 50)
  | map-color ok/to-rgb
  | move [-60 0 60])
  (union :r (osc t 5 0 30)
    (box 50 | shade red)
    (ball 40 | shade blue | move y 50)
  | move [60 0 -60]))
  ```
  ````

  (def invB (mat3 0.4121656120 0.2118591070 0.0883097947
                  0.5362752080 0.6807189584 0.2818474174
                  0.0514575653 0.1074065790 0.6302613616))

  (def invA (mat3 0.2104542553 1.9779984951 0.0259040371
                  0.7936177850 -2.4285922050 0.7827717662
                  -0.0040720468 0.4505937099 -0.8086757660))
  (var lms (invB * rgb))
  (return (invA * (sign lms * pow (abs lms) (/ 3)))))

(defhelper :vec3 ok/to-rgb [:vec3 ok]
  ````
  Convert a linear RGB color to the Oklab color space. See `ok/of-rgb` for examples.
  ````
  (def fwdA (mat3 1 1 1
                  0.3963377774 -0.1055613458 -0.0894841775
                  0.2158037573 -0.0638541728 -1.2914855480))

  (def fwdB (mat3 4.0767245293 -1.2681437731 -0.0041119885
                  -3.3072168827 2.6093323231 -0.7034763098
                  0.2307590544 -0.3411344290  1.7068625689))
  (var lms (fwdA * ok))
  (return (fwdB * (* lms lms lms))))

(defhelper :vec3 ok/hcl [:float hue :float chroma :float lightness]
  ````
  This is a way to generate colors in the Oklab color space.

  Oklab colors maintain "perceptual brightness" better than `hsv` or `hsl`:

  ```example
  (union 
    (rect [200 50] | color (hsv    (q.x / 200 | remap+) 1 1) | move y 51)
    (rect [200 50] | color (ok/hcl (q.x / 200 | remap+) 0.5 0.5) | move y -51))
  ```

  Note that there is no yellow in that rainbow, because yellow is a bright color. If we increase
  the lightness above 0.5, we notice that pure blue disappears:

  ```example
  (union 
    (rect [200 50] | color (hsv    (q.x / 200 | remap+) 1 1) | move y 51)
    (rect [200 50] | color (ok/hcl (q.x / 200 | remap+) 0.5 1) | move y -51))
  ```

  Because pure blue is a dark color.

  `chroma` is analogous to "saturation," and should approximately range from 0 to 0.5.

  ```example
  (union 
    (rect [200 50] | color (hsv    (q.x / 200 | remap+) (q.y / 50 | remap+) 1) | move y 101)
    (rect [200 50] | color (ok/hcl (q.x / 200 | remap+) (q.y / 50 | remap+) 1) | move y 0)
    (rect [200 50] | color (ok/hcl (q.x / 200 | remap+) (q.y / 50 | remap+) 0.5) | move y -101))
  ```

  `lightness` should approximately range from 0 to 1, but is not properly defined at all hues or chromas.
  For example, if we try to make a high-chroma yellow too dark, it slips into being green instead:

  ```example
  (union 
    (rect [200 50] | color (hsv    (q.x / 200 | remap+) 1 (q.y / 50 | remap+)) | move y 151)
    (rect [200 50] | color (ok/hcl (q.x / 200 | remap+) 0.25 (q.y / 50 | remap+)) | move y 50)
    (rect [200 50] | color (ok/hcl (q.x / 200 | remap+) 0.5 (q.y / 50 | remap+)) | move y -51)
    (rect [200 50] | color (ok/hcl (q.x / 200 | remap+) 0.75 (q.y / 50 | remap+)) | move y -151))
  ```
  ````
  (var theta (* hue tau))
  (return [lightness (chroma * cos theta) (chroma * sin theta) | ok/to-rgb]))
