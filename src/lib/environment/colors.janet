(use ./import)
(use ./surfacing)

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
