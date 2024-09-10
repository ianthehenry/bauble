(use ./import)

(defhelper :vec3 perspective [:float fov :vec2 size :vec2 pos]
  ```
  TODOC
  ```
  (var xy (pos - (size * 0.5)))
  (var cot-half-fov (tan (radians (90 - (fov * 0.5)))))
  (var z (* -0.5 (min size.x size.y) cot-half-fov))
  (return (normalize [xy z])))
