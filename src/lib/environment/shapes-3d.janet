(use ./import)

(defshape/3d sphere [radius]
  "Returns a 3D shape."
  (- (length p) radius))

(defshape/3d box [size]
  ```
  Returns a 3D shape, a box with corners at `(- size)` and `size`. `size` will be coerced to a `vec3`.
  
  Think of `size` like the "radius" of the box: a box with `size.x = 50` will be `100` units wide.
  ```
  (var d (abs p - vec3 ,size))
  # TODO: i would like to add an overload such that this is just (max d)
  (max d 0 | length + (min (max d.x (max d.y d.z)) 0)))

