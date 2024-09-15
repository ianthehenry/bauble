(use ./import)
(use ./rotation)

(defhelper :vec3 perspective-vector [:float fov]
  ```
  Returns a unit vector pointing in the `-z` direction for the
  given camera field-of-view (degrees).
  ```
  (var cot-half-fov (tan (radians (90 - (fov * 0.5)))))
  (var z (* -0.5 cot-half-fov))
  (return (normalize [frag-coord z])))

(sugar (defnamed camera/perspective [pos target :?fov]
  ```
  Returns a ray from a perspective camera located at `pos` and aiming towards `target`.

  You can change the field of view by passing `:fov` with a number of degrees. The default is `60`, and
  the default orbiting free camera uses `45`.
  ```
  (default fov 60)
  (def pos (typecheck pos jlsl/type/vec3))
  (def target (typecheck target jlsl/type/vec3))
  (Ray pos (alignment-matrix -z (normalize (target - pos)) * perspective-vector fov))))
