(use ./import)

(put (curenv) 'inf (dyn 'math/inf))

(defn- split-axis [axis]
  (sugar (case axis
    x [p.x p.yz]
    y [p.y p.xz]
    z [p.z p.xy]
    (errorf "unknown axis %q" (jlsl/show axis)))))

(defn revolve
  ```
  Revolve a 2D shape around the given `axis` to return a 3D shape.

  You can optionally supply an `offset` to move the shape away from the origin first (the default is `0`).
  ```
  [shape axis &opt offset]
  (def [this others] (split-axis axis))
  (def offset (typecheck (jlsl/coerce-expr (@or offset 0)) jlsl/type/float))

  (field-set/map shape (fn [expr]
    (jlsl/with "revolve" [q [(- (length ,others) ,offset) ,this]] expr))
    jlsl/type/vec3))

(defn extrude
  ```
  Extrude a 2D shape into 3D along the given `axis`.

  `distance` defaults to `0` and determines the width, length, or height of the final shape.
  You can also pass `inf` to get an infinite extrusion (which is slightly cheaper to compute).
  ```
  [shape axis &opt distance]
  (def [this others] (split-axis axis))
  (def distance (if (not= math/inf distance)
    (typecheck (jlsl/coerce-expr (@or distance 0)) jlsl/type/float)))

  (def in-3d (field-set/map shape (fn [expr]
    (jlsl/with "extrude" [q ,others] expr))
    jlsl/type/vec3))

  (if distance
    (field-set/map-distance in-3d (fn [expr]
      (sugar (jlsl/do "extrude"
        (var w [,expr (- (abs ,this) ,distance)])
        (min (max w) 0 + length (max w 0))))))
    in-3d))

(defn slice
  ```
  Take a 2D slice of a 3D shape at a given `position` along the supplied `axis`.

  `position` defaults to `0`.
  ```
  [shape axis &opt position]
  (def position (typecheck (jlsl/coerce-expr (@or position 0)) jlsl/type/float))

  (def new-p (jlsl/coerce-expr (sugar (case axis
    x [position q]
    y [q.x position q.y]
    z [q position]
    (errorf "unknown axis %q" (jlsl/show axis))))))

  (field-set/map shape (fn [expr]
    (jlsl/with "slice" [p ,new-p] ,expr))
    jlsl/type/vec2))
