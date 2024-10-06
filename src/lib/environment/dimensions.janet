(use ./import)
(use ./axis-helpers)
(use ./transforms)

(put (curenv) 'inf (dyn 'math/inf))

(defn revolve
  ````
  Revolve a 2D shape around the given `axis` to return a 3D shape.

  ```example
  (revolve (triangle 100) y)
  ```

  This lets you create shapes that look like they were turned on a lathe:

  ```example
  (union :r 10
    (circle 40 | move y 80)
    (rect [(ss q.y -90 -52 60 10) 100])
    (rect [(ss q.y -70 57 58 20 * ss q.y -80 -5) 80] | move y -38)
    (rect :r 5 [20 5] | move y 40 | rotate -0.48)
    #| view
    | revolve y)
  ```

  You can optionally supply an `offset` to move the shape away from the
  origin first (the default is `0`).

  ```example
  (revolve (triangle 50) y 50)
  ```

  You can use this to create different types of toroidal shapes:

  ```example
  (revolve (star 30 50 | rotate t) y 100)
  ```
  ````
  [shape axis &opt offset]
  (def [this others] (split-axis axis))
  (def offset (typecheck (@or offset 0) jlsl/type/float))

  (shape/map shape (fn [expr]
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
    (typecheck (@or distance 0) jlsl/type/float)))

  (def in-3d (shape/map shape (fn [expr]
    (jlsl/with "extrude" [q ,others] expr))
    jlsl/type/vec3))

  (if distance
    (shape/map-distance in-3d (fn [expr]
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
  (def position (typecheck (@or position 0) jlsl/type/float))

  (def new-p (jlsl/coerce-expr (sugar (case axis
    x [position q]
    y [q.x position q.y]
    z [q position]
    (errorf "unknown axis %q" (jlsl/show axis))))))

  (shape/map shape (fn [expr]
    (jlsl/with "slice" [p ,new-p] ,expr))
    jlsl/type/vec2))

(defn sliced
  ````
  Take a 2D slice of a 3D shape at a given `position` along the supplied `axis`,
  and then project it back into 3D space at the same spot.

  This is useful for quickly looking inside shapes:

  ```example
  (union
    (box 80 | shade red)
    (ball 100 | shade green)
  # try commenting out this line:
  | sliced y (sin t * 100)
  )
  ```
  ````
  [shape axis &opt position]
  (def position (typecheck (@or position 0) jlsl/type/float))
  (sugar (gl/let [position position]
    (slice shape axis position | extrude axis | move axis position))))

(defmacro- bezier-builder [op type pos]
  (sugar ~(,op :float bezier-t [,type A ,type B ,type C]
    (var a (B - A))
    (var b (A - (2 * B) + C))
    (var d (A - ,pos))
    (var kk (dot b /))
    (var kx (kk * dot a b))
    (var ky (kk * (2 * dot a + dot d b)))
    (var kz (kk * dot d a))
    # there is a weird discontinuity when p is very close to zero
    # and i can't figure out how to fix it
    (var p (ky / 3 - (kx * kx)))
    (var q (kx * (2 * kx * kx - ky) + kz))
    (var h (q * q + (* 4 p p p)))
    (if (>= h 0)
      (do
        (set h (sqrt h))
        (var x [h (- h) - q / 2])
        (var uv (sign x * (pow (abs x) (1 / 3))))
        (return (uv.x + uv.y - kx)))
      (do
        (var z (sqrt (- p)))
        (var v (acos (q / (p * z * 2)) / 3))
        (var m (cos v))
        (var n (sin v * sqrt 3))
        (var tv [(m + m) (n + m -) * z - kx])
        (var c (a * 2))
        (var r1 (b * tv.x + c * tv.x + d | dot))
        (var r2 (b * tv.y + c * tv.y + d | dot))
        (return (if (< r1 r2) tv.x tv.y)))))))

(bezier-builder defhelper- :vec2 q)
(bezier-builder overload :vec3 p)

(sugar (defhelper- :mat3 bezier-orient [:vec3 A :vec3 B :vec3 C :float t :vec3 up]
  (var tangent (2 - (2 * t) * (B - A) + (2 * t * (C - B)) | normalize))
  (var binormal (cross tangent up | normalize))
  (var normal (cross tangent binormal))
  (var t1 (cross normal tangent))
  (return (mat3 t1 (cross tangent t1) tangent))))

(defn- ndot [a b] (sugar (a.x * b.y - (b.x * a.y))))

(sugar (overload :mat2 bezier-orient [:vec2 A :vec2 B :vec2 C :float t :vec2 up]
  (var tangent (2 - (2 * t) * (B - A) + (2 * t * (C - B)) | normalize))
  (var normal [1 -1 * tangent.yx])
  (return (mat2 (dot up normal) (ndot up normal)
                (ndot normal up) (dot up normal)))))

(defhelper- :vec3 bezier-position [:vec3 A :vec3 B :vec3 C :float t]
  (return (mix (mix A B t) (mix B C t) t)))

(overload :vec2 bezier-position [:vec2 A :vec2 B :vec2 C :float t]
  (return (mix (mix A B t) (mix B C t) t)))

(sugar (defhelper- :vec4 bezier-extrusion [:vec3 A :vec3 B :vec3 C :vec3 up :float from :float to]
  (var t (bezier-t A B C))
  (var rotation (bezier-orient A B C t up))
  (set t (clamp t from to))
  (return [((bezier-position A B C t) - p * rotation) t])))

(sugar (overload :vec3 bezier-extrusion [:vec2 A :vec2 B :vec2 C :vec2 up :float from :float to]
  (var t (bezier-t A B C))
  (var rotation (bezier-orient A B C t up))
  (set t (clamp t from to))
  (return [((bezier-position A B C t) - q * rotation) t])))

(defn- bezier-3d [shape A B C up from to]
  (sugar (gl/let [t (bezier-t A B C)
                  t* (clamp t from to)
                  projection (bezier-position A B C t*)]
    (def subject (if (function? shape) (shape t*) shape))
    (if (shape/shape? subject)
      (gl/with [p (projection - p * bezier-orient A B C t up)]
        (case (shape/type subject)
          jlsl/type/vec2 (subject | extrude z)
          jlsl/type/vec3 subject
          (error "BUG")))
      (gl/with [p (projection - p)]
        (let [radius (typecheck subject jlsl/type/float)]
          (shape/3d (length p - radius))))))))

(defn- bezier-2d [shape A B C up from to]
  (def up (axis-vector-2d up))
  (sugar (gl/let [t (bezier-t A B C)
                  t* (clamp t from to)
                  projection (bezier-position A B C t*)]
    (def subject (if (function? shape) (shape t*) shape))
    (if (shape/shape? subject)
      (gl/with [q (projection - q * bezier-orient A B C t up)]
        (case (shape/type subject)
          jlsl/type/vec2 subject
          jlsl/type/vec3 (error "cannot extrude a 3D shape along a 2D bezier curve")
          (error "BUG")))
      (gl/with [q (projection - q)]
        (let [radius (typecheck subject jlsl/type/float)]
          (shape/2d (length q - radius))))))))

(defnamed bezier [shape start control end :?up :?from :?to]
  ````
  Returns a 2D or 3D quadratic bezier curve, or extrudes a shape along that curve.

  A quadratic bezier curve is defined by three points: a start point, an end point, and a control point.

  If you connect a line between the start and the control point, and another line between the control point
  and the end point, you will have two legs of a triangle. If you then move a point along each line,
  draw a line between those two points, and then move a point along *that* line, you will get a quadratic
  bezier curve.

  ```example
  (def start [-200 (osc (t + 20) 19.1 -200 200)])
  (def middle [0 (osc (t + 29) 20.2 -200 200)])
  (def end [200 (oss t 21.3 -200 200)])
  (def h (sin+ t))

  (def v1 (mix start middle h))
  (def v2 (mix middle end h))
  (union
    (union :r 5
      (line start middle 2)
      (circle 3 | move v1)
    | color red)
    (union :r 5
      (line middle end 2)
      (circle 3 | move v2)
    | color sky)
    (bezier 2 start middle end | color white)
    (union :r 5
      (circle 5 | move (mix v1 v2 h))
      (line v1 v2 2)
    | color magenta))
  ```

  Maybe better intuition is that you linearly interpolate between two pairs of points,
  then linearly interpolate between that linear interpolation, you get a quadratic bezier
  curve.

  Like `line`, bezier curves are defined in 2D or in 3D:

  ```example
  (def start [-200 (osc (t + 20) 19.1 -200 200) (osc (t + 20) 21.5 -100 100)])
  (def middle [0 (osc (t + 29) 20.2 -200 200) (osc (t + 29) 19.2 -100 100)])
  (def end [200 (oss t 21.3 -200 200) (osc (t + 29) 18.5 -100 100)])
  (def h (sin+ t))

  (def v1 (mix start middle h))
  (def v2 (mix middle end h))
  (union
    (union :r 5
      (line start middle 2)
      (ball 3 | move v1)
    | color red)
    (union :r 5
      (line middle end 2)
      (ball 3 | move v2)
    | color sky)
    (bezier 2 start middle end | color white)
    (union :r 5
      (ball 5 | move (mix v1 v2 h))
      (line v1 v2 2)
    | color magenta)
  | union (ground -210 | shade dark-gray))
  ```

  Thes simplest version of a bezier curve produces round lines:

  ```example
  (bezier (osc t 3 1 10) [-100 0] [0 -100] [100 0] | color white)
  ```

  But you can also pass a shape instead of a float as the first argument,
  in which case you will *extrude* the shape along the curve, which you
  can use to produce differently-shaped lines:

  ```example
  (bezier (rect (osc t 3 1 10)) [-100 0] [0 -100] [100 0] | color white)
  ```

  Or, in 3D, more interesting curves:

  ```example
  (bezier (torus y 20 (osc t 3 1 10)) [-100 0 100] [0 -100 0] [100 0 -100])
  ```

  You can also vary the shape over the course of the extrusion, by passing a function
  as the first argument:

  ```example
  (bezier (fn [$t] (mix 1 10 $t)) [-100 0] [0 -100] [100 0] | color white)
  ```

  Although the `bezier:` helper gives you a slightly more convenient way to write
  this that fits into a pipeline:

  ```example
  (triangle [10 (osc $t 0.1 1 20)]
  | bezier: $t [-100 0] [0 -100] [100 0] | color white)
  ```

  You can also pass `:from` and `:to` to constrain the extrusion.

  ```example
  (box 20 | shade red | subtract (ball 23 | shade green) | rotate z ($t * tau)
  | bezier: $t [-100 0 100] [0 -100 0] [100 0 -100] :to (osc t 3 0 1))
  ```

  The shape will be oriented along the curve according to the vector `:up`, which determines
  what direction will become normal to the curve. The default is `+y`, but you can specify
  any normalized vector:

  ```example
  (torus y 30 5
  | bezier [-100 0 100] [0 -100 0] [100 0 -100]
    :up y
    :from 0
    :to (sin+ t))
  ```
  ```example
  (torus y 30 5
  | bezier [-100 0 100] [0 -100 0] [100 0 -100]
    :up z
    :from 0
    :to (sin+ t))
  ```
  ```example
  (torus y 30 5
  | bezier [-100 0 100] [0 -100 0] [100 0 -100]
    :up x
    :from 0
    :to (sin+ t))
  ```
  ````
  (default up y)
  (default from 0)
  (default to 1)
  (def f (case (jlsl/expr/type (jlsl/coerce-expr start))
    jlsl/type/vec2 bezier-2d
    jlsl/type/vec3 bezier-3d
    (error "you gotta gimme a point here")))
  (f shape start control end up from to))

(defmacro bezier:
  ````
  Like `bezier`, but implicitly wraps its first argument in an anonymous function. See `bezier` for examples.
  ````
  [shape $t & args]
  ~(,bezier (fn [,$t] ,shape) ,;args))
