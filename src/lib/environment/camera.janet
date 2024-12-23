(use ./import)
(use ./rotation)
(use ./transforms)

(defhelper :vec3 perspective-vector [:float fov]
  ```
  Returns a unit vector pointing in the `+z` direction for the
  given camera field-of-view (in degrees).
  ```
  (var cot-half-fov (tan (radians (90 - (fov * 0.5)))))
  (return [frag-coord cot-half-fov | normalize]))

(sugar (defnamed camera/perspective [position :?target :?dir :?roll :?fov]
  ````
  Returns a camera with a perspective projection located at `position` and
  pointing towards the origin. You can have the camera face another point
  by passing `:target`, or set the orientation explicitly by passing a
  normalized vector as `:dir` (you can't pass both).

  You can change the field of view by passing `:fov` with a number of degrees. The default is `60`, and
  the default orbiting free camera uses `45`.

  ```example
  (morph (ball 50) (box 50) 2
  | union (circle 200 | extrude y 10 | move y -100)
  | shade (vec3 0.75))
  (def pos [(sin t * 200) (cos+ (t / 2) * 300) 500])
  (set camera (camera/perspective pos :fov 45))
  ```
  ````
  (default fov 60)
  (def position (typecheck position jlsl/type/vec3))
  (def dir (typecheck? dir jlsl/type/vec3))
  (def target (typecheck? target jlsl/type/vec3))
  (def roll (typecheck? roll jlsl/type/float))
  (when (@and target dir) (error "well which is it, target or dir"))
  (def dir (if dir dir (if target
    (target - position | normalize)
    [0 0 0 - position | normalize])))
  (def up (if roll (rotate y roll) y))
  (PerspectiveCamera position dir up fov)))

(sugar (defnamed camera/orthographic [position :?target :?dir :?roll :?scale]
  ````
  Returns a camera with an orthographic projection and the given `:scale`
  (default 512). Other arguments are the same as `camera/perspective`.

  ```example
  (morph (ball 50) (box 50) 2
  | union (circle 200 | extrude y 10 | move y -100)
  | shade (vec3 0.75))
  (def pos [(sin t * 200) (cos+ (t / 2) * 300) 500])
  (set camera (camera/orthographic pos))
  ```

  An orthographic camera shoots every ray in the same direction, but from a different
  origin. In effect it produces an image without any sense of depth, because objects
  farther away from the camera don't get smaller. Compare the following scenes, with
  a typical perspective camera:

  ```example
  (box 50 | tile [150 0 150])
  (set camera (camera/perspective [1 1 1 | normalize * 512]))
  ```

  And the same scene with an orthographic camera:

  ```example
  (box 50 | tile [150 0 150])
  (set camera (camera/orthographic [1 1 1 | normalize * 512]))
  ```
  ````
  (default scale 512)
  (def position (typecheck position jlsl/type/vec3))
  (def dir (typecheck? dir jlsl/type/vec3))
  (def target (typecheck? target jlsl/type/vec3))
  (def roll (typecheck? roll jlsl/type/float))
  (when (@and target dir) (error "well which is it, target or dir"))
  (def dir (if dir dir (if target
    (target - position | normalize)
    [0 0 0 - position | normalize])))
  (def up (if roll (rotate y roll) y))
  (OrthographicCamera position dir up scale)))

(sugar (defn camera/ray
  ````
  Returns the perspective-adjusted ray from this camera for
  the current `frag-coord`. You probably don't need to call
  this.
  ````
  [camera]
  (gl/let [camera camera
           z-axis camera.direction
           x-axis (cross z-axis camera.up | normalize)
           y-axis (cross x-axis z-axis)]
    (case (jlsl/variable/type camera)
      (jlsl/type/coerce PerspectiveCamera)
        (Ray camera.position (mat3 x-axis y-axis z-axis * perspective-vector camera.fov))
      (jlsl/type/coerce OrthographicCamera)
        (gl/let [rotation-matrix (mat3 x-axis y-axis z-axis)]
          (Ray (rotation-matrix * [frag-coord 0] * camera.scale + camera.position) (rotation-matrix * +z)))
      (error "whoa whoa whoa, i can't take pictures with this")))))

(sugar (defnamed camera/pan [camera angle :?up]
  ````
  Rotate the camera left and right.

  ```example
  (morph (ball 50) (box 50) 2
  | union (circle 200 | extrude y 10 | move y -100)
  | shade (vec3 0.75))
  (set camera (camera/perspective [0 100 600] :fov 45
  | camera/pan (sin t * 0.2)))
  ```

  By default this rotation is relative to the camera's current
  orientation, so the image you see will always appear to be moving
  horizontally during a pan. But you can provide an absolute
  `:up` vector to ignore the camera's roll. (I think the difference
  is easier to understand if you unroll the camera afterward.)

    ```example
  (morph (ball 50) (box 50) 2
  | union (circle 200 | extrude y 10 | move y -100)
  | shade (vec3 0.75))
  (set camera (camera/perspective [0 100 600] :fov 45
  | camera/roll pi/4
  | camera/pan (sin t * 0.2)
  # | camera/roll -pi/4
  ))
  ```

  ```example
  (morph (ball 50) (box 50) 2
  | union (circle 200 | extrude y 10 | move y -100)
  | shade (vec3 0.75))
  (set camera (camera/perspective [0 100 600] :fov 45
  | camera/roll pi/4
  | camera/pan (sin t * 0.2) :up y
  # | camera/roll -pi/4
  ))
  ```
  ````
  (assert (camera? camera) "not a camera")
  (def angle (typecheck angle jlsl/type/float))
  (def up (typecheck? up jlsl/type/vec3))
  (gl/do "pan"
    (var camera camera)
    (set camera.direction (rotate camera.direction ,(@or up camera.up) (- angle)))
    camera)))

(sugar (defnamed camera/tilt [camera angle :?up]
  ````
  Rotate the camera up and down.

  ```example
  (morph (ball 50) (box 50) 2
  | union (circle 200 | extrude y 10 | move y -100)
  | shade (vec3 0.75))
  (set camera (camera/perspective [0 100 600] :fov 45
  | camera/tilt (sin t * 0.2)))
  ```

  As with `pan`, you can supply an absolute `:up` vector to use
  instead of the camera's current roll.

  ````
  (assert (camera? camera) "not a camera")
  (def angle (typecheck angle jlsl/type/float))
  (def up (typecheck? up jlsl/type/vec3))
  (gl/do "tilt"
    (var camera camera)
    (set camera.direction (rotate camera.direction (cross camera.direction ,(@or up camera.up) | normalize) angle))
    camera)))

(sugar (defn camera/roll
  ````
  Roll the camera around.

  ```example
  (morph (ball 50) (box 50) 2
  | union (circle 200 | extrude y 10 | move y -100)
  | shade (vec3 0.75))
  (set camera (camera/perspective [0 100 600] :fov 45
  | camera/roll (sin t * 0.2)))
  ```
  ````
  [camera angle]
  (assert (camera? camera) "not a camera")
  (def angle (typecheck angle jlsl/type/float))
  (gl/do "roll"
    (var camera camera)
    (set camera.up (rotate camera.up camera.direction angle))
    camera)))

(sugar (defn camera/zoom
  ````
  Zoom the camera by changing its field of view (for a perspective camera)
  or scale (for an orthographic camera).

  ```example
  (morph (ball 50) (box 50) 2
  | union (circle 200 | extrude y 10 | move y -100)
  | shade (vec3 0.75))
  (set camera (camera/perspective [0 100 600] :fov 45
  | camera/zoom (sin t * 0.2 + 1)))
  ```
  ````
  [camera amount]
  (def camera (jlsl/coerce-expr camera))
  (assert (camera? camera) "not a camera")
  (def amount (typecheck amount jlsl/type/float))
  (case (jlsl/expr/type camera)
    (jlsl/type/coerce PerspectiveCamera)
      (gl/do "zoom"
        (var camera camera)
        (set camera.fov (camera.fov / amount))
        camera)
    (jlsl/type/coerce OrthographicCamera)
      (gl/do "zoom"
        (var camera camera)
        (set camera.scale (camera.scale / amount))
        camera)
    (error "i'm freaking out here where is my camera"))))

(sugar (defn camera/dolly
  ````
  Move the camera forward or backward.

  ```example
  (morph (ball 50) (box 50) 2
  | union (circle 200 | extrude y 10 | move y -100)
  | shade (vec3 0.75))
  (set camera (camera/perspective [0 100 600] :fov 45
  | camera/dolly (sin t * 100)))
  ```

  Useful for Hitchcocking:

  ```example
  (morph (ball 50) (box 50) 2
  | union
    (circle 200 | extrude y 10 | move y -100)
    (box [100 200 50] | tile [300 0 300] :limit 4 | move [0 0 -1000])
  | shade (vec3 0.75))

  (set camera (camera/perspective [0 100 600] :fov 45
  | camera/dolly (sin+ t * -500)
  | camera/zoom (sin+ t + 1)
  ))
  ```
  ````
  [camera amount]
  (assert (camera? camera) "not a camera")
  (def amount (typecheck amount jlsl/type/float))
  (gl/do "dolly"
    (var camera camera)
    (set camera.position (camera.direction * amount + camera.position))
    camera)))
