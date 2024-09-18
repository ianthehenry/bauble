(use ./import)
(use ./rotation)
(use ./transforms)

(defhelper :vec3 perspective-vector [:float fov]
  ```
  Returns a unit vector pointing in the `+z` direction for the
  given camera field-of-view (in degrees).
  ```
  (var cot-half-fov (tan (radians (90 - (fov * 0.5)))))
  (var z (* 0.5 cot-half-fov))
  (return [frag-coord z | normalize]))

(sugar (defnamed camera/perspective [position target :?fov]
  ````
  Returns the camera located at `position` and aiming towards `target`
  that has no roll.

  You can change the field of view by passing `:fov` with a number of degrees. The default is `60`, and
  the default orbiting free camera uses `45`.

  ```example
  (morph (ball 50) (box 50) 2
  | union (circle 200 | extrude y 10 | move y -100)
  | blinn-phong (vec3 0.75))
  (def pos [(sin t * 200) (cos+ (t / 2) * 300) 500])
  (set camera (camera/perspective pos [0 0 0] :fov 45))
  ```
  ````
  (default fov 60)
  (def position (typecheck position jlsl/type/vec3))
  (def target (typecheck target jlsl/type/vec3))
  (Camera position (target - position | normalize) y fov)))

(sugar (defn camera/ray
  ````
  Returns the perspective-adjusted ray from this camera for
  the current `frag-coord`. You probably don't need to call
  this.
  ````
  [camera]
  (def camera (typecheck camera Camera))

  (gl/let [camera camera
           z-axis camera.direction
           x-axis (cross z-axis camera.up | normalize)
           y-axis (cross x-axis z-axis)]
    (Ray camera.position (mat3 x-axis y-axis z-axis * perspective-vector camera.fov)))))

(sugar (defnamed camera/pan [camera angle :?up]
  ````
  Rotate the camera left and right.

  ```example
  (morph (ball 50) (box 50) 2
  | union (circle 200 | extrude y 10 | move y -100)
  | blinn-phong (vec3 0.75))
  (set camera (camera/perspective [0 100 600] [0 0 0] :fov 45
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
  | blinn-phong (vec3 0.75))
  (set camera (camera/perspective [0 100 600] [0 0 0] :fov 45
  | camera/roll pi/4
  | camera/pan (sin t * 0.2)
  # | camera/roll -pi/4
  ))
  ```

  ```example
  (morph (ball 50) (box 50) 2
  | union (circle 200 | extrude y 10 | move y -100)
  | blinn-phong (vec3 0.75))
  (set camera (camera/perspective [0 100 600] [0 0 0] :fov 45
  | camera/roll pi/4
  | camera/pan (sin t * 0.2) :up y
  # | camera/roll -pi/4
  ))
  ```
  ````
  (def camera (typecheck camera Camera))
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
  | blinn-phong (vec3 0.75))
  (set camera (camera/perspective [0 100 600] [0 0 0] :fov 45
  | camera/tilt (sin t * 0.2)))
  ```

  As with `pan`, you can supply an absolute `:up` vector to use
  instead of the camera's current roll.

  ````
  (def camera (typecheck camera Camera))
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
  | blinn-phong (vec3 0.75))
  (set camera (camera/perspective [0 100 600] [0 0 0] :fov 45
  | camera/roll (sin t * 0.2)))
  ```
  ````
  [camera angle]
  (def camera (typecheck camera Camera))
  (def angle (typecheck angle jlsl/type/float))
  (gl/do "roll"
    (var camera camera)
    (set camera.up (rotate camera.up camera.direction angle))
    camera)))

(sugar (defn camera/zoom
  ````
  Zoom the camera by changing its field of view.

  ```example
  (morph (ball 50) (box 50) 2
  | union (circle 200 | extrude y 10 | move y -100)
  | blinn-phong (vec3 0.75))
  (set camera (camera/perspective [0 100 600] [0 0 0] :fov 45
  | camera/zoom (sin t * 0.2 + 1)))
  ```
  ````
  [camera amount]
  (def camera (typecheck camera Camera))
  (def amount (typecheck amount jlsl/type/float))
  (gl/do "zoom"
    (var camera camera)
    (set camera.fov (camera.fov / amount))
    camera)))

(sugar (defn camera/dolly
  ````
  Move the camera forward or backward.

  ```example
  (morph (ball 50) (box 50) 2
  | union (circle 200 | extrude y 10 | move y -100)
  | blinn-phong (vec3 0.75))
  (set camera (camera/perspective [0 100 600] [0 0 0] :fov 45
  | camera/dolly (sin t * 100)))
  ```

  ```example
  (morph (ball 50) (box 50) 2
  | union
    (circle 200 | extrude y 10 | move y -100)
    (box [100 200 50] | tile [300 0 300] :limit 4 | move [0 0 -1000])
  | blinn-phong (vec3 0.75))

  # hitchcock zoom
  (set camera (camera/perspective [0 100 600] [0 0 0] :fov 45
  | camera/dolly (sin+ t * -500)
  | camera/zoom (sin+ t + 1)
  ))
  ```
  ````
  [camera amount]
  (def camera (typecheck camera Camera))
  (def amount (typecheck amount jlsl/type/float))
  (gl/do "dolly"
    (var camera camera)
    (set camera.position (camera.direction * amount + camera.position))
    camera)))
