(use ./import)

(thunk ~(var subject
```
A variable that determines what Bauble will render.

You can set this variable explicitly to change your focus, or use
the `view` macro to change your focus. If you don't set a subject,
Bauble will render the last shape in your script.
```
  nil))

(defmacro view
  "A shorthand for `(set subject _)` that fits nicely into pipe notation, e.g. `(ball 50 | view)`."
  [subject]
  ~(set subject ,subject))

(thunk ~(var aa-grid-size ```
The size of the grid used to sample a single pixel. The total samples per pixel will
be the square of this number. The default value is 1 (no anti-aliasing).
```
  nil))

(thunk ~(var default-2d-color ```
A variable that determines the default color to use when rendering a 2D shape with no color field.

Default is `isolines`.
```
  isolines))

(thunk ~(var default-3d-color ```
A variable that determines the default color to use when rendering a 3D shape with no color field.

Default is `(mix normal+ [1 1 1] (fresnel 5))`.
```
  (mix normal+ [1 1 1] (fresnel 5))))

(thunk ~(var background-color ````
A variable that determines the background color of the canvas.

Default is `graydient`. This can be a vec3 or a vec4:

```example
(ball 100)
(set background-color transparent)
```
````
  graydient))

(thunk ~(var camera
```
An expression for a `ray` that determines the position and direction of the camera.
```
  nil))
