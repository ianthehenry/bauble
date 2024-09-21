(use ../../jlsl)
(use ./types)

# available in distance or color fields:

(jlsl/defdyn viewport :vec4 ```
You don't have to think about this value unless you're implementing a custom `main` function,
which you probably aren't doing.

This represents the portion of the canvas currently being rendered. The `xy` components are the start
(bottom left) and the `zw` coordinates are the size.

Normally this will be equal to `[[0 0] resolution]`, but when rendering quad-view or a chunked render,
it may have a different origin or resolution.

You can use `(gl_FragCoord.xy - viewport.xy)` in order to get the logical fragment position (the value
exposed to a typical shader as `Frag-Coord`).
```)

(jlsl/defdyn resolution :vec2 ```
The size, in physical pixels, of the canvas being rendered. In quad view, this
will be smaller than the physical size of the canvas.
```)
(jlsl/defdyn Frag-Coord :vec2 ```
The center of the current pixel being rendered. Pixel centers are at `[0.5 0.5]`,
so with no anti-aliasing this will have values like `[0.5 0.5]`, `[1.5 0.5]`, etc.
If you're using multisampled antialiasing, this will have off-centered values
like `[0.3333 0.3333]`.
```)
(jlsl/defdyn frag-coord :vec2 ```
The logical position of the current fragment being rendered, in the approximate
range `-0.5` to `0.5`, with `[0 0]` as the center of the screen. Note though that
we always shade pixel centers, so we never actual render `-0.5` or `0.5`, just
nearby subpixel approximations depending on the antialiasing level.

This is equal to `(Frag-Coord - (resolution * 0.5) / max resolution)`.
```)

(jlsl/defdyn t :float "The current time in seconds.")
(jlsl/defdyn ray Ray ```
The current ray being used to march and shade the current fragment. This always represents
the ray from the camera, even when raymarching for shadow casting.

A ray has two components: an `origin` and a `dir`ection. `origin` is a point in the 
global coordinate space, and you can intuitively think of it as "the location of the camera"
when you're using the default perspective camera (orthographic cameras shoot rays from different
origins).

The direction is always normalized.
```)
(jlsl/defdyn depth :float ```
The distance that the current ray has marched, equal to `(distance ray-origin P)`. Not defined in 2D.
```)

(jlsl/defdyn p :vec3 ```
The local point in 3D space. This is the position of the current ray, with
any transformations applied to it.
```)
(jlsl/defdyn P :vec3 ```
The global point in 3D space. This is the position of the current ray before any
transformations are applied to it.
```)
(jlsl/defdyn q :vec2 ```
The local point in 2D space. This is the position being shaded, with any
transformations applied.
```)
(jlsl/defdyn Q :vec2 ```
The global point in 2D space.
```)

# only available in color fields:

(jlsl/defdyn normal :vec3 ```
(Color only!) A normalized vector that approximates the 3D distance field
gradient at `P` (in other words, the surface normal for shading).
```)
(jlsl/defdyn gradient :vec2 ```
(Color only!) An approximation of the 2D distance field gradient at `Q`.
```)
(jlsl/defdyn dist :float ```
(Color only!) The value of the global distance field at `P`. In 3D, this
should be a very small positive number, assuming the ray was able to
converge correctly. In 2D, this gives a more useful value.
```)
