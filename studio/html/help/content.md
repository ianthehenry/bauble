# Shapes

A quick list of all shape primitives:

```
(defn grid [$ x z]
  (move $ ([x 0 z] * 150)))
(union
  (sphere 50 | grid 0 0)
  (box 50 | grid 1 0)
  (cone :y 50 100 | move :y -50 | grid 0 1)
  (torus :z 35 15 | grid -1 0)
  (ellipsoid [25 50 75] | grid 0 -1)
  (cylinder :y 50 50 | grid 1 -1)
  (line [-40 -40 40] [40 40 -40] 10 | grid -1 -1)
  (ground -50 | shade white)
| scale 0.75
| move :x -30)
```

## `sphere`

`sphere` is the simplest of all primitives. It has only one parameter: the radius.

```
(union
  (sphere (8 + sin t * 10) | move :x 100)
  (sphere (6 + sin (p.y / 4 + t) * 10) | move :x -100 | slow 0.6))
```

## `box`

`box` can take a single `float` dimension or a `vec3`.

The dimensions you supply to `box` are twice the size that the box appears. Think of them like a radius: a box of "width 50" will range from `p.x = -50` to `p.x = 50`.

`:r` to round the edges.

```
(union
  (box 50 :r 10)
  (box [20 50 100] | move :x -150)
  (box 50 | rotate :y (sin (p.y / 5) * 0.2) | slow 0.7 | move :x 150))
```

## `cylinder`

The second argument is twice the height of the cylinder, which is centered at the origin.

`:r` to round the edges.

```
(union
  (cylinder :y 50 100 | move :x 60)
  (cylinder :r 10 :y 50 100 | move :x -60))
```

## `ellipsoid`

Not an exact distance field! This is [an approximation](https://iquilezles.org/articles/ellipsoids/) that produces better results than scaling a sphere, but that is still only an approximation.

```
(ellipsoid [100 75 50])
```

## `line`

The two `vec3` arguments are the start and end points. An optional `float` argument is the radius of the line (default `0`).

```
(line 10 [-50 -50 50] [50 50 -50])
```

## `torus`

```
(torus :z 100 25)
```

## `half-space`

This one is very hard to show with images, because it just looks like a flat-colored plane of infinite extent. Instead, it's easier to understand when used as an input to a boolean operation:

```
(intersect
  (half-space :-y 10)
  (sphere 50))
```

The first argument is a signed axis, and the second optional argument is the position along that axis.

## `cone`

Unlike most other shapes, which are centered at the origin, the cone rests at the origin, and the height you pass it is the cone's actual height, not one half of its height. It's still the extent of the cone in a single axis, like other shapes, but unlike most shapes the cone does not extend in both directions.

`:r` to round the edges.

```
(union
  (cone :y 50 100 | move :x -50)
  (cone :r 10 :y 50 100 | move :x 50))
```

## `ground`

`ground` should not be used in actual art, but it's useful for debugging. It's kind of a "`(half-space :-y)` that you can see through," so that you can add a ground to see shadows, and still move the camera around to view the underside of your shape.

It is not an actual distance field (the distance depends on the position of the camera), so it will not work well with morphs, boolean operations, or ambient occlusion. It's really just for quickly looking at shadows.

```
(union
  (ground -50 | shade [1 1 1])
  (box 50 :r 5))
```

# Spatial operations

## `move`

Can take a `vec3` elements or named `:x`, `:y`, and `:z` float arguments. You can mix and match these forms, and the computed sum will be used.

```
(union
  (move :y (sin t * 50) (sphere 50))
  (move [-100 0 0] (sphere 50))
  (move [50 50 0] :x 25 :y -50 :x 25 (sphere 50)))
```

`move` is also a great way to distort shapes.

```
(union
  (move :y (sin (atan2 p.x p.z * 5) * (ss (length p.xz) 0 50) * 10) (cylinder :y 50 10) | move :x -75)
  (move :y (p.xz / 15 | pow 2 | sum) (box [50 10 50]) | move :x 50)
| slow 0.5)
```

## `rotate`

Takes named `:x`, `:y`, and `:z` arguments; the final rotation is the combination of each rotation in order. There is no vector form because the order of the rotations is significant: `(rotate :x 1 :y 1)` is not the same as `(rotate :y 1 :y x)`.

You can also pass `:pivot`, with a `vec3` that will form the point around which rotation happens.

You can also optionally supply scale arguments, which will multiply the rotations *following* that scale argument by a fixed amount. Valid scales are `:pi`, `:tau`, or `:deg`. You can change scale mid-rotation, for example by saying `(rotate :deg :x 45 :tau :y 0.125)`. This is equivalent to `(rotate :x tau/8 :y tau/8)`.

```
(union
  (rotate :y t (box 50))
  (rotate :x (sin t) :y (sin t) (box 50) | move :x 150)
  (rotate :y (sin t) :x (sin t) (box 50) | move :x -150))
```

## `scale`

Can take a single `float`, a `vec3`, or named `:x` `:y` `:z` parameters. Like [`move`](#move), you can mix and match these forms and the final product will be used as the scale factor for each axis.

If you scale by different amounts across different axes, the distance field produced will be an underestimate. This means that Bauble can still raymarch it accurately without the use of [`slow`](#slow), but soft shadows and boolean operations will be inaccurate.

```
(union
  (scale (sin+ t + 0.5) (box 50) | move :x -100 :z -50)
  (scale :y (sin+ t + 1) (box 50) | move :x 100 :z -50)
  (scale ([1 1 1] + ([-0.5 1 0] * (cos+ t))) (box 50) | move :z 100))
```

## `offset`

Name will probably change.

Good way to apply texture to a shape, although if your expression is expensive you should use [`bounded-offset`](#bounded-offset) instead.

```
(union
  (box 50 | offset (sin+ t * 10) | move :x -100)
  (box 50 | offset (perlin (p / 15) | pow 2 * 10) | move :x 100))
```

## `onion`

`float` argument is half of the thickness. The shape will be both inset and outset by this amount.

```
(onion 5 (box 50)
| subtract (half-space :y))
```

## `distort`

This allows you to supply an arbitrary expression that will replace `p`. You can use this to deform space in ways that are not possible with any of the built-in operators.

This is by its nature a bizarre operation that can be used to do anything, including implement any of the other spatial distortion operations, so examples are not very informative.

```
(distort (sphere 100)
  [p.x (sin ((p.x - p.z) / 10) * 100) p.z]
| slow 0.5)
```

## `mirror`

Interior distances will have discontinuities when a shape crosses the axis.

You can specify multiple axes to mirror along. The optional `:r` argument makes this a smooth mirror.

```
(union
  (mirror :x :y (box 50 | rotate :y t :z t | move :x 25) | move :z 100)
  (mirror :x :y :r 10 (box 50 | rotate :y t :z t | move :x 25) | move :z -100))
```

## `reflect`

Like mirror, but doesn't create a copy. Can take multiple axes.

```
(union
  (cone :y 50 100 | move :x -50)
  (reflect :y (cone :y 50 100 | move :x 50)))
```

## `mirror-plane`

TODO

## `mirror-space`

TODO

## `symmetry`

It's hard to describe this one. It mirrors space across every axis and also flips it across every axis of rotation. You end up with only a tiny sliver of useful space that you can put a shape in, so it's only really suitable for abstract things.

```
(def r (+ 50 (sin+ t * 50)))
(symmetry (cone :x 50 50 | move [(r - 50 * 0.5) r (3 * r - 100)]))
```

## `flip`

Rotates the shape 90° around a signed axis (so `:x` rotates counter-clockwise, `:-x` rotates clockwise). More efficient than `rotate`.

```
(union
  (cone :y 50 100 | move :x -50)
  (flip :-x (cone :y 50 100 | move :x 50)))
```

## `twist`

`float` argument is the rate of rotation, in radians per unit distance.

This is equivalent to a rotation around an axis that varies with `p.(axis)`, but it might be slightly more efficent.

Does not produce a correct distance field.

```
(twist :y (tau/4 / 50 * sin t)
  (box 50)
| slow 0.5)
```

## `swirl`

`float` argument is the rate of rotation, in radians per unit distance.

This is equivalent to a rotation around an axis that varies with `(length p.(other axes))`, but it might be slightly more efficent.

Does not produce a correct distance field.

```
(swirl :y (tau/4 / 50 * sin t)
  (box 50)
| slow 0.5)
```

## `bend`

This operation is weird and hard to explain and I will probably get rid of it.

Does not produce a correct distance field.

```
(bend :y :z (tau/4 / 200 * sin t)
  (box 50)
| slow 0.5)
```

## `map-distance`

Apply a function to a shape's distance field. Can be used to do weird things. Many operations, like `offset` or `slow`, are simple transformations on the underlying distance field. For example, `offset`:

```
(map-distance (sphere 50) (fn [d]
  (d + (sin (p.x / 2)))))
```

# Hybrid spatial/surface operations

The boolean operations `union`, `intersect`, and `subtract` can all take an optional `:r` value, but note that this will make color calculations slower: *all* surfaces will be evaluated, even those that do not contribute at all to the final result. When used without `:r`, or with an `:r` value of `0` (as evaluated on the CPU -- any symbolic expression will cause Bauble to take the slow branch), only the one relevant surface will be evaluated.

Because of this, you should prefer to apply surfaces *after* smooth boolean operations, unless of course you are relying on the surface blending. Even if it's the same surface, Bauble will still evaluate it multiple times! (This is a fixable deficiency in Bauble but it is the way that it is right now.)

## `union`

Union produces a correct distance field unless shapes overlap, in which case the interior distance field will be discontinuous.

## `intersect`

Does not produce a correct distance field.

## `subtract`

Does not produce a correct distance field.

## `morph`

Produces incorrect distance fields when the amount is outside of the range `[0, 1]`.

```
(union
  (morph (cos+ t) (sphere 50) (box 50) | move :x -75)
  (morph (sin t * 2) (sphere 50) (box 50) | move :x 75 | slow 0.5))
```

# Repetition

## `tile`

Tile divides space into rectangular regions.

```
(tile [100 0 150] (sphere 50))
```

The `vec3` argument determines the size of each tile, with `0` meaning that no repetition takes place in that direction. By default the shape will be repeatedly infinitely in every direction, but the optional argument `:limit` will clamp the repetition.

```
(tile [100 100 100]
  :limit [4 3 2]
  (sphere 50))
```

`:limit` must be a tuple of three positive integers; you cannot write a dynamic expression for `:limit`.

If you provide a function argument instead of a shape, your function will be called with an expression for the current index of the tiling. This allows you to produce different objects at each instance of the tiling. The index will always be a `vec3`, even if you are not repeating in all three directions.

```
(tile [100 0 100]
  (fn [i]
    (sphere 50
    | shade (hsv (i.x / 6) 1 1))))
```

Indexes are integers, so the index of the element at the origin is `[0 0 0]`, the index to the right of that is `[1 0 0]`, etc.

You can also provide a shape *and* a function, in which case the shape will be passed as the first argument to your function. By appropriating a little bit of Janet convention, we call this argument `$`, for `$hape`. This is useful for fitting `tile` into a pipeline:

```
(sphere 50
| tile [100 0 100]
  (fn [$ i]
    (shade $ (hsv (i.z / 6) 1 1))))
```

### Asymmetry

The way `tile` works is that, for each step of the raymarch, it computes the current "slice" of space. Then it evaluates *only* that one slice, and returns the nearest distance.

This means that, if the actual nearest shape is in a *different* tile than the current one, this will produce an invalid distance field.

```
(tile [150 0 150] (fn [i]
  (box 50 :r 5
  | rotate :x t :y (t + hash i)
  | shade [1 1 1])))
```

Here you can see lots of artifacts around the edges of the boxes, where rays overshoot their targets. The solution to this problem, if you want to tile an asymmetric shape, is to first duplicate the shape a small number of times (with `union`) and then to tile that array of shapes with overlap.

A future version of Bauble will have a helper to make this more convenient.

## `radial`

Similar to `tile`, but repeats radially. Requires an axis and a count. Can optionally take a number that will determine how far to outset the shape before repeating it (default `0`).

```
(radial :y 12 (sin+ t * 100)
  (cone :x 50 100))
```

Like `tile` you can supply a mapping function (with a shape) or a producing function (without a shape). The index will be an integer in the range `[0, count)`. As with `tile`, only a fraction of space is considered at once, so asymmetric shapes will produce invalid distance fields.

```
(cone :y 50 100
| radial :y 12 (sin+ t * 100) (fn [$ i]
  (rotate $ :tau :z (i / 12 + (t / 10))
  | move [100 0 0]
  | shade (hsv (i / 12) 1 1))))
```

# Operations on color

## `shade`
## `fresnel`
## `cel?`
## `resurface`
## `map-color`
## `color`

# Meta-operations

## `bound`
## `bounded`
## `bounded-offset`
## `slow`
## `pivot`

# General helpers

- `fork`
- `spoon`
- `remap+` (and `sin+`, `cos+`, `perlin+`)
- `hsv`
- `hsl`
- `rgb`
- `hex-rgb`
- `pi`, `pi/2`, `pi/3`, etc
- `tau`, `tau/2`, `tau/3`, etc
- `deg`
- `tau*`, `pi*`, `tau/`, `pi/`
- `ss`
- `hash` functions
- `perlin`

# GLSL functions ported to Janet

- `sign`
- `clamp`
- `step` (note: argument order reversed)
- `smoothstep`
- `distance`
- `dot`
- `mix`
- `min` (note: overloaded better)
- `max` (note: overloaded better)
- `fract`
- `normalize`
- `sqrt`
- `length`/`vec-length`
- `pow` (note: overloaded better)

# Janet functions ported to GLSL

- `sum`
- `product`
- `atan2` (note: safer than `atan`)

# Functions that are the same

- `sin`
- `cos`
- `abs`
- `round`
- `floor`
- `ceil`
- `atan`
- `mod`

# Color constants

- `(def red      (hsv (/ 0 6)    0.98 1))`
- `(def orange   (hsv (/ 0.25 6) 0.98 1))`
- `(def yellow   (hsv (/ 1 6)    0.98 1))`
- `(def green    (hsv (/ 2 6)    0.98 1))`
- `(def cyan     (hsv (/ 3 6)    0.98 1))`
- `(def sky      (hsv (/ 3.5 6)  0.98 1))`
- `(def blue     (hsv (/ 4 6)    0.98 1))`
- `(def purple   (hsv (/ 4.5 6)  0.98 1))`
- `(def magenta  (hsv (/ 5 6)    0.98 1))`
- `(def hot-pink (hsv (/ 5.5 6)  0.98 1))`
- `(def white      [1    1    1])`
- `(def light-gray [0.75 0.75 0.75])`
- `(def gray       [0.5  0.5  0.5])`
- `(def dark-gray  [0.25 0.25 0.25])`
- `(def black      [0.03 0.03 0.03])`
