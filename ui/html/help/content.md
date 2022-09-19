# Shapes

A quick list of all shape primitives:

```
(defn grid [$ x z]
  (move $ ([x 0 z] * 200)))
(union
  (sphere 50 | grid 0 0)
  (box 50 | grid 1 0)
  (cone :y 50 100 | move :y -50 | grid 0 1)
  (torus :z 35 15 | grid -1 0)
  (ellipsoid [25 50 75] | grid 0 -1)
  (cylinder :y 50 50 | grid 1 -1)
  (line [-40 -40 40] [40 40 -40] 10 | grid -1 -1)
  # (ground -50 | shade (vec3 0.25))
| scale 0.5)
```

## `sphere`

`sphere` is the simplest of all primitives. It has only one parameter: the radius.

```
(union
  (sphere (4 + sin t * 10) | move :x 50)
  (sphere (5 + sin (p.y / 4 + t) * 8) | move :x -50 | slow 0.6))
```

## `box`

`box` is slightly more complicated. It can take a single dimension or a vector of three elements.

The dimensions you supply to `box` are twice the size that the box appears. Think of them like a radius: a box of "width 50" will range from `p.x = -50` to `p.x = 50`.

There is one optional argument: `:r`, which rounds the corners.

```
(union
  (box 50 :r 10)
  (box [20 50 100] | move :x -150)
  (box 50 | rotate :y (sin (p.y / 5) * 0.2) | slow 0.7 | move :x 150))
```

## `cylinder`
## `ellipsoid`
## `line`
## `torus`
## `half-space`
## `cone`
## `ground`

A weird hacky thing that I might get rid of. Not really a valid shape. A "half-space you can see through."

# Operations on shape

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

## `offset`
## `onion`
## `distort`
## `mirror`
## `reflect`
## `mirror-plane`
## `mirror-space`
## `symmetry`

It's hard to describe this one. It mirrors space across every axis and also flips it across every across. You end up with only a tiny sliver of useful space that you can put a shape in, so it's only really suitable for abstract things.

```
(def r (+ 50 (sin+ t * 50)))
(symmetry (cone :x 50 50 | move [(r - 50 * 0.5) r (3 * r - 100)]))
```

## `flip`
## `twist`
## `swirl`
## `bend`
## `map-distance`

# Operations on shape and color

The boolean operations `union`, `intersect`, and `subtract` can all take an optional `:r` value, but note that this will make color calculations slower: *all* surfaces will be evaluated, even those that do not contribute at all to the final result. When used without `:r`, or with an `:r` value of `0` (as evaluated on the CPU -- any symbolic expression will cause Bauble to take the slow branch), only the one relevant surface will be evaluated.

Because of this, you should prefer to apply surfaces *after* smooth boolean operations, unless of course you are relying on the surface blending. Even if it's the same surface, Bauble will still evaluate it multiple times! (This is a fixable deficiency in Bauble but it is the way that it is right now.)

## `union`
## `intersect`
## `subtract`
## `morph`

Produces incorrect distance fields when the amount is outside of the range `[0, 1]`.

```
(union
  (morph (cos+ t) (sphere 50) (box 50) | move :x -75)
  (morph (sin t * 2) (sphere 50) (box 50) | move :x 75 | slow 0.5))
```

# Repetition

## `tile`
## `radial`

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
- `remap+`
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
- `perlin` functions (and `perlin+`)

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
