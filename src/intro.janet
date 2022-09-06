# Hello, and welcome to an extremely
# early and unfinished demo!

# Bauble is a playground for creating
# and rendering 3D shapes using signed
# distance functions. Like this one:

(torus :z 60 30
| twist :y 0.07
| rotate-pi :y t :z 0.05
| move :x 50
| mirror :r 10 :x
| rotate-tau :y (/ t 20)
| fresnel
| slow 0.25)

# Drag the viewport around with your
# mouse, and scroll to move the camera
# in and out.

# This text field is a Janet program
# that is re-evaluated every time you
# make a change. This program "returns"
# whatever the final expression is --
# in this case, that animated bauble up
# there. Uncomment the next line to
# return something else:

# (spoon :r 15 (torus :z 50 25) (move :y 50 | rotate :y tau/4) | fresnel 1)

# Janet is a fully-featured language, so
# you can define variables, functions,
# macros, loops -- anything your heart
# desires. Here's a more complex
# (minified!) example -- to uncomment
# it, select the whole paragraph and
# press "cmd-/" or "ctrl-/":

# (defn sphere-sequence [radius args]
#   (var r radius) (var pos [0 0 0]) (var period 0) (def spheres @[])
#   (each offset args
#     (+= pos offset)
#     (array/push spheres (sphere r | move (+ pos (* 2 (sin (+ t period))))))
#     (*= r 0.75) (+= period 1324.132))
#   spheres)
# (def body (union :r 22 (sphere 80 | scale (+ 1 (* 0.01 (sin (* 5 t)))))
#   ;(sphere-sequence 71 [[-21 43 1] [4 36 4] [6 27 4] [8 25 7] [0 22 8] [-7 12 7] [-12 12 7] [4 10 -3] [13 15 14]])
#   ;(sphere-sequence 40 [[67 -45 1] [37 2 21] [18 17 24] [4 -6 34] [-8 -12 28] [-22 -11 -1]])
#   ;(sphere-sequence 40 [[-67 -37 1] [-37 25 21] [-34 -17 14] [-4 0 34] [-18 8 16] [-17 22 -1]])))
# (defn triple [shape] (shape | fork :r 2 (move :x -20) (move :x 20) (move :y 33) | move :z 75))
# (def bottom-eyelid (intersect :r 2 (onion 1 (sphere 15)) (half-space :y 0) | rotate :x 2.15))
# (def top-eyelid (intersect :r 2 (onion 1 (sphere 15)) (half-space :y 0) | rotate :x (- 0.75 (cos+ (* 2 t) | ss 0.99 1 | * 2))))
# (def eye (sphere 15 | rotate :y (- (ss (- (sin t) 0.95) 0.03) (ss (- (sin (* 0.87 (+ pi t))) 0.95) 0.03))
#   | shade (vec3 (+ 0.1 (step 0.81 (step p.z 12)))) :gloss 15 :shine 1 | fresnel :exponent 1 | fresnel))
# (def skin (shade [0 1 1] :ambient (mix 0.2 0.1 occlusion) :gloss 10 | fresnel :exponent 0.5 0.05 [0 1 0] | fresnel))
# (union :r 5 body (triple (union :r 2 top-eyelid bottom-eyelid)) | resurface skin | union (triple eye) | move :y -40)

# You can also edit values with your
# mouse. Uncomment the next block of
# code, then ctrl-click and drag the
# value 0.00 left to right.

# (def r 0.00)
# (-> (box 80)
#   (rotate-pi :y r :z (* r 0.7) :x (* 0.5 r))
#   (symmetry))

# You can also hold down cmd-shift to
# move your mouse without clicking
# anywhere to edit the value under the
# text cursor. This is a workaround for
# Firefox on macOS, which due to a
# long-standing bug cannot report
# ctrl-click events correctly.
# https://bugzilla.mozilla.org/show_bug.cgi?id=1504210

# When editing values with your mouse,
# Bauble will increment the smallest
# digit of the number, so you can
# increase the precision by adding
# zeroes to the end. In other words,
# editing a value like 3.0 will
# increment by 0.1, but editing 3.000
# will increment by 0.001.

# There will be keyboard shortcuts for
# these things eventually but I haven't
# implemented them yet.

#### Surfacing ####

# So far everything has been weird
# shades of pastel, which is a mapping
# of the XYZ normal vector into RGB
# space. That's the default surface for
# new shapes, but you can apply other
# surfaces:

# (union
#   (shade (box 50 :r 10)
#     (hsv 0.00 1 1)
#     :gloss 4
#     :shine 0.5
#     :ambient 0.2)
#   (shade (ground -50)
#     [0.9 0.9 0.9]))

# (shade) is an alias for (blinn-phong),
# a simple material shader. Try tweaking
# the parameters to see how they work,
# and remember that you can use your
# mouse to edit numbers! Also note that
# specular highlights depend on the
# viewing angle, so rotate the viewport
# a little too.

#### Lisp heresy ####

# So far our examples have mostly stuck
# to "vanilla" Janet, which, of course,
# has a lot of parentheses. But Bauble
# provides a helpful macro that you can
# use to invoke functions with a little
# less typing. Let's take a look,
# starting without any helpers:

# (shade [1 0.1 0.1] (rotate :y pi/4 (box 50 :r 5)))

# First of all, the Bauble DSL is very
# forgiving about named and positional
# argument order. So that's actually the
# same -- modulo color -- as:

# (shade (rotate (box :r 5 50) :y pi/4) [0.1 1 0.1])

# Janet provides a useful threading
# macro that we can use to write this
# as a subject and then a series of
# transformations, so that the
# expression is not as nested:

# (-> (box 50 :r 5) (rotate :y pi/4) (shade [0.1 0.1 1]))

# Which is very useful. Bauble lets you
# go a little bit further:

# (box 50 :r 5 | rotate :y pi/4 | shade [1 1 0.1])

# At first this might not look like Lisp
# at all, but it's a pretty simple macro
# that has the same effect as the (->)
# threading macro -- but it's a lot
# easier to type out.

#### Expressions ####

# Alright, now we're getting to the good stuff.

# (cone :y 100 100)

# We made a cone. Boring. Let's make it
# less boring:

# (cone :y 100 (+ 100 (* 10 (cos (/ p.x 5)))))

# Whoa, okay; there's a lot to unpack
# there.

# First of all: p.x is more lisp heresy.
# This is equivalent to writing the
# more verbose:

# (cone :y 100 (+ 100 (* 10 (cos (/ ~(. ,p :x) 5)))))

# But what is p?

# p is a magic variable that represents
# the current point in space. It's a
# symbolic expression, and the /
# function -- and most math
# functions -- are overloaded to work
# on symbolic expressions. (+ 1 2)
# produces 3, but (+ 1 p.x) produces
# the Janet tuple ~(+ 1 (. ,p :x)), and
# Bauble knows how to compiles that
# into a string of GLSL code that will
# execute on the GPU.

# Symbolic expressions nest, so when we
# take the cosine of ~(/ (. ,p :x) 5),
# we wind up with the symbolic
# expression ~(cos (/ (. ,p :x) 5)).
# And so on, until we finally have an
# expression for the cone's height.

# Oh, hey! S-expressions! That's what
# that stands for. Look at us: we're
# lisp programmers now.

# p isn't the only magic variable. You
# also have t, which is the current
# time in seconds:

# (cone :y 100 (+ 100 (* 10 (cos (+ (* 5 t) (/ p.x 5))))))

# There's also P -- while p is the point
# in space local to the current shape
# (so translated, rotated, etc), P is
# the global position of the ray, which
# is mostly useful for calculating
# reflections or specular highlights
# during surfacing. But here's a
# contrived example just to show you
# the difference:

# (union
#   (sphere 50 | shade [1 (abs (/ p.x 50)) 0] | move :y 50)
#   (sphere 50 | shade [1 (abs (/ P.x 50)) 0] | move :y -50)
# | move :x (* 50 (sin t)))

# There's also camera, which is the
# position of the camera in world
# coordinates. Once again, mostly
# useful for surfacing.

# (box 50 :r 10
# | tile [150 0 150]
# | shade [1 (/ (distance camera P) 1000) 0])

# When writing a color expression, you
# have access to a couple other magic
# variables: "normal" and "occlusion."

# normal is an approximation of the
# surface normal at the point that
# you're shading:

# (box 50 :r 10 | rotate :x t | shade [1 (clamp normal.y 0 1) 0])

# occlusion is an approximation of how
# much stuff is near the point that
# you're shading. 1 means there is
# nothing around you, while 0 means
# there is another shape right next to
# you. It's commonly used to modulate
# light:

# (defn shade-ao [shape color]
#   (shade shape color :ambient (mix 0.1 0.2 occlusion)))
# (box 50 :r 10 | spoon (rotate :x tau/8 :y tau/4 :x tau/3 | scale 0.75 | move :y 70)
# | shade-ao [0 1 1]
# | union (half-space :-y -50 | shade-ao [1 1 1]))

# Try changing (shade-ao) back to
# (shade). Without ambient occlusion,
# the areas in shadow appear completely
# flat. With ambient occlusion, you get
# a sense of depth.

# Just to review, the only magic
# variables are:

# - t: time in seconds
# - p: point in local coordinate system
# - P: point in global coordinate
#   system
# - camera: camera position in global
#   coordinate system
# - normal: (color only) surface normal
# - occlusion: (color only) an
#   approximation of the concavity of
#   the distance field near this point

#### Spatial artifacts ####

# Let's return to our cone.

# (cone :y 100 (+ 100 (* 10 (cos (/ p.x 5)))))

# Actually, let's really lean into the
# pipe syntax for a second:

# (cone :y 100 (p.x | / 5 | cos | * 10 | + 100))

# What do you think? Neat? Horrifying? I
# kinda like it, but it's definitely an
# acquired taste.

# Anyway, drag the camera around, and
# direct your attention to the tip of
# the cone. See how it seems to flicker
# at certain angles?

# That's because we no longer have an
# accurate distance field to raymarch.
# Because the shape depends on the
# position of the ray, the distance
# field is only correct when the ray is
# very close to the distorted cone.

# This means that sometimes the
# raymarcher will overshoot the surface
# of the cone -- landing inside the
# cone or, in this case, passing
# through it entirely.

# Here's a more extreme example:

# (torus :x 100 25 | rotate :y (* p.y 0.020))

# We're rotating this torus by an angle
# that varies over the y axis, which
# gives us a twisting effect. But it
# looks really bad! Especially if you
# view it from the top down.

# We can mitigate this sort of error by
# slowing down the raymarcher as it
# approaches the torus. Since the
# distance field is no longer giving us
# accurate values, we can choose to
# advance our rays by only half the
# reported distance value. This is called
# (slow):

# (torus :x 100 25 | rotate :y (* p.y 0.020) | slow 0.50)

# It's aptly named, because it will
# increase the number of raymarching
# steps we have to take, and slow down
# our render.

# Try increasing the twist amount.
# Eventually you will notice that a
# slow coefficient of 0.5 isn't enough,
# and you'll have to reduce it. But
# reducing it too much will start to
# introduce new artifacts, as the
# raymarcher might begin to abort after
# taking the maximum steps per fragment
# (currently hardcoded to 256) before
# it finds the torus. So there's sort
# of a limit to how distorted you can
# make space. At least for now.

# Let me draw your attention to the
# upper-right hand corner. See those
# buttons? There's a camera, a magnet,
# and a... I dunno; it's hard to come
# up with icons for these things.

# These views are useful for debugging
# spatial distortions. The first one
# shows the number of steps, as a
# gradient from black to write. The
# darker the pixel, the fewer steps it
# took to find an intersection with the
# shape. Rays that didn't manage to
# converge in 256 steps show up in
# magenta.

# The second debug view shows the value
# of the distance field as
# (distance / minimum_hit_distance).
# Blue values are good: that means the
# ray stopped a small positive distance
# from the surface. Magenta values mean
# the ray overshot, and landed inside
# the shape. You can use these views to
# help you select slow coefficients.

# Also note that slowing down the
# raymarcher has other effects as well.
# Functions that rely on the distance
# field, like boolean operations, will
# not be as accurate. For example, look
# at this snowman:

# (sphere 50 | move :y -10 | union :r 10 (sphere 40 | move :y 45))

# If we slow down space around one of
# the spheres, the smooth union will no
# longer be symmetric:

# (sphere 50 | move :y -10 | slow 0.5 | union :r 10 (sphere 40 | move :y 45))

# But if we slow down space around the
# whole shape, we won't have that
# problem:

# (sphere 50 | move :y -10 | union :r 10 (sphere 40 | move :y 45) | slow 0.5)

# Lastly, slowing down space will cause
# soft shadows to become too soft, for
# complicated reasons that I don't want
# to explain right now because this is
# so long already.

# (union
#   (sphere 50 | move :z -70 | slow 0.25)
#   (sphere 50 | move :z 70)
#   (ground -50 | shade [1 1 1]))

#### Surfacing expressions ####

# You can change the color of a shape
# using the (map-color) helper. Let's
# go back to our pastel sphere:

# (sphere 50)

# We can tint it red:

# (sphere 50 | map-color (fn [col] (+ [1 0 0] col)))

# We can deepen the color intensity:

# (sphere 50 | map-color (fn [col] (pow col 2)))

# We can replace the color with
# something else altogether:

# (sphere 50 | map-color (fn [col] [1 0 1]))

# Bauble also has a (color) macro. This
# is just like map-color, except it
# automatically wraps our expression in
# a function that takes its argument
# as "c". So these two lines are
# exactly equivalent:

# (fork (sphere 50) (move :y -25) (move :y 25) | color (* c (sqrt occlusion)))
# (fork (sphere 50) (move :y -25) (move :y 25) | map-color (fn [c] (* c (sqrt occlusion))))

# (Note that (color) is a *macro*, not a
# function, so unlike most of Bauble,
# the argument order matters here.)

# For an example using some of the
# fancier magic variables to good
# effect, consider this expression:

# (box 50 :r 10
# | color (let [view-dir (normalize (- camera P))]
#     (+ c (pow (- 1.0 (dot normal view-dir)) 5))))

# That adds a little bit of
# (simulated) fresnel reflectivity to a
# surface. Move the camera around a bit
# to see what it does. Note that Bauble
# doesn't actually support reflection
# yet, so it just tints the edges, but
# it still looks pretty nice.

# There is actually a built-in that does
# exactly the same thing as that
# complicated expression above, but with a
# few more knobs to tweak:

# (fresnel (box 50 :r 10) [1 1 0] 0.5 :exponent 5)

# All of the arguments are optional, so
# you can quickly apply it to a shape
# and add a little depth. You can use a
# lower exponent and a warmer color to
# evoke a subsurface scattering
# effect.

# (fresnel (sphere 50 | shade [1 1 1]) [1 0.6 0.5] 0.25 :exponent 0.5)

#### Surfacing with boolean operations ####

# When you combine shapes together, you
# also combine their surfaces. For
# example, here are a couple shapes:

# (def green-box (shade [0 1 0] (box 50 :r 5) :gloss 12 :shine 1))
# (def red-sphere (shade [1 0 0] (sphere 60)))

# Now uncomment each of these one at a
# time to see how the colors interact:

# (union green-box red-sphere)
# (intersect green-box red-sphere)
# (subtract green-box red-sphere)

# And now let's try it with smooth
# transitions:

# (union :r 5 green-box red-sphere)
# (intersect :r 5 green-box red-sphere)
# (subtract :r 5 green-box red-sphere)

# That's interesting, but sometimes you
# might not want to see that yellow
# bleeding through. Sometimes you want
# a smooth shape transition, but a sharp
# color transition. And you can have it:

# (resurface
#   (subtract :r 5 green-box red-sphere)
#   (subtract green-box red-sphere))

# (resurface) works to transplant the
# color field from any shape to
# another shape. In that case the shapes
# were very similar, but they don't have
# to be.

# (resurface
#   green-box
#   (union green-box red-sphere))

# The way this works is that the
# raymarcher uses the signed distance
# field from the first shape to
# determine the geometry, but when it
# hits the surface it uses the second
# shape to determine the color.

# This is a useful technique for
# "painting" complex colors onto shapes,
# but you can also use (resurface) to
# save a material to apply to multiple
# shapes. Instead of this:

# (shade [1 1 0] (sphere 50))

# You can write:

# (def yellow (shade [1 1 0]))
# (resurface (sphere 50) yellow)

# The way this works is that (shade) and
# other material primitives, when not
# given a shape to act on, default to
# the entirety of ℝ³ -- the shape that
# is a distance 0 away from every point.
# So a "material" is still a pair of
# distance and color functions, but the
# distance function isn't really useful.

#### Overloading ####

# I already mentioned that many of the
# built-in math functions are
# overloaded to work on symbolic
# expressions. They're also overloaded
# to work on vectors, in ways that
# mirror GLSL functions.

# For example, you can write (+ [1 2 3]
# [4 5 6]) to add the elements of two
# tuples together. That would normally
# be a type error in Janet, but inside
# Bauble, numeric functions like + or
# sin or pow have all been overloaded
# to match GLSL's semantics.

# In addition many -- but not all --
# GLSL functions have been ported to
# Janet, and when you call them with
# constant arguments they will execute
# on the CPU. For example,
# (distance [0 0] [1 1]) will give you
# the number 1.41421. But
# (distance [0 0] p.xy) will produce a
# symbolic expression that will
# execute on the GPU.

# One notable exception is length(),
# since that's already a very
# common Janet function that returns
# the length of an array or tuple. As
# such (length [1 2 3]) is 3, but
# (length p) is the symbolic expression
# ~(length ,p). The generic version of
# GLSL's length() is called
# (vec-length).

# If for some reason you want to *force*
# a computation to occur on the GPU,
# you can quote it like this:
# ~(distance [0 0] [1 2]).

# Also note that some functions -- for
# example, the procedural noise
# functions -- always produce symbolic
# expressions, even with constant
# arguments, so they'll always execute
# on the GPU.

#### Helpers ####

# smoothstep is a cubic interpolation
# function with native GPU support that
# shows up all the time in procedural
# art.

# Bauble has a helper that makes it a
# little more convenient to use:

# (ss x) = (smoothstep 0 1 x)
# (ss x hi) = (smoothstep 0 hi x)
# (ss x lo hi) = (smoothstep lo hi x)
# (ss x lo hi to-hi) = (* (smoothstep lo hi x) to-hi)
# (ss x lo hi to-lo to-hi) = (+ to-lo (* (smoothstep lo hi x) (- to-hi to-lo)))

# Also, there are many functions that
# return values in the range -1 to 1,
# when often you want values in the
# range 0 to 1 (to use as a color, for
# example). There is a helper function
# called remap+ defined like this:

# (defn remap+ [x]
#   (* 0.5 (+ x 1)))

# For example, you can use it to
# re-create the default normal
# coloring, but with lighting and
# shadows:

# (sphere 50)
# (sphere 50 | shade (remap+ normal))

# There are also "+" versions of a few
# functions that you might frequently
# want to remap into the 0 to 1 range:

# (sin+ x)    = (remap+ (sin x))
# (cos+ x)    = (remap+ (cos x))
# (perlin+ x) = (remap+ (perlin x))

#### Procedural noise ####

# Bauble currently has a single noise
# function:

# (box 100 :r 10 | color [0 (perlin+ (* 0.1 p)) 0])

# (perlin) can take a vec2, vec3, or
# vec4. Each one is more expensive to
# compute than the last, so only use
# what you need.

# (box 100 :r 10 | color [0 (perlin+ (* 0.1 p.xz)) 0])

# The vec4 version is useful if you want
# a 3D noise signal that varies over
# time without imparting a sense of
# motion. Compare these two:

# (box 100 :r 10 | color [0 (perlin+ (+ (* 0.1 p) t)) 0])
# (box 100 :r 10 | color [0 (perlin+ (vec4 (* 0.1 p) t)) 0])

# You'll usually use noise to color your
# shapes, but you can also deform space
# with noise. This is quite a bit more
# expensive than shading, so your GPU
# might not like it if you do it too
# much. But you can use it to produce
# some very cool effects:

# (sphere (+ 50 (p | * 0.05 | + t | perlin+ | * (* t 4 | sin+ | * 25)))
# | slow 0.9)

# You can use noise to compute complex
# procedural textures:

# (def spots (+ 0.15 (perlin+ (* 0.103 p))))
# (def outline (step (abs (- 0.5 spots)) 0.016))
# (def brown (hsv 0.01 0.63 0.5))
# (def tan   (hsv 0.07 0.63 0.9))
# (sphere 100 | shade (mix brown tan (round spots) | * (max outline 0.05)))

# Just beautiful. Let's hold on to that
# one; I have a feeling we're going to
# do great things together:

# (def leppard (shade (mix brown tan (round spots) | * (max outline 0.05))))
# (def eye (sphere 5 | shade [1 1 1] | union (sphere 2 | move :z 4 | shade [0.1 0.1 0.1])))
# (line [20 0 32] [50 -50 50] 5
# | mirror :x :z
# | union :r 10
#   (line [0 9 -31] [0 -14 -90] 5)
#   (box [33 20 41] :r 10)
#   (sphere 20 | move :z 51 :y 24)
# | resurface leppard
# | union
#   (eye | rotate :x 0.63 :y -0.47 | move [7 34 67])
#   (eye | scale 0.95 | rotate :x -0.09 :y 0.52 | move [-7 33 67]))

# We will never speak of this again.

#### Light and shadow ####

# Currently there's no way to customize
# anything about the lighting used by
# the built-in Blinn-Phong shader. But
# it's coming soon!

#### Getting Help ####

# Uhhh okay look you have just read
# literally all of the documentation.

# Sorry about that.

# You can print values for debugging
# with (print "string") or
# (pp expression). Error messages are
# extremely bad right now, so don't
# make any mistakes. If you write an
# infinite loop it *will* just hang the
# browser tab and you will have no way
# to get out of it except to refresh
# the page.

# Your changes will automatically save,
# but if you want to restore this
# initial tutorial, just empty out this
# text field and refresh the page.

# For more info... maybe check out the
# source?

# https://github.com/ianthehenry/bauble/blob/master/src/dsl.janet
# https://github.com/ianthehenry/bauble/blob/master/src/glslisp/src/builtins.janet
# https://github.com/ianthehenry/bauble/blob/master/src/helpers.janet

# Or try studying these examples:

# (union (box 50) (sphere 70))
# (union :r 10 (box 50) (cone :z 40 100))
# (sphere 100 | onion 5 | intersect (half-space :-z))
# (sphere 100 | onion 5 | intersect :r 5 (half-space :-z 60))
# (morph 0.5 (box 50) (sphere 50))
# (box 50 | subtract (cylinder :z 30 100))
# (subtract :r 30 (box 50 | rotate :y tau/8 :z tau/8) (sphere 50 | move :x 50))
# (cone :x 50 100)
# (cone :x 50 100 | reflect :x)
# (cone :x 50 100 | rotate :y pi/4 | mirror :x :z)
# (union :r 50 (line [-50 0 0] [50 0 0] 10) (sphere 50 | move :x 100 | mirror :x))
# (sphere 50 | tile [100 100 100] :limit [3 10 2])
# (cone :-z 50 100 :r 10)
# (cone :-z 50 100 | offset 10)
# (box 40 | scale 1.5)
# (box 40 | scale :x 0.5)
# (box 40 | scale [1 2 0.5])
# (torus :y 100 25)
# (box 50 | twist :y 0.010 | slow 0.5)
# (box [50 10 50] | bend :x :y 0.010 | slow 0.5)
# (box 50 | swirl :y 0.040 | slow 0.5)

# Comments? Questions? Requests?
# https://github.com/ianthehenry/bauble/discussions

# Found a bug? Let me know!
# https://github.com/ianthehenry/bauble/issues
