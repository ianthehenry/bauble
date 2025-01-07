# Welcome to Bauble! This is a little
# playground for turning parentheses
# into pixels. Here's the source for
# that hot air balloon over there:

(ball [50 100 100]
| union :r 50 (cylinder y 25 50 | move [0 -100 0])
| scale y (ss p.y -100 100 1 0.8)
| color
  (shade r3 (hsv (quantize (p.y / 100 - (t / 5)) 8) 1 1) :g 5
  | move y (sin (t * 1.5) | ss * 100 / 8 * parity)
  | rotate z (parity | remap- * pi/4)
  | gl/let [parity (mod $i 2)] _)
| radial: $i y 18
| shell 1
| subtract (plane -x (osc t 10 -150 0) | color [0 1 1])
| tint [0.5 0.5 1] (fresnel 5 * 0.2))

# I know it's a bit... cryptic. Lots of
# punctuation and some inscrutable
# single-letter variable names in
# there. But don't panic! It will all
# make sense soon.

# We'll start slow: drag the viewport
# around with your mouse, and scroll to
# move the camera in and out. If you
# get disoriented, you can reset the
# camera by double-clicking or pressing
# alt-r.

# This text field is a Janet program
# (https://janet-lang.org/) that runs
# every time you make a change. It
# implicitly "returns" its last
# expression. Uncomment this next line
# to return something else:

# (union :r 15 (torus z 50 25) (torus x 50 25 | move y 50))

# Neat. That's how we'll do this little
# intro.

# To uncomment a block of code, select
# the whole thing and press "cmd-/" or
# "ctrl-/".

# (torus z 60 30
# | rotate y (p.y / 50 + t * 3)
# | rotate [1 1 1 | normalize] t
# | move x 50
# | mirror :r 10 x
# | slow 0.4)

# Whoa. Okay enough with the demos for a
# minute.

# Bauble is a tool for composing "signed
# distance functions." Signed distance
# functions (or SDFs) are a different
# way to represent 3D shapes than you
# might be used to. Instead of
# triangles and vertices, SDFs
# represent shapes as pure functions of
# space, in a form that lets
# us "raymarch" those shapes in
# realtime.

# There are already a lot of great
# resources available on the internet
# for understanding SDFs and how they
# work and what "raymarching" is, so
# I'm not going to rehash that here.
# You can do a lot of cool stuff before
# you understand the math behind it.

# Like this:

# (box 10 | morph (ball 10) (sin+ (t + hash $i))
# | move y (sin+ (hash $i * 5 + t * 3) | pow 11 | ss * 20)
# | shade (hsv (hash $i / 8) 1 (hash $i | step 0.98))
# | tile: $i [30 0 30] :oversample true
# | union (ground -10 | shade (vec3 0.5)))
# (gl/def elevation (ss t 0 30 100 500))
# (gl/def eye [0 elevation (t * 50)])
# (set camera
#   (camera/perspective eye :target (z * elevation + [1 0 1 * eye])
#   | camera/pan (sin t * 0.1)
#   | camera/tilt (sin (t / 5) | ss 0 1 * pi/12)))

# None of this is very complicated! Once
# you understand how SDFs work, you can
# write a basic raymarcher in thirty
# lines of code. But there are a lot of
# different SDFs and SDF combinators
# and procedural noise functions and
# other things that you might want to
# bring into your raymarcher, and that
# takes work. When you're making
# generative art in raw GLSL, you'll
# frequently switch between high-level
# artistic decisions and low-level
# implementation details.

# So that's where Bauble comes in.
# Bauble makes it easier to play around
# with SDFs: it gives you a high-level,
# expression-oriented, "functional"
# library for writing shaders, as well
# as a UI full of tools for debugging
# and editing them interactively.

# For example: Bauble lets you edit
# values with with your mouse, so you
# can quickly tweak things until they
# feel just right. Uncomment the next
# block of code, then ctrl-click and
# drag the value 0.00 left to right
# until you find something you like.

# (def r 0.00)
# (box 80 :r 1
# | rotate [0 1 1 | normalize] r z (r * 0.7) x (r * 0.5)
# | mirror x y z :r 10
# | rotate y r x (r * 0.7) y (r * 0.5)
# | mirror x y z :r 10)
# (set camera nil)

# > You can also hold down cmd-shift and
# > move your mouse without clicking
# > anywhere to edit the value under the
# > text cursor. This is a workaround for
# > Firefox on macOS, which due to a
# > long-standing bug cannot report
# > ctrl-click events correctly. But it's
# > also convenient for editing values
# > while typing, without having to do
# > any precision mousing.
# > https://bugzilla.mozilla.org/show_bug.cgi?id=1504210

# This works on any number -- try it out
# on more of the examples! And note
# that when you're editing values with
# your mouse, Bauble will increment the
# smallest digit of the number, so you
# can increase the precision by adding
# zeroes to the end. In other words,
# editing a value like 3.0 will
# increment by 0.1, but editing 3.000
# will increment by 0.001.

# Bauble isn't just about 3D art,
# either. It also supports 2D SDFs:

# (circle (osc t 5 1 45)
# | move (hash2 $i - 0.5 * 2 * 10)
# | color (hsv (hash $i + (t * 0.1)) 0.7 1)
# | tile: $i [30 30] :oversample true :sample-from -1)

# Although a great use of 2D SDFs is
# constructing 3D SDFs...

# (circle (osc t 5 1 45)
# | move (hash2 $i - 0.5 * 2 * 10)
# | shade (hsv (hash $i + (t * 0.1)) 0.7 1)
# | with-lights (light/ambient 1 normal)
# | tile: $i [30 30] :oversample true :sample-from -1
# | revolve x 100
# | intersect :r 5 (ball [150 50 150]))

# Or even more simply:

# (rect 25 | rotate t | revolve y 100)

# Bauble wants to be a "batteries
# included" playground. It has a lot of
# familiar functions from the
# procedural art world built right in.
# Things like noise functions:

# (set background-color
#   (gl/let [s (frag-coord * 5 + [0 t])]
#     (vec3 (perlin+ (s + (perlin+ (s + perlin+ (s - (sin t)))))))))

# Helpers for constructing rotation
# matrices:

# (gl/def pos (rotate [100 100 0] y t))
# (union
#   (ball 10 | move pos)
#   (cone y 30 100 | align y (normalize pos)))

# Shadow casting:

# (torus y 110 20 | rotate z t x t y (t / 2)
# | shade [0.25 0.75 0.75]
# | union (ball 100 | shade [0 0.75 0])
# | union (ground -100 | shade (vec3 0.75)))

# But you can implement any of these
# things yourself! Bauble exposes a
# low-level API that you can use to
# generate GLSL directly. Or, well,
# indirectly. You still write it in
# lisp.

# If any of this piqued your curiosity,
# check out the Help page for
# interactive examples of Bauble's
# standard library, as well as more
# information about the language.

# There will probably also be a tutorial
# soon but I haven't written it yet.
# Sorry. Bauble is pretty new and I
# have a baby to keep alive.

# This video has a few tidbits, though:
#
# https://www.youtube.com/watch?v=XHNBRAgD4f4
