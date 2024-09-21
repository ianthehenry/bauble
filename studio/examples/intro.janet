# Welcome to Bauble!

# Bauble is an offline playground for
# turning parentheses into pixels. Here's
# the source for that hot air balloon over
# there:

(ball [50 100 100]
| union :r 50 (cylinder y 25 50 | move [0 -100 0])
| scale y (ss p.y -100 100 1 0.8)
| color
  (blinn-phong r3
    (hsv (quantize (p.y / 100 - (t / 5)) 8) 1 1) :g 5
  | move y (sin (t * 1.5) | ss * 100 / 8 * parity)
  | rotate z (parity * 2 - 1 * pi/4)
  | gl/let [parity (mod $i 2)] _)
| radial: $i y 18
| shell 1
| subtract (plane -x (osc t 10 -150 0) | color [0 1 1])
| tint [0.5 0.5 1] (fresnel 5 * 0.2))

# I know it's a bit... cryptic. Lots
# of punctuation and some inscrutable
# single-letter variable names in
# there. But don't panic! It will
# all make sense soon.

# We'll start slow: drag the viewport
# around with your mouse, and scroll to
# move the camera in and out. If you get
# disoriented, you can reset the camera
# by pressing the box icon in the top-left
# corner of the preview window.

# This text field is a Janet program
# (https://janet-lang.org/) that runs
# every time you make a change. It
# implicitly "returns" its last
# expression. Uncomment this next line
# to return something else:

# (union :r 15 (torus z 50 25) (torus x 50 25 | move y 50))

# Neat. That's how we'll do this little intro.

# To uncomment a block of code, select
# the whole thing and press "cmd-/" or
# "ctrl-/".

# (torus z 60 30
# | rotate y (p.y / 50 + t * 3)
# | rotate z t
# | move x 50
# | mirror :r 10 x
# | slow 0.25)

# Whoa. Okay enough with the demos for a minute.

# Bauble is actually a tool for composing
# "signed distance functions." A signed
# distance function (or SDF) is a way to
# represent 3D shapes with pure functions
# in a way that makes it possible to
# "raymarch" those shapes in realtime.
# There are no triangle meshes involved,
# no quads, just pure numeric expressions.
# There are already a lot of great
# resources available on the internet for
# understanding SDFs and how they work
# and what "raymarching" is, so I'm not
# going to rehash that here. You can do a
# lot of cool stuff before you understand
# the math behind it.

# Like this:

# (box 10 | morph (ball 10) (sin+ (t + hash $i))
# | move y (sin+ (hash $i * 5 + t * 3) | pow 11 | ss * 20)
# | blinn-phong (hsv (hash $i / 8) 1 (hash $i | step 0.98))
# | tile: $i [30 0 30] :oversample true
# | union (ground -10 | blinn-phong (vec3 0.5))
# )

# The beauty of SDFs is that they're so simple
# that, once you understand how they work, you
# can write a basic raymarcher in GLSL in like
# ten minutes. And this is very cool -- but it
# takes quite a bit longer to *compose a scene*
# with SDFs.

# So Bauble makes this easier. It gives you a
# high-level, expression-oriented, "functional"
# library for composing SDFs, as well as a UI
# full of tools for debugging and editing them.

# Like, one useful feature is that Bauble lets you edit
# values with your mouse. Uncomment the next block of
# code, then ctrl-click and drag the value 0.00
# left to right until you find something you like.

# (def r 0.00)
# (box 80
# | rotate [0 1 1 | normalize] r z (r * 0.7) x (r * 0.5)
# | mirror x y z :r 10
# | rotate y r x (r * 0.7) y (r * 0.5)
# | mirror x y z :r 10)

# > You can also hold down cmd-shift and
# > move your mouse without clicking
# > anywhere to edit the value under the
# > text cursor. This is a workaround for
# > Firefox on macOS, which due to a
# > long-standing bug cannot report
# > ctrl-click events correctly. But it's
# > also convenient for editing values
# > while typing, without having to do any
# > precision mousing.
# > https://bugzilla.mozilla.org/show_bug.cgi?id=1504210

# When editing values with your mouse,
# Bauble will increment the smallest
# digit of the number, so you can
# increase the precision by adding
# zeroes to the end. In other words,
# editing a value like 3.0 will
# increment by 0.1, but editing 3.000
# will increment by 0.001.

# Bauble isn't just about 3D art, either. It
# supports 2D SDFs:

# (circle (osc t 5 1 45)
# | move (hash2 $i - 0.5 * 2 * 10)
# | color (hsv (hash $i + (t * 0.1)) 0.95 1)
# | tile: $i [30 30] :oversample true :sample-from -1)

# Although a great use of 2D SDFs is constructing 3D SDFs...

# (circle (osc t 5 1 45)
# | move (hash2 $i - 0.5 * 2 * 10)
# | color (hsv (hash $i + (t * 0.1)) 0.95 1)
# | tile: $i [30 30] :oversample true :sample-from -1
# | extrude y 100
# | intersect (ball 150))

# Or even simpler:

# (rect 25 | rotate t | revolve y 100)

# Bauble is sort of a "batteries included"
# playground. It has lots of functions that
# come up in the procedural art world built
# right in. Things like noise functions:

# (set background-color
#   (gl/let [s (frag-coord * 10 + [0 t])]
#     (vec3 (perlin+ (s + (perlin+ (s + perlin+ (s - (sin t)))))))))

# Various helpers for constructing rotation
# matrices:

# (gl/def pos ([(sin t) 1 (cos t)] * 100))
# (union
#   (ball 10 | move pos)
#   (cone y 30 100 | align y (normalize pos)))

# And casting shadows:

# (torus y 110 20 | rotate z t x t y (t / 2)
# | blinn-phong [0.25 0.75 0.75]
# | union (ball 100 | blinn-phong [0 0.75 0])
# | union (ground -100 | blinn-phong (vec3 0.75)))

# But you can implement any of these things
# yourself! Bauble exposes a low-level API
# that you can use to generate GLSL directly.
# Or, well, indirectly. You have to go through
# a little s-expression DSL first.

# If any of this piqued your curiosity, check
# out the Help page for interactive examples of
# all of Bauble's built-in functions, as well
# as more information about the language.

# There will probably also be a tutorial soon
# but I haven't written it yet. Sorry. Bauble
# is pretty new and I have a baby.

# In the meantime, join the Bauble Discord:
#
# https://discord.gg/NzR375gJH6
#
# Or reach out to me on Twitter:
#
# https://twitter.com/ianthehenry
#
# To apply some social pressure to help me to
# finish it.
