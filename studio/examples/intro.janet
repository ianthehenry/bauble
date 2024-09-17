# Welcome to Bauble!

# Bauble is an offline playground for
# turning parentheses into pixels. Here's
# the source for that hot air balloon over
# there:

(ball [50 100 100]
| union :r 50 (tube y 25 50 | move [0 -100 0])
| scale y (ss p.y [-100 100] [1 0.8])
| recolor
  (blinn-phong r3 (hsv (quantize (p.y / 100 - (t / 5)) 8) 1 1) :g 5
  | move y (sin (t * 1.5) | ss * 100 / 8 * parity)
  | rotate z (parity * 2 - 1 * pi/4)
  | gl/let [parity (mod $i 2)] _)
| radial: $i y 18
| shell 1
| subtract (plane -x (osc t 10 -150 0) | color [0 1 1])
| fresnel :color (vec3 0.1))

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

# (union :r 15 (torus z 50 25) (torus x 50 25 | move y 50) | fresnel)

# Neat. That's how we'll do this little intro.

# To uncomment a block of code, select
# the whole thing and press "cmd-/" or
# "ctrl-/".

# (torus z 60 30
# | rotate y (p.y / 50 + t * 3)
# | rotate z t
# | move x 50
# | mirror :r 10 x
# | fresnel
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
# | union (plane y -10 | blinn-phong (vec3 0.5))
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

# Like, one cool thing is that Bauble lets you edit
# values with your mouse. Uncomment the next block of
# code, then ctrl-click and drag the value 0.00
# left to right.

# (def r 0.00)
# (box 80
# | rotate (normalize [0 1 1]) r z (r * 0.7) x (r * 0.5)
# | mirror x y z
# | rotate y r z (r * 0.7) x (r * 0.5)
# | mirror x y z)

# You can also hold down cmd-shift and
# move your mouse without clicking
# anywhere to edit the value under the
# text cursor. This is a workaround for
# Firefox on macOS, which due to a
# long-standing bug cannot report
# ctrl-click events correctly. But it's
# also convenient for editing values
# while typing, without having to do any
# precision mousing.
# https://bugzilla.mozilla.org/show_bug.cgi?id=1504210

# When editing values with your mouse,
# Bauble will increment the smallest
# digit of the number, so you can
# increase the precision by adding
# zeroes to the end. In other words,
# editing a value like 3.0 will
# increment by 0.1, but editing 3.000
# will increment by 0.001.

# More

# Uhh okay look. There used to be a pretty
# long interactive tutorial here. But then I
# rewrote Bauble from scratch, and changed a
# lot of how it worked, and the tutorial was
# all wrong. So the tutorial is gone for now.
# And I haven't written a new one. But! I did
# write actual documentation this time, which
# you can find in the "Help" link in the
# header. A real tutorial is forthcoming...

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
