# Welcome to Bauble!

# Bauble is a playground for making
# 2D and 3D art with signed distance
# fields. Like this:

(ball [50 100 100]
| union :r 50 (cyl y 25 50 | move [0 -100 0])
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

# Drag the viewport around with your
# mouse, and scroll to move the camera
# in and out. If you get disoriented,
# you can reset the camera by pressing
# the box icon in the top-left corner of
# the preview window.

# This text field is a Janet program
# that is re-evaluated every time you
# make a change. This program "returns"
# whatever the final expression is --
# in this case, that animated bauble up
# there. Uncomment the next line to
# return something else:

# (union :r 15 (torus z 50 25) (torus x 50 25 | move y 50) | fresnel)

# To uncomment a block of code, select
# the whole thing and press "cmd-/" or
# "ctrl-/".

# (torus z 60 30
# | rotate y (p.y * 0.07)
# | move x 50
# | mirror :r 10 x
# | fresnel
# | slow 0.25)

# You can also edit values with your
# mouse. Uncomment the next block of
# code (select it, and hit cmd-/ or ctrl-/),
# then ctrl-click and drag the value 0.00
# left to right.

# (def r 0.00)
# (box 80
# | rotate (normalize [1 1 1]) r z (r * 0.7) x (r * 0.5)
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

# Tutorial

# Uhh okay look. There used to be an interactive
# tutorial here. But then I rewrote Bauble from
# scratch, and changed a lot of how it worked,
# and the tutorial was all wrong. So the tutorial
# is gone for now. And I haven't written a new one.
# But! I did write actual documentation, which you
# can find in the "Help" link in the header. A real
# tutorial is forthcoming...

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
