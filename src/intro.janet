# Hello, and welcome to an extremely
# early and unfinished demo!

# Bauble is a playground for creating
# and rendering 3D shapes using signed
# distance functions. Like this one:

(spoon :r 15
  (torus :z 50 25)
  (move :y 50 | rotate :y tau/4)
| fresnel 1)

# Drag the viewport around with your
# mouse, and scroll to move the camera
# in and out.

# This text field is a Janet program
# that is re-evaluated every time you
# make a change. This program "returns"
# whatever the final expression is --
# in this case, those interlocking
# donuts up there. Uncomment the next
# line to return something else:

# (morph 2.50 (sphere 50) (box 50))

# Janet is a fully-featured language, so
# you can define variables, functions,
# macros, loops -- anything your heart
# desires. Here's a nonsense example --
# to uncomment it, select the whole
# paragraph and press "cmd-/"
# or "ctrl-/":

# (var top 0)
# (defn hollow-box [size]
#   (subtract :r 5
#     (box size :r 2)
#     (sphere (* 1.20 size))))
# (defn stack-box [size]
#   (let [result (move :y (+ top size)
#                  (hollow-box size))]
#     (+= top (* 2 size))
#     result))
# (move :y -45
#   (union :r 10
#     ;(map stack-box [40 30 20])))

# You can also edit values with your
# mouse. Uncomment the next block of
# code, then ctrl-click and drag the
# value 0.00 left to right.

# (def r 0.00)
# (-> (box 80)
#   (rotate-pi :y r :z (* r 0.7) :x (* 0.5 r))
#   (symmetry))

# (Note for macOS Firefox users:
# there is a longstanding bug in Firefox
# where it doesn't report ctrl-click
# events correctly. If you have a
# physical mouse, you can use ctrl-right
# click instead. If you have a trackpad,
# that means ctrl-two finger drag. Since
# that's very annoying, there is also a
# hacky workaround: position the text
# cursor on the number you want to
# change, then hold down cmd+shift
# (without clicking) and move the
# mouse around.)
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
#   (shade (half-space :-y -50)
#     [0.9 0.9 0.9]))

# (shade) is an alias for (blinn-phong),
# a simple material shader. Try tweaking
# the parameters to see how they work,
# and remember that you can use your
# mouse to edit numbers! Also note that
# specular highlights depend on the
# viewing angle, so rotate the viewport
# a little too.

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

# Last thing: Bauble also has functions
# to modify the underlying color field
# in some way. Actually, just one at the
# moment:

# (fresnel green-box [1 1 0] 0.5 :exponent 5)

# That adds a little bit of (simulated)
# fresnel reflectivity to a surface.
# Move the camera around a bit to see
# what it does. Note that Bauble doesn't
# actually support reflection yet, so it
# just tints the edges, but it still
# looks pretty nice.

# All of the arguments are optional,
# so you can quickly apply it to a shape
# and add a little depth. Note that it
# works even with the default
# normal-coloring:

# (sphere 50)
# (fresnel (sphere 50))

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
# same as:

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

#### Light and shadow ####

# Currently there's no way to customize
# anything about the lighting. But it's
# coming soon!

#### Procedural texturing ####

# Haha aw I haven't gotten to that yet
# either. But a future version of Bauble
# will support it!

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
# (box 50 | twist :y 0.010)
# (box [50 10 50] | bend :x :y 0.010)
# (box 50 | swirl :y 0.040)

# Comments? Questions? Requests?
# https://github.com/ianthehenry/bauble/discussions

# Found a bug? Let me know!
# https://github.com/ianthehenry/bauble/issues
