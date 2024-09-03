# Okay, so this is a global thing. I admit that. I will not deny it.
# It would be smarter to have this be like a dynamic variable, and then to
# require the environment with the dynamic variable in place correctly,
# and use that. But. This is way easier and we can change it if it ever
# becomes a problem.
#
# The point of this is that there are some parts of the user environment that we
# can evaluate eagerly -- they're functions that don't change from one invocation
# of the user's script to the next, so we can just evaluate them up front.
#
# But there are other functions that we need to re-create from scratch every time.
# These are things like the `nearest-distance` definition: it's a forward-declared
# GLSL function, and we can only implement it once. And therefore anything that
# references that function -- like our shadow-casting code, or our raymarcher --
# also needs to be re-evaluated every time. So they are declared as "thunks", and
# gathered up into this array, and then evaluated every time we build a new user
# environment.
(def *thunks* @[])
(defn thunk [expr]
  (array/push *thunks* (macex expr)))
