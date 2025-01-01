(import ../../jlsl)

# The idea is that we want to define some expensive expressions -- basically,
# our lighting calculations -- that might be used by multiple different shapes.
# You might have a sphere shaded with a certain light, and a box shaded with the
# same light, and if you're rendering their smooth union you don't want to
# cast that light ray twice in the region where both color fields contribute to
# the final result.
#
# So... we could just not care about this, if we had the simplest form of common
# subexpression elimination. If the JLSL compiler had the ability to perform that
# optimization pass, then we could just let both fields compute the same light and
# trust that it would be hoisted up automatically and we wouldn't have to think about
# it. (This is pretty hard, though, since in a smooth union we conditionally evaluate
# color fields only when they contribute anything to the final result. And you don't
# want to cast the light if *neither* shape contributes anything to the final color
# field, as that would change the behavior of the program... although that is
# equivalent to what we're doing here and it would still be nicer to have it as an
# implicit pass.)
#
# But... we don't. Instead we have to mark specific expressions as explicitly expensive,
# put them in variables, and then hoist those variables up to the root of the distance
# or color field expressions that we render.
#
# Now, the *right* thing to do would be to only hoist them up to the lowest common
# ancestor. But that would require, like, being smart, and doing smart things, and
# building a complex expression graph.
#
# This would be a reasonable thing to do in the future, but right now we just hoist
# everything up as far as we can. Because *in practice* we only use this for light
# or occlusion calculations, and *in practice* you pretty much always use a global
# set of lights for the whole scene, this doesn't make a difference. But it *could*,
# and it's a reasonable avenue to explore in the future, especially if we want to do
# something like adding lights as explicit elements in a scene graph.

(defdyn *hoisted-vars* "A dynamic variable that keeps track of all hoisted variables.")

(defn hoist [name expr]
  (def expr (jlsl/coerce-expr expr))
  (def variable (jlsl/variable/new name (jlsl/expr/type expr)))
  (put (dyn *hoisted-vars*) variable expr)
  variable)
