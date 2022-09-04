(use ./util)
(use ./builtin-macros)
(use ./glsl-ports)
(import ./index :as glslisp)
(import ./type)

(make-generic-1 sin :f math/sin)
(make-generic-1 cos :f math/cos)
(make-generic-1 abs :f math/abs)
(make-generic-1 round :f math/round)
(make-generic-1 floor :f math/floor)
(make-generic-1 ceil :f math/ceil)
(make-generic-1 sqrt :f math/sqrt)
(make-generic-1 fract)
(make-generic-1 sign)

(make-vec-2 distance)

# Okay, this is confusing, but I think it's a good
# compromise.
(make-vec-1 length)
(make-vec-1 vec-length :glf length)

(make-vec-1 normalize)

# TODO: so this is a tricky one. this is overloaded
# on the GPU to accept one or two arguments, so it
# should work with one or two arguments. But... it
# doesn't, yet.
(make-generic-2 atan :f math/atan2)
(make-generic-2 max)
(make-generic-2 min)
(make-generic-2 mod)

# TODO: I'm inverting the order here because
# the GLSL order makes absolutely no sense.
# But this is the only function where I'm doing
# this, so maybe that will be upsetting to people.
# I dunno. Seems strictly better.
(make-generic-2 step :expr ~(step ,b ,a))

(make-generic-2 pow :f math/pow :expr ~(pow ,a ,b))
(def- pow- pow)

(defn pow [a b]
  (if (= (glslisp/typecheck b) type/float)
    (case (glslisp/typecheck a)
      type/vec2 (pow- a ~(vec2 ,b))
      type/vec3 (pow- a ~(vec3 ,b))
      type/vec4 (pow- a ~(vec4 ,b))
      (pow- a b))))

(make-generic-1 neg :glf - :f |(- $))
(make-generic-1 recip :expr ~(/ 1 ,a) :f |(/ $))

(make-variadic + identity)
(make-variadic - neg)
(make-variadic * identity)
(make-variadic / recip)

(make-generic-3 smoothstep)
(make-generic-3 clamp)
(make-generic-3 mix)
(make-numeric-3 hsv)
(make-numeric-3 hsl)

(defn ss [x &opt from-lo from-hi to-lo to-hi]
  (cond
    (nil? from-lo) (smoothstep 0 1 x)
    (nil? from-hi) (smoothstep 0 from-lo x)
    (nil? to-lo) (smoothstep from-lo from-hi x)
    (nil? to-hi) (* (smoothstep from-lo from-hi x) to-lo)
    (+ to-lo (* (smoothstep from-lo from-hi x) (- to-hi to-lo)))))

(defn remap+ [x]
  (* 0.5 (+ x 1)))

(defn perlin [x]
  (case (glslisp/typecheck x)
    type/vec2 ~(perlin2 ,x)
    type/vec3 ~(perlin3 ,x)
    type/vec4 ~(perlin4 ,x)
    type/unknown (errorf "cannot determine type of expression; please tell ian about this")
    (errorf "noise requires a vector")))

(defn perlin+ [x] (remap+ (perlin x)))

# TODO...
# (defn simplex [x]
#   (case (glslisp/typecheck x)
#     type/vec2 ~(simplex2 ,x)
#     type/vec3 ~(simplex3 ,x)
#     type/vec4 ~(simplex4 ,x)
#     type/unknown (errorf "cannot determine type of expression; please tell ian about this")
#     (errorf "noise requires a vector")))

(make-generic-1 rotate-x-matrix :glf rotate_x)
(make-generic-1 rotate-y-matrix :glf rotate_y)
(make-generic-1 rotate-z-matrix :glf rotate_z)

(make-matrix-2 mat3/multiply :glf *)

# Primitives

(defn vec2 [& args] ~(vec2 ,;args))
(defn vec3 [& args] ~(vec3 ,;args))
(defn vec4 [& args] ~(vec4 ,;args))

(defn . [x k] ~(. ,x ,k))

# Helpers

(defn rgb [r g b] [(/ r 255) (/ g 255) (/ b 255)])

(defn hsv-deg [h s v] (hsv (/ h 360) s v))

(defn hsl-deg [h s l] (hsl (/ h 360) s l))

(defn sin+ [x]
  (remap+ (sin x)))

(defn sin- [x]
  (- 1 (sin+ x)))

(defn cos+ [x]
  (remap+ (cos x)))

(defn cos- [x]
  (- 1 (cos+ x)))

# TODO: should probably fill out the rest of these
(defmacro += [expr value]
  ~(set ,expr (,+ ,expr ,value)))
(defmacro *= [expr value]
  ~(set ,expr (,* ,expr ,value)))
