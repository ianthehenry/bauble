(use ./import)

# TODO: it would be nice to add vec4 inputs to hash2 and hash3

# Taken from David Hoskins: https://www.shadertoy.com/view/4djSRW
(defhelper- :float hash [:float v]
  (set v (v * 0.1031 | fract))
  (*= v (v + 33.33))
  (*= v (v + v))
  (return (fract v)))

(overload :float hash [:vec2 v]
  (var v (v.xyx * 0.1031 | fract))
  (+= v (dot v (v.yzx + 33.33)))
  (return (v.x + v.y * v.z | fract)))

(overload :float hash [:vec3 v]
  (set v (v * .1031 | fract))
  (+= v (dot v (v.zyx + 31.32)))
  (return (v.x + v.y * v.z | fract)))

 (overload :float hash [:vec4 v]
  (set v (v * [.1031 .1030 .0973 .1099] | fract))
  (+= v (dot v (v.wzxy + 33.33)))
  (return ((v.x + v.y) * (v.z + v.w) | fract)))

(defhelper- :vec2 hash2 [:float v]
  (var v (vec3 v * [.1031 .1030 .0973] | fract))
  (+= v (dot v (v.yzx + 33.33)))
  (return (v.xx + v.yz * v.zy | fract)))

(overload :vec2 hash2 [:vec2 v]
  (var v (v.xyx * [.1031 .1030 .0973] | fract))
  (+= v (dot v (v.yzx + 33.33)))
  (return (v.xx + v.yz * v.zy | fract)))

(overload :vec2 hash2 [:vec3 v]
  (set v (v * [.1031 .1030 .0973] | fract))
  (+= v (dot v (v.yzx + 33.33)))
  (return (v.xx + v.yz * v.zy | fract)))

(defhelper- :vec3 hash3 [:float v]
  (var v (vec3 v * [.1031 .1030 .0973] | fract))
  (+= v (dot v (v.yzx + 33.33)))
  (return (v.xxy + v.yzz * v.zyx | fract)))
(overload :vec3 hash3 [:vec2 v]
  (var v (v.xyx * [.1031 .1030 .0973] | fract))
  (+= v (dot v (v.yxz + 33.33)))
  (return (v.xxy + v.yzz * v.zyx | fract)))
(overload :vec3 hash3 [:vec3 v]
  (set v (v * [.1031 .1030 .0973] | fract))
  (+= v (dot v (v.yxz + 33.33)))
  (return (v.xxy + v.yxx * v.zyx | fract)))

(defhelper- :vec4 hash4 [:float v]
  (var v (vec4 v * [.1031 .1030 .0973 .1099] | fract))
  (+= v (dot v (v.wzxy + 33.33)))
  (return (v.xxyz + v.yzzw * v.zywx | fract)))
(overload :vec4 hash4 [:vec2 v]
  (var v (v.xyxy * [.1031 .1030 .0973 .1099] | fract))
  (+= v (dot v (v.wzxy + 33.33)))
  (return (v.xxyz + v.yzzw * v.zywx | fract)))
(overload :vec4 hash4 [:vec3 v]
  (var v (v.xyzx  * [.1031 .1030 .0973 .1099] | fract))
  (+= v (dot v (v.wzxy + 33.33)))
  (return (v.xxyz + v.yzzw * v.zywx | fract)))
(overload :vec4 hash4 [:vec4 v]
  (set v (v * [.1031 .1030 .0973 .1099] | fract))
  (+= v (dot v (v.wzxy + 33.33)))
  (return (v.xxyz + v.yzzw * v.zywx | fract)))

(def- hash- hash)
(def- hash2- hash2)
(def- hash3- hash3)
(def- hash4- hash4)

(defn hash
  ```
  Return a pseudorandom float. The input can be a float or vector. With multiple arguments,
  this will return the hash of the sum.

  This should return consistent results across GPUs, unlike high-frequency sine functions.
  ```
  [& args]
  (if (> (@length args) 1) (hash- (+ ;args)) (hash- ;args)))
(defn hash2
  ```
  Return a pseudorandom `vec2`. The input can be a float or vector. With multiple arguments,
  this will return the hash of the sum.

  This should return consistent results across GPUs, unlike high-frequency sine functions.
  ```
  [& args]
  (if (> (@length args) 1) (hash2- (+ ;args)) (hash2- ;args)))
(defn hash3
  ```
  Return a pseudorandom `vec3`. The input can be a float or vector. With multiple arguments,
  this will return the hash of the sum.

  This should return consistent results across GPUs, unlike high-frequency sine functions.
  ```
  [& args]
  (if (> (@length args) 1) (hash3- (+ ;args)) (hash3- ;args)))
(defn hash4
  ```
  Return a pseudorandom `vec4`. The input can be a float or vector. With multiple arguments,
  this will return the hash of the sum.

  This should return consistent results across GPUs, unlike high-frequency sine functions.
  ```
  [& args]
  (if (> (@length args) 1) (hash4- (+ ;args)) (hash4- ;args)))

(defhelper- :vec2 mod289 [:vec2 x] (return (x - (floor (x * (/ 289)) * 289))))
(overload   :vec3 mod289 [:vec3 x] (return (x - (floor (x * (/ 289)) * 289))))
(overload   :vec4 mod289 [:vec4 x] (return (x - (floor (x * (/ 289)) * 289))))
(defhelper- :vec3 permute [:vec3 x] (return (x * 34 + 10 * x | mod289)))
(overload   :vec4 permute [:vec4 x] (return (x * 34 + 10 * x | mod289)))
(defhelper- :vec4 taylor-inv-sqrt [:vec4 r] (return (1.79284291400159 - (0.85373472095314 * r))))
(defhelper- :vec2 fade [:vec2 t] (return (* t t t (t * (t * 6 - 15) + 10))))
(overload   :vec3 fade [:vec3 t] (return (* t t t (t * (t * 6 - 15) + 10))))
(overload   :vec4 fade [:vec4 t] (return (* t t t (t * (t * 6 - 15) + 10))))
(defhelper- :vec3 mod7 [:vec3 x] (return (x - (floor (x * (/ 7)) * 7))))

(defhelper- :float perlin [:vec2 point]
  (var Pi ((floor point.xyxy) + [0 0 1 1]))
  (var Pf ((fract point.xyxy) - [0 0 1 1]))
  (set Pi (mod289 Pi))
  (var ix Pi.xzxz)
  (var iy Pi.yyww)
  (var fx Pf.xzxz)
  (var fy Pf.yyww)

  (var i (permute (permute ix + iy)))

  (var gx ((fract (i * (/ 41))) * 2 - 1))
  (var gy (abs gx - 0.5))
  (var tx (floor (gx + 0.5)))
  (set gx (gx - tx))

  (var g00 [gx.x gy.x])
  (var g10 [gx.y gy.y])
  (var g01 [gx.z gy.z])
  (var g11 [gx.w gy.w])

  (var norm (taylor-inv-sqrt [(dot g00 g00) (dot g01 g01) (dot g10 g10) (dot g11 g11)]))
  (*= g00 norm.x)
  (*= g01 norm.y)
  (*= g10 norm.z)
  (*= g11 norm.w)

  (var n00 (dot g00 [fx.x fy.x]))
  (var n10 (dot g10 [fx.y fy.y]))
  (var n01 (dot g01 [fx.z fy.z]))
  (var n11 (dot g11 [fx.w fy.w]))

  (var fade-xy (fade Pf.xy))
  (var n-x (mix [n00 n01] [n10 n11] fade-xy.x))
  (var n-xy (mix n-x.x n-x.y fade-xy.y))
  (return (2.3 * n-xy)))

(overload :float perlin [:vec3 point]
  (var Pi0 (floor point))
  (var Pi1 (Pi0 + 1))
  (var Pi0 (mod289 Pi0))
  (var Pi1 (mod289 Pi1))
  (var Pf0 (fract point))
  (var Pf1 (Pf0 - 1))
  (var ix [Pi0.x Pi1.x Pi0.x Pi1.x])
  (var iy [Pi0.yy Pi1.yy])
  (var iz0 Pi0.zzzz)
  (var iz1 Pi1.zzzz)

  (var ixy (permute ix + iy | permute))
  (var ixy0 (ixy + iz0 | permute))
  (var ixy1 (ixy + iz1 | permute))

  (var gx0 (ixy0 * (/ 7)))
  (var gy0 (floor gx0 * (/ 7) | fract - 0.5))
  (set gx0 (fract gx0))
  (var gz0 (- 0.5 (abs gx0) (abs gy0)))
  (var sz0 (step gz0 (vec4 0))) # TODO: step overload?
  (-= gx0 (sz0 * (step 0 gx0 - 0.5)))
  (-= gy0 (sz0 * (step 0 gy0 - 0.5)))

  (var gx1 (ixy1 * (/ 7)))
  (var gy1 (floor gx1 * (/ 7) | fract - 0.5))
  (set gx1 (fract gx1))
  (var gz1 (- 0.5 (abs gx1) (abs gy1)))
  (var sz1 (step gz1 (vec4 0))) # TODO: overload?
  (-= gx1 (sz1 * (step 0 gx1 - 0.5)))
  (-= gy1 (sz1 * (step 0 gy1 - 0.5)))

  (var g000 [gx0.x gy0.x gz0.x])
  (var g100 [gx0.y gy0.y gz0.y])
  (var g010 [gx0.z gy0.z gz0.z])
  (var g110 [gx0.w gy0.w gz0.w])
  (var g001 [gx1.x gy1.x gz1.x])
  (var g101 [gx1.y gy1.y gz1.y])
  (var g011 [gx1.z gy1.z gz1.z])
  (var g111 [gx1.w gy1.w gz1.w])

  (var norm0 (taylor-inv-sqrt [(dot g000 g000) (dot g010 g010) (dot g100 g100) (dot g110 g110)]))
  (*= g000 norm0.x)
  (*= g010 norm0.y)
  (*= g100 norm0.z)
  (*= g110 norm0.w)
  (var norm1 (taylor-inv-sqrt [(dot g001 g001) (dot g011 g011) (dot g101 g101) (dot g111 g111)]))
  (*= g001 norm1.x)
  (*= g011 norm1.y)
  (*= g101 norm1.z)
  (*= g111 norm1.w)

  (var n000 (dot g000 Pf0))
  (var n100 (dot g100 [Pf1.x Pf0.yz]))
  (var n010 (dot g010 [Pf0.x Pf1.y Pf0.z]))
  (var n110 (dot g110 [Pf1.xy Pf0.z]))
  (var n001 (dot g001 [Pf0.xy Pf1.z]))
  (var n101 (dot g101 [Pf1.x Pf0.y Pf1.z]))
  (var n011 (dot g011 [Pf0.x Pf1.yz]))
  (var n111 (dot g111 Pf1))

  (var fade-xyz (fade Pf0))
  (var n-z (mix [n000 n100 n010 n110] [n001 n101 n011 n111] fade-xyz.z))
  (var n-yz (mix n-z.xy n-z.zw fade-xyz.y))
  (var n-xyz (mix n-yz.x n-yz.y fade-xyz.x))
  (return (2.2 * n-xyz)))

(overload :float perlin [:vec4 point]
  (var Pi0 (floor point))
  (var Pi1 (Pi0 + 1))
  (set Pi0 (mod289 Pi0))
  (set Pi1 (mod289 Pi1))
  (var Pf0 (fract point))
  (var Pf1 (Pf0 - 1))
  (var ix [Pi0.x Pi1.x Pi0.x Pi1.x])
  (var iy [Pi0.yy Pi1.yy])
  (var iz0 Pi0.zzzz)
  (var iz1 Pi1.zzzz)
  (var iw0 Pi0.wwww)
  (var iw1 Pi1.wwww)

  (var ixy (permute ix + iy | permute))
  (var ixy0 (ixy + iz0 | permute))
  (var ixy1 (ixy + iz1 | permute))
  (var ixy00 (ixy0 + iw0 | permute))
  (var ixy01 (ixy0 + iw1 | permute))
  (var ixy10 (ixy1 + iw0 | permute))
  (var ixy11 (ixy1 + iw1 | permute))

  (var gx00 (ixy00 * (/ 7)))
  (var gy00 (floor gx00 * (/ 7)))
  (var gz00 (floor gy00 * (/ 6)))
  (set gx00 (fract gx00 - 0.5))
  (set gy00 (fract gy00 - 0.5))
  (set gz00 (fract gz00 - 0.5))
  (var gw00 (- 0.75 (abs gx00) (abs gy00) (abs gz00)))
  (var sw00 (step gw00 (vec4 0)))
  (-= gx00 (sw00 * (step 0 gx00 - 0.5)))
  (-= gy00 (sw00 * (step 0 gy00 - 0.5)))

  (var gx01 (ixy01 * (/ 7)))
  (var gy01 (floor gx01 * (/ 7)))
  (var gz01 (floor gy01 * (/ 6)))
  (set gx01 (fract gx01 - 0.5))
  (set gy01 (fract gy01 - 0.5))
  (set gz01 (fract gz01 - 0.5))
  (var gw01 (- 0.75 (abs gx01) (abs gy01) (abs gz01)))
  (var sw01 (step gw01 (vec4 0)))
  (-= gx01 (sw01 * (step 0 gx01 - 0.5)))
  (-= gy01 (sw01 * (step 0 gy01 - 0.5)))

  (var gx10 (ixy10 * (/ 7)))
  (var gy10 (floor gx10 * (/ 7)))
  (var gz10 (floor gy10 * (/ 6)))
  (set gx10 (fract gx10 - 0.5))
  (set gy10 (fract gy10 - 0.5))
  (set gz10 (fract gz10 - 0.5))
  (var gw10 (- 0.75 (abs gx10) (abs gy10) (abs gz10)))
  (var sw10 (step gw10 (vec4 0)))
  (-= gx10 (sw10 * (step 0 gx10 - 0.5)))
  (-= gy10 (sw10 * (step 0 gy10 - 0.5)))

  (var gx11 (ixy11 * (/ 7)))
  (var gy11 (floor gx11 * (/ 7)))
  (var gz11 (floor gy11 * (/ 6)))
  (set gx11 (fract gx11 - 0.5))
  (set gy11 (fract gy11 - 0.5))
  (set gz11 (fract gz11 - 0.5))
  (var gw11 (- 0.75 (abs gx11) (abs gy11) (abs gz11)))
  (var sw11 (step gw11 (vec4 0)))
  (-= gx11 (sw11 * (step 0 gx11 - 0.5)))
  (-= gy11 (sw11 * (step 0 gy11 - 0.5)))

  (var g0000 [gx00.x gy00.x gz00.x gw00.x])
  (var g1000 [gx00.y gy00.y gz00.y gw00.y])
  (var g0100 [gx00.z gy00.z gz00.z gw00.z])
  (var g1100 [gx00.w gy00.w gz00.w gw00.w])
  (var g0010 [gx10.x gy10.x gz10.x gw10.x])
  (var g1010 [gx10.y gy10.y gz10.y gw10.y])
  (var g0110 [gx10.z gy10.z gz10.z gw10.z])
  (var g1110 [gx10.w gy10.w gz10.w gw10.w])
  (var g0001 [gx01.x gy01.x gz01.x gw01.x])
  (var g1001 [gx01.y gy01.y gz01.y gw01.y])
  (var g0101 [gx01.z gy01.z gz01.z gw01.z])
  (var g1101 [gx01.w gy01.w gz01.w gw01.w])
  (var g0011 [gx11.x gy11.x gz11.x gw11.x])
  (var g1011 [gx11.y gy11.y gz11.y gw11.y])
  (var g0111 [gx11.z gy11.z gz11.z gw11.z])
  (var g1111 [gx11.w gy11.w gz11.w gw11.w])

  (var norm00 (taylor-inv-sqrt [(dot g0000 g0000) (dot g0100 g0100) (dot g1000 g1000) (dot g1100 g1100)]))
  (*= g0000 norm00.x)
  (*= g0100 norm00.y)
  (*= g1000 norm00.z)
  (*= g1100 norm00.w)

  (var norm01 (taylor-inv-sqrt [(dot g0001 g0001) (dot g0101 g0101) (dot g1001 g1001) (dot g1101 g1101)]))
  (*= g0001 norm01.x)
  (*= g0101 norm01.y)
  (*= g1001 norm01.z)
  (*= g1101 norm01.w)

  (var norm10 (taylor-inv-sqrt [(dot g0010 g0010) (dot g0110 g0110) (dot g1010 g1010) (dot g1110 g1110)]))
  (*= g0010 norm10.x)
  (*= g0110 norm10.y)
  (*= g1010 norm10.z)
  (*= g1110 norm10.w)

  (var norm11 (taylor-inv-sqrt [(dot g0011 g0011) (dot g0111 g0111) (dot g1011 g1011) (dot g1111 g1111)]))
  (*= g0011 norm11.x)
  (*= g0111 norm11.y)
  (*= g1011 norm11.z)
  (*= g1111 norm11.w)

  (var n0000 (dot g0000 Pf0))
  (var n1000 (dot g1000 [Pf1.x Pf0.yzw]))
  (var n0100 (dot g0100 [Pf0.x Pf1.y Pf0.zw]))
  (var n1100 (dot g1100 [Pf1.xy Pf0.zw]))
  (var n0010 (dot g0010 [Pf0.xy Pf1.z Pf0.w]))
  (var n1010 (dot g1010 [Pf1.x Pf0.y Pf1.z Pf0.w]))
  (var n0110 (dot g0110 [Pf0.x Pf1.yz Pf0.w]))
  (var n1110 (dot g1110 [Pf1.xyz Pf0.w]))
  (var n0001 (dot g0001 [Pf0.xyz Pf1.w]))
  (var n1001 (dot g1001 [Pf1.x Pf0.yz Pf1.w]))
  (var n0101 (dot g0101 [Pf0.x Pf1.y Pf0.z Pf1.w]))
  (var n1101 (dot g1101 [Pf1.xy Pf0.z Pf1.w]))
  (var n0011 (dot g0011 [Pf0.xy Pf1.zw]))
  (var n1011 (dot g1011 [Pf1.x Pf0.y Pf1.zw]))
  (var n0111 (dot g0111 [Pf0.x Pf1.yzw]))
  (var n1111 (dot g1111 Pf1))

  (var fade-xyzw (fade Pf0))
  (var n-0w (mix [n0000 n1000 n0100 n1100] [n0001 n1001 n0101 n1101] fade-xyzw.w))
  (var n-1w (mix [n0010 n1010 n0110 n1110] [n0011 n1011 n0111 n1111] fade-xyzw.w))
  (var n-zw (mix n-0w n-1w fade-xyzw.z))
  (var n-yzw (mix n-zw.xy n-zw.zw fade-xyzw.y))
  (var n-xyzw (mix n-yzw.x n-yzw.y fade-xyzw.x))
  (return (2.2 * n-xyzw)))

(def- perlin- perlin)
(defn perlin
  ````
  Returns perlin noise ranging from `-1` to `1`. The input `point` can be a vector of any dimension.

  Use `perlin+` to return noise in the range `0` to `1`.
  ````
  [point &opt period]
  (if period
    (perlin- (/ point period))
    (perlin- point)))

(defn perlin+
  ````
  Perlin noise in the range `0` to `1`.

  ```example
  (ball 100 | color (perlin+ (p.xy / 10) | vec3))
  ```
  ```example
  (ball 100 | color (perlin+ (p / 10) | vec3))
  ```
  ```example
  (ball 100 | color (perlin+ [(p / 10) t] | vec3))
  ```
  ````
  [point &opt period]
  (remap+ (perlin point period)))

(defmacro- define-worley2d [return-type name & closing-statements]
  ~(defhelper- ,return-type ,name [:vec2 point]
    (def K (1 / 7))
    (def Ko (3 / 7))
    (def jitter 1)
    (var Pi (mod289 (floor point)))
    (var Pf (fract point))
    (var oi [-1 0 1])
    (var of [-0.5 0.5 1.5])
    (var px (permute (+ Pi.x oi)))
    (var p (permute (+ px.x Pi.y oi)))
    (var ox (fract (p * K) - Ko))
    (var oy (mod7 (floor (p * K)) * K - Ko))
    (var dx (Pf.x + 0.5 + (jitter * ox)))
    (var dy (Pf.y - of + (jitter * oy)))
    (var d1 (dx * dx + (dy * dy)))
    (set p (permute (+ px.y Pi.y oi)))
    (set ox (fract (p * K) - Ko))
    (set oy (mod7 (floor (p * K)) * K - Ko))
    (set dx (Pf.x - 0.5 + (jitter * ox)))
    (set dy (Pf.y - of + (jitter * oy)))
    (var d2 (dx * dx + (dy * dy)))
    (set p (permute (+ px.z Pi.y oi)))
    (set ox (fract (p * K) - Ko))
    (set oy (mod7 (floor (p * K)) * K - Ko))
    (set dx (Pf.x - 1.5 + (jitter * ox)))
    (set dy (Pf.y - of + (jitter * oy)))
    (var d3 (dx * dx + (dy * dy)))
    ,;closing-statements))

(define-worley2d :float worley
  (return (sqrt (min (min (min d1 d2) d3)))))

(define-worley2d :vec2 worley2
  (var d1a (min d1 d2))
  (set d2 (max d1 d2))
  (set d2 (min d2 d3))
  (set d1 (min d1a d2))
  (set d2 (max d1a d2))
  (set d1.xy (if (< d1.x d1.y) d1.xy d1.yx))
  (set d1.xz (if (< d1.x d1.z) d1.xz d1.zx))
  (set d1.yz (min d1.yz d2.yz))
  (set d1.y (min d1.y d1.z))
  (set d1.y (min d1.y d2.x))
  (return (sqrt d1.xy)))

# This is a very gross "copy and paste" macro that exists to generate
# similar but different versions of worley noise.
(defmacro- overload-worley3d [return-type name & closing-statements]
  ~(overload ,return-type ,name [:vec3 point]
    (def K (1 / 7))
    (def Ko (0.5 * (1 - K)))
    (def K2 (1 / 49))
    (def Kz (1 / 6))
    (def Kzo (0.5 - (1 / 12)))
    (def jitter 1.0)

    (var Pi (mod289 (floor point)))
    (var Pf (fract point - 0.5))

    (var Pfx (Pf.x + [1 0 -1]))
    (var Pfy (Pf.y + [1 0 -1]))
    (var Pfz (Pf.z + [1 0 -1]))

    (var p (permute (Pi.x + [-1 0 1])))
    (var p1 (permute (p + Pi.y - 1.0)))
    (var p2 (permute (p + Pi.y)))
    (var p3 (permute (p + Pi.y + 1.0)))

    (var p11 (permute (p1 + Pi.z - 1.0)))
    (var p12 (permute (p1 + Pi.z)))
    (var p13 (permute (p1 + Pi.z + 1.0)))

    (var p21 (permute (p2 + Pi.z - 1.0)))
    (var p22 (permute (p2 + Pi.z)))
    (var p23 (permute (p2 + Pi.z + 1.0)))

    (var p31 (permute (p3 + Pi.z - 1.0)))
    (var p32 (permute (p3 + Pi.z)))
    (var p33 (permute (p3 + Pi.z + 1.0)))

    (var ox11 (fract (p11 * K) - Ko))
    (var oy11 (mod7 (floor (p11 * K)) * K - Ko))
    (var oz11 (floor (p11 * K2) * Kz - Kzo))

    (var ox12 (fract (p12 * K) - Ko))
    (var oy12 (mod7 (floor (p12 * K)) * K - Ko))
    (var oz12 (floor (p12 * K2) * Kz - Kzo))

    (var ox13 (fract (p13 * K) - Ko))
    (var oy13 (mod7 (floor (p13 * K)) * K - Ko))
    (var oz13 (floor (p13 * K2) * Kz - Kzo))

    (var ox21 (fract (p21 * K) - Ko))
    (var oy21 (mod7 (floor (p21 * K)) * K - Ko))
    (var oz21 (floor (p21 * K2) * Kz - Kzo))

    (var ox22 (fract (p22 * K) - Ko))
    (var oy22 (mod7 (floor (p22 * K)) * K - Ko))
    (var oz22 (floor (p22 * K2) * Kz - Kzo))

    (var ox23 (fract (p23 * K) - Ko))
    (var oy23 (mod7 (floor (p23 * K)) * K - Ko))
    (var oz23 (floor (p23 * K2) * Kz - Kzo))

    (var ox31 (fract (p31 * K) - Ko))
    (var oy31 (mod7 (floor (p31 * K)) * K - Ko))
    (var oz31 (floor (p31 * K2) * Kz - Kzo))

    (var ox32 (fract (p32 * K) - Ko))
    (var oy32 (mod7 (floor (p32 * K)) * K - Ko))
    (var oz32 (floor (p32 * K2) * Kz - Kzo))

    (var ox33 (fract (p33 * K) - Ko))
    (var oy33 (mod7 (floor (p33 * K)) * K - Ko))
    (var oz33 (floor (p33 * K2) * Kz - Kzo))

    (var dx11 (Pfx + (jitter * ox11)))
    (var dy11 (Pfy.x + (jitter * oy11)))
    (var dz11 (Pfz.x + (jitter * oz11)))

    (var dx12 (Pfx + (jitter * ox12)))
    (var dy12 (Pfy.x + (jitter * oy12)))
    (var dz12 (Pfz.y + (jitter * oz12)))

    (var dx13 (Pfx + (jitter * ox13)))
    (var dy13 (Pfy.x + (jitter * oy13)))
    (var dz13 (Pfz.z + (jitter * oz13)))

    (var dx21 (Pfx + (jitter * ox21)))
    (var dy21 (Pfy.y + (jitter * oy21)))
    (var dz21 (Pfz.x + (jitter * oz21)))

    (var dx22 (Pfx + (jitter * ox22)))
    (var dy22 (Pfy.y + (jitter * oy22)))
    (var dz22 (Pfz.y + (jitter * oz22)))

    (var dx23 (Pfx + (jitter * ox23)))
    (var dy23 (Pfy.y + (jitter * oy23)))
    (var dz23 (Pfz.z + (jitter * oz23)))

    (var dx31 (Pfx + (jitter * ox31)))
    (var dy31 (Pfy.z + (jitter * oy31)))
    (var dz31 (Pfz.x + (jitter * oz31)))

    (var dx32 (Pfx + (jitter * ox32)))
    (var dy32 (Pfy.z + (jitter * oy32)))
    (var dz32 (Pfz.y + (jitter * oz32)))

    (var dx33 (Pfx + (jitter * ox33)))
    (var dy33 (Pfy.z + (jitter * oy33)))
    (var dz33 (Pfz.z + (jitter * oz33)))

    (var d11 (+ (dx11 * dx11) (dy11 * dy11) (dz11 * dz11)))
    (var d12 (+ (dx12 * dx12) (dy12 * dy12) (dz12 * dz12)))
    (var d13 (+ (dx13 * dx13) (dy13 * dy13) (dz13 * dz13)))
    (var d21 (+ (dx21 * dx21) (dy21 * dy21) (dz21 * dz21)))
    (var d22 (+ (dx22 * dx22) (dy22 * dy22) (dz22 * dz22)))
    (var d23 (+ (dx23 * dx23) (dy23 * dy23) (dz23 * dz23)))
    (var d31 (+ (dx31 * dx31) (dy31 * dy31) (dz31 * dz31)))
    (var d32 (+ (dx32 * dx32) (dy32 * dy32) (dz32 * dz32)))
    (var d33 (+ (dx33 * dx33) (dy33 * dy33) (dz33 * dz33)))
    ,;closing-statements))

(overload-worley3d :float worley
  (var d1 (min (min d11 d12) d13))
  (var d2 (min (min d21 d22) d23))
  (var d3 (min (min d31 d32) d33))
  (var d (min (min d1 d2) d3))
  (return (sqrt (min d))))

(overload-worley3d :vec2 worley2
  (var d1a (min d11 d12))
  (set d12 (max d11 d12))
  (set d11 (min d1a d13))
  (set d13 (max d1a d13))
  (set d12 (min d12 d13))
  (var d2a (min d21 d22))
  (set d22 (max d21 d22))
  (set d21 (min d2a d23))
  (set d23 (max d2a d23))
  (set d22 (min d22 d23))
  (var d3a (min d31 d32))
  (set d32 (max d31 d32))
  (set d31 (min d3a d33))
  (set d33 (max d3a d33))
  (set d32 (min d32 d33))
  (var da (min d11 d21))
  (set d21 (max d11 d21))
  (set d11 (min da d31))
  (set d31 (max da d31))
  (set d11.xy (if (< d11.x d11.y) d11.xy d11.yx))
  (set d11.xz (if (< d11.x d11.z) d11.xz d11.zx))
  (set d12 (min d12 d21))
  (set d12 (min d12 d22))
  (set d12 (min d12 d31))
  (set d12 (min d12 d32))
  (set d11.yz (min d11.yz d12.xy))
  (set d11.y (min d11.y d12.z))
  (set d11.y (min d11.y d11.z))
  (return (sqrt d11.xy)))

(def- worley- worley)
(defn worley
  ````
  Worley noise, also called cellular noise or voronoi noise.
  The input `point` can be a `vec2` or a `vec3`.

  ```example
  (ball 100 | color (worley (p.xy / 30) | vec3))
  ```
  ```example
  (ball 100 | color (worley (p / 30) | vec3))
  ```

  Returns the nearest distance to points distributed randomly within the tiles of a square or cubic grid.
  ````
  [point &opt period]
  (if period
    (worley- (/ point period))
    (worley- point)))

(def- worley2- worley2)
(defn worley2
  ````
  Like `worley`, but returns the nearest distance in `x` and the second-nearest distance in `y`.

  ```example
  (ball 100 | color [(worley2 (p.xy / 30)) 1])
  ```
  ```example
  (ball 100 | color [(worley2 (p / 30)) 1])
  ```
  ````
  [point &opt period]
  (if period
    (worley2- (/ point period))
    (worley2- point)))

(defnamed fbm [octaves f point ?period :?rate :?gain]
  ````
  Run the given noise function over the input multiple times,
  with different periods, and sum the results with some decay.

  You can use this to create interesting procedural patterns
  out of simple noise functions. For example, perlin noise is
  a fairly smooth and "low resoultion" pattern:

  ```example
  (rect 100
  | color (vec3 (perlin q 30 | remap+)))
  ```

  But by summing multiple instances of perlin noise
  sampled with different periods, we can create more detailed
  patterns:

  ```example
  (rect 100
  | color (vec3 (fbm 4 perlin q 30 | remap+)))
  ```

  ```example
  (rect 100
  | color (vec3 (fbm 4 perlin q 30 :gain 0.8 | remap+)))
  ```

  You can use this to create many effects, like clouds or landscapes,
  although note that it is pretty expensive to use in a distance field.

  ```example
  (box [500 5 500]
  | expound (fbm 5 perlin p 50 | remap+) 50 10
  | intersect (ball 200)
  | slow 0.5)
  ```
  ````
  (gl/do "fbm"
    (var octaves (int ,octaves))
    (var point ,(if period (/ point period) point))
    (var gain ,(@or gain 0.5))
    (var rate ,(@or rate 2))
    (var amplitude 1)
    (var result 0)
    (for (var i 0:s) (< i octaves) (++ i)
      (+= result (* amplitude (f point)))
      (*= point rate)
      (*= amplitude gain))
    result))
