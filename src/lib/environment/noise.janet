(use ./import)

(defhelper- :vec3 mod289_3 [:vec3 x]
  (return (x - (floor (x * (/ 289)) * 289))))

(defhelper- :vec4 mod289_4 [:vec4 x]
  (return (x - (floor (x * (/ 289)) * 289))))

(defhelper- :vec4 permute4 [:vec4 x]
  (return (x * 34 + 10 * x | mod289_4)))

(defhelper- :vec4 taylor_inv_sqrt4 [:vec4 r]
  (return (1.79284291400159 - (0.85373472095314 * r))))

(defhelper- :vec2 fade2 [:vec2 t]
  (return (* t t t (t * (t * 6 - 15) + 10))))

(defhelper- :vec3 fade3 [:vec3 t]
  (return (* t t t (t * (t * 6 - 15) + 10))))

(defhelper- :vec4 fade4 [:vec4 t]
  (return (* t t t (t * (t * 6 - 15) + 10))))

(defhelper :float perlin2 [:vec2 P]
  "TODO"
  (var Pi ((floor P.xyxy) + [0 0 1 1]))
  (var Pf ((fract P.xyxy) - [0 0 1 1]))
  (set Pi (mod289_4 Pi)) # To avoid truncation effects in permutation
  (var ix Pi.xzxz)
  (var iy Pi.yyww)
  (var fx Pf.xzxz)
  (var fy Pf.yyww)

  (var i (permute4 (permute4 ix + iy)))

  (var gx ((fract (i * (/ 41))) * 2 - 1))
  (var gy (abs gx - 0.5))
  (var tx (floor (gx + 0.5)))
  (set gx (gx - tx))

  (var g00 [gx.x gy.x])
  (var g10 [gx.y gy.y])
  (var g01 [gx.z gy.z])
  (var g11 [gx.w gy.w])

  (var norm (taylor_inv_sqrt4 [(dot g00 g00) (dot g01 g01) (dot g10 g10) (dot g11 g11)]))
  (*= g00 norm.x)
  (*= g01 norm.y)
  (*= g10 norm.z)
  (*= g11 norm.w)

  (var n00 (dot g00 [fx.x fy.x]))
  (var n10 (dot g10 [fx.y fy.y]))
  (var n01 (dot g01 [fx.z fy.z]))
  (var n11 (dot g11 [fx.w fy.w]))

  (var fade_xy (fade2 Pf.xy))
  (var n_x (mix [n00 n01] [n10 n11] fade_xy.x))
  (var n_xy (mix n_x.x n_x.y fade_xy.y))
  (return (2.3 * n_xy)))

(defhelper :float perlin3 [:vec3 P]
  "TODO"
  (var Pi0 (floor P)) # Integer part for indexing
  (var Pi1 (Pi0 + 1))
  (var Pi0 (mod289_3 Pi0))
  (var Pi1 (mod289_3 Pi1))
  (var Pf0 (fract P)) # Fractional part for interpolation
  (var Pf1 (Pf0 - 1))
  (var ix [Pi0.x Pi1.x Pi0.x Pi1.x])
  (var iy [Pi0.yy Pi1.yy])
  (var iz0 Pi0.zzzz)
  (var iz1 Pi1.zzzz)

  (var ixy (permute4 ix + iy | permute4))
  (var ixy0 (ixy + iz0 | permute4))
  (var ixy1 (ixy + iz1 | permute4))

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

  (var norm0 (taylor_inv_sqrt4 [(dot g000 g000) (dot g010 g010) (dot g100 g100) (dot g110 g110)]))
  (*= g000 norm0.x)
  (*= g010 norm0.y)
  (*= g100 norm0.z)
  (*= g110 norm0.w)
  (var norm1 (taylor_inv_sqrt4 [(dot g001 g001) (dot g011 g011) (dot g101 g101) (dot g111 g111)]))
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

  (var fade_xyz (fade3 Pf0))
  (var n_z (mix [n000 n100 n010 n110] [n001 n101 n011 n111] fade_xyz.z))
  (var n_yz (mix n_z.xy n_z.zw fade_xyz.y))
  (var n_xyz (mix n_yz.x n_yz.y fade_xyz.x))
  (return (2.2 * n_xyz)))

(defhelper :float perlin4 [:vec4 P]
  "TODO"
  (var Pi0 (floor P))
  (var Pi1 (Pi0 + 1))
  (set Pi0 (mod289_4 Pi0))
  (set Pi1 (mod289_4 Pi1))
  (var Pf0 (fract P))
  (var Pf1 (Pf0 - 1))
  (var ix [Pi0.x Pi1.x Pi0.x Pi1.x])
  (var iy [Pi0.yy Pi1.yy])
  (var iz0 Pi0.zzzz)
  (var iz1 Pi1.zzzz)
  (var iw0 Pi0.wwww)
  (var iw1 Pi1.wwww)

  (var ixy (permute4 ix + iy | permute4))
  (var ixy0 (ixy + iz0 | permute4))
  (var ixy1 (ixy + iz1 | permute4))
  (var ixy00 (ixy0 + iw0 | permute4))
  (var ixy01 (ixy0 + iw1 | permute4))
  (var ixy10 (ixy1 + iw0 | permute4))
  (var ixy11 (ixy1 + iw1 | permute4))

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

  (var norm00 (taylor_inv_sqrt4 [(dot g0000 g0000) (dot g0100 g0100) (dot g1000 g1000) (dot g1100 g1100)]))
  (*= g0000 norm00.x)
  (*= g0100 norm00.y)
  (*= g1000 norm00.z)
  (*= g1100 norm00.w)

  (var norm01 (taylor_inv_sqrt4 [(dot g0001 g0001) (dot g0101 g0101) (dot g1001 g1001) (dot g1101 g1101)]))
  (*= g0001 norm01.x)
  (*= g0101 norm01.y)
  (*= g1001 norm01.z)
  (*= g1101 norm01.w)

  (var norm10 (taylor_inv_sqrt4 [(dot g0010 g0010) (dot g0110 g0110) (dot g1010 g1010) (dot g1110 g1110)]))
  (*= g0010 norm10.x)
  (*= g0110 norm10.y)
  (*= g1010 norm10.z)
  (*= g1110 norm10.w)

  (var norm11 (taylor_inv_sqrt4 [(dot g0011 g0011) (dot g0111 g0111) (dot g1011 g1011) (dot g1111 g1111)]))
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

  (var fade_xyzw (fade4 Pf0))
  (var n_0w (mix [n0000 n1000 n0100 n1100] [n0001 n1001 n0101 n1101] fade_xyzw.w))
  (var n_1w (mix [n0010 n1010 n0110 n1110] [n0011 n1011 n0111 n1111] fade_xyzw.w))
  (var n_zw (mix n_0w n_1w fade_xyzw.z))
  (var n_yzw (mix n_zw.xy n_zw.zw fade_xyzw.y))
  (var n_xyzw (mix n_yzw.x n_yzw.y fade_xyzw.x))
  (return (2.2 * n_xyzw)))

