(use ./import)

# TODO: it would be nice to add vec4 inputs to hash2 and hash3

# Taken from David Hoskins: https://www.shadertoy.com/view/4djSRW
(defhelper :float hash [:float v]
  ```
  Return a pseudorandom float. The input can be a float or vector.

  This should return consistent results across GPUs, unlike high-frequency sine functions.
  ```
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

(defhelper :vec2 hash2 [:float v]
  ```
  Return a pseudorandom `vec2`. The input can be a float or vector.

  This should return consistent results across GPUs, unlike high-frequency sine functions.
  ```
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

(defhelper :vec3 hash3 [:float v]
  ```
  Return a pseudorandom `vec3`. The input can be a float or vector.

  This should return consistent results across GPUs, unlike high-frequency sine functions.
  ```
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

(defhelper :vec4 hash4 [:float v]
  ```
  Return a pseudorandom `vec4`. The input can be a float or vector.

  This should return consistent results across GPUs, unlike high-frequency sine functions.
  ```
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
