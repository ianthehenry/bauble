(import ../glslisp/src/type :as type)

(def functions @{})

(defn- define [return-type name params body & deps]
  (def entry {:name name :params params :body body :return-type return-type})
  (set (functions name) {:entry entry :deps deps}))

(define type/float 'max2 ["vec2 p"] `return max(p.x, p.y);`)
(define type/float 'min2 ["vec2 p"] `return min(p.x, p.y);`)
(define type/float 'max3 ["vec3 p"] `return max(p.x, max(p.y, p.z));`)
(define type/float 'min3 ["vec3 p"] `return min(p.x, min(p.y, p.z));`)
(define type/float 'max4 ["vec4 p"] `return max(p.x, max(p.y, max(p.z, p.w)));`)
(define type/float 'min4 ["vec4 p"] `return min(p.x, min(p.y, max(p.z, p.w)));`)

(define type/float 'sum2 ["vec2 p"] `return p.x + p.y;`)
(define type/float 'sum3 ["vec3 p"] `return p.x + p.y + p.z;`)
(define type/float 'sum4 ["vec4 p"] `return p.x + p.y + p.z + p.w;`)
(define type/float 'product2 ["vec2 p"] `return p.x * p.y;`)
(define type/float 'product3 ["vec3 p"] `return p.x * p.y * p.z;`)
(define type/float 'product4 ["vec4 p"] `return p.x * p.y * p.z * p.w;`)

(define type/vec3 'sort3 ["vec3 p"] `
  float smallest = min3(p);
  float largest = max3(p);
  float middlest =
   p.x > smallest && p.x < largest ? p.x :
   p.y > smallest && p.y < largest ? p.y :
   p.z;
  return vec3(smallest, middlest, largest);
  ` 'min3 'max3)

(define type/vec3 'safe_div3 ["vec3 a" "vec3 b"] `
  return mix(a / b, vec3(0.0), equal(b, vec3(0.0)));
  `)

(define type/float 'atan2 ["float y" "float x"] `
  return x == 0.0 ? sign(y) * 0.5 * PI : atan(y, x);
  `)

(define type/vec3 'hsv ["float h" "float s" "float v"] `
  vec3 p = abs(fract(vec3(h, h, h) + vec3(1.0, 2.0 / 3.0, 1.0 / 3.0)) * 6.0 - 3.0);
  return v * mix(vec3(1.0), clamp(p - 1.0, 0.0, 1.0), s);
  `)

(define type/vec3 'hsl ["float h" "float s" "float l"] `
  vec3 p = abs(mod(h * 6.0 + vec3(0.0,4.0,2.0), 6.0) - 3.0);
  return l + s * (clamp(p - 1.0, 0.0, 1.0) - 0.5) * (1.0 - abs(2.0 * l - 1.0));
  `)

(define type/mat3 'rotate_x ["float angle"] `
  float s = sin(angle);
  float c = cos(angle);
  return mat3(
    1.0, 0.0, 0.0,
    0.0, c, -s,
    0.0, s, c);`)

(define type/mat3 'rotate_y ["float angle"] `
  float s = sin(angle);
  float c = cos(angle);
  return mat3(
    c, 0.0, s,
    0.0, 1.0, 0.0,
    -s, 0.0, c);`)

(define type/mat3 'rotate_z ["float angle"] `
  float s = sin(angle);
  float c = cos(angle);
  return mat3(
    c, -s, 0.0,
    s, c, 0.0,
    0.0, 0.0, 1.0);`)

### Lighting stuff ###

(define "LightIncidence" 'cast_light_no_shadow ["vec3 p" "vec3 normal" "vec3 light_position" "vec3 color" "float brightness"] `
  vec3 target = p + MINIMUM_HIT_DISTANCE * normal;
  float target_distance = distance(light_position, target);
  if (target_distance == 0.0) {
    return LightIncidence(vec3(0.0), color * brightness);
  }
  vec3 to_light = (light_position - target) / target_distance;
  return LightIncidence(to_light, brightness * color);`)

(define "LightIncidence" 'cast_light_hard_shadow ["vec3 p" "vec3 normal" "vec3 light_position" "vec3 color" "float brightness"] `
  vec3 target = p + MINIMUM_HIT_DISTANCE * normal;
  float target_distance = distance(light_position, target);
  if (target_distance == 0.0) {
    return LightIncidence(vec3(0.0), color * brightness);
  }
  vec3 to_light = (light_position - target) / target_distance;
  if (brightness == 0.0) {
    return LightIncidence(to_light, vec3(0.0));
  }
  float progress = 0.0;
  for (int i = 0; i < MAX_STEPS; i++) {
    float distance = nearest_distance(light_position - to_light * progress);
    if (distance < MINIMUM_HIT_DISTANCE) {
      if (progress + distance >= target_distance - MINIMUM_HIT_DISTANCE) {
        return LightIncidence(to_light, brightness * color);
      } else {
        return LightIncidence(to_light, vec3(0.0));
      }
    }
    progress += distance;
  }
  return LightIncidence(to_light, vec3(0.0));`)

(define "LightIncidence" 'cast_light_soft_shadow ["vec3 p" "vec3 normal" "vec3 light_position" "vec3 color" "float brightness" "float softness"] `
  if (softness == 0.0) {
    cast_light_hard_shadow(p, normal, light_position, color, brightness);
  }
  vec3 target = p + MINIMUM_HIT_DISTANCE * normal;
  float target_distance = distance(light_position, target);
  if (target_distance == 0.0) {
    return LightIncidence(vec3(0.0), color * brightness);
  }
  vec3 to_light = (light_position - target) / target_distance;
  if (brightness == 0.0) {
    return LightIncidence(to_light, vec3(0.0));
  }
  float in_light = 1.0;
  float sharpness = 1.0 / (softness * softness);
  float last_distance = 1e20;
  float progress = 0.0;
  for (int i = 0; i < MAX_STEPS; i++) {
    float distance = nearest_distance(light_position - to_light * progress);
    if (distance < MINIMUM_HIT_DISTANCE) {
      if (progress + distance >= target_distance - MINIMUM_HIT_DISTANCE) {
        return LightIncidence(to_light, in_light * brightness * color);
      } else {
        return LightIncidence(to_light, vec3(0.0));
      }
    }

    if (distance < last_distance) {
      float intersect_offset = distance * distance / (2.0 * last_distance);
      float intersect_distance = sqrt(distance * distance - intersect_offset * intersect_offset);
      in_light = min(in_light, sharpness * intersect_distance / max(0.0, target_distance - progress - intersect_offset));
    }
    progress += distance;
    last_distance = distance;
  }
  return LightIncidence(to_light, vec3(0.0));` 'cast_light_hard_shadow)

### Hash functions ###

# Taken from David Hoskins: https://www.shadertoy.com/view/4djSRW

(define type/float 'hash11 ["float p"] `
  p = fract(p * 0.1031);
  p *= p + 33.33;
  p *= p + p;
  return fract(p);`)

(define type/float 'hash12 ["vec2 p"] `
  vec3 p3  = fract(vec3(p.xyx) * 0.1031);
  p3 += dot(p3, p3.yzx + 33.33);
  return fract((p3.x + p3.y) * p3.z);`)

(define type/float 'hash13 ["vec3 p3"] `
  p3  = fract(p3 * .1031);
  p3 += dot(p3, p3.zyx + 31.32);
  return fract((p3.x + p3.y) * p3.z);`)

(define type/float 'hash14 ["vec4 p4"] `
  p4 = fract(p4  * vec4(.1031, .1030, .0973, .1099));
  p4 += dot(p4, p4.wzxy+33.33);
  return fract((p4.x + p4.y) * (p4.z + p4.w));`)

(define type/vec2 'hash21 ["float p"] `
  vec3 p3 = fract(vec3(p) * vec3(.1031, .1030, .0973));
  p3 += dot(p3, p3.yzx + 33.33);
  return fract((p3.xx+p3.yz)*p3.zy);`)

(define type/vec2 'hash22 ["vec2 p"] `
  vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
  p3 += dot(p3, p3.yzx+33.33);
  return fract((p3.xx+p3.yz)*p3.zy);`)

(define type/vec2 'hash23 ["vec3 p3"] `
  p3 = fract(p3 * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yzx+33.33);
    return fract((p3.xx+p3.yz)*p3.zy);`)

(define type/vec3 'hash31 ["float p"] `
   vec3 p3 = fract(vec3(p) * vec3(.1031, .1030, .0973));
   p3 += dot(p3, p3.yzx+33.33);
   return fract((p3.xxy+p3.yzz)*p3.zyx); `)

(define type/vec3 'hash32 ["vec2 p"] `
  vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
  p3 += dot(p3, p3.yxz+33.33);
  return fract((p3.xxy+p3.yzz)*p3.zyx);`)

(define type/vec3 'hash33 ["vec3 p3"] `
  p3 = fract(p3 * vec3(.1031, .1030, .0973));
  p3 += dot(p3, p3.yxz+33.33);
  return fract((p3.xxy + p3.yxx)*p3.zyx);`)

(define type/vec4 'hash41 ["float p"] `
  vec4 p4 = fract(vec4(p) * vec4(.1031, .1030, .0973, .1099));
  p4 += dot(p4, p4.wzxy+33.33);
  return fract((p4.xxyz+p4.yzzw)*p4.zywx);`)

(define type/vec4 'hash42 ["vec2 p"] `
  vec4 p4 = fract(vec4(p.xyxy) * vec4(.1031, .1030, .0973, .1099));
  p4 += dot(p4, p4.wzxy+33.33);
  return fract((p4.xxyz+p4.yzzw)*p4.zywx);`)

(define type/vec4 'hash43 ["vec3 p"] `
  vec4 p4 = fract(vec4(p.xyzx)  * vec4(.1031, .1030, .0973, .1099));
  p4 += dot(p4, p4.wzxy+33.33);
  return fract((p4.xxyz+p4.yzzw)*p4.zywx);`)

(define type/vec4 'hash44 ["vec4 p4"] `
  p4 = fract(p4  * vec4(.1031, .1030, .0973, .1099));
  p4 += dot(p4, p4.wzxy+33.33);
  return fract((p4.xxyz+p4.yzzw)*p4.zywx);`)

### Noise stuff ###

# Taken from https://stegu.github.io/webgl-noise/webdemo/

(define type/vec3 'mod289_3 ["vec3 x"] `
  return x - floor(x * (1.0 / 289.0)) * 289.0;`)

(define type/vec4 'mod289_4 ["vec4 x"] `
  return x - floor(x * (1.0 / 289.0)) * 289.0;`)

(define type/vec4 'permute4 ["vec4 x"] `
  return mod289_4(((x * 34.0) + 10.0) * x);` 'mod289_4)

(define type/vec4 'taylor_inv_sqrt4 ["vec4 r"] `
  return 1.79284291400159 - 0.85373472095314 * r;`)

(define type/vec2 'fade2 ["vec2 t"] `
  return t*t*t*(t * (t * 6.0 - 15.0) + 10.0);`)

(define type/vec3 'fade3 ["vec3 t"] `
  return t*t*t*(t * (t * 6.0 - 15.0) + 10.0);`)

(define type/vec4 'fade4 ["vec4 t"] `
  return t*t*t*(t * (t * 6.0 - 15.0) + 10.0);`)

(define type/float 'perlin2 ["vec2 P"] `
  vec4 Pi = floor(P.xyxy) + vec4(0.0, 0.0, 1.0, 1.0);
  vec4 Pf = fract(P.xyxy) - vec4(0.0, 0.0, 1.0, 1.0);
  Pi = mod289_4(Pi); // To avoid truncation effects in permutation
  vec4 ix = Pi.xzxz;
  vec4 iy = Pi.yyww;
  vec4 fx = Pf.xzxz;
  vec4 fy = Pf.yyww;

  vec4 i = permute4(permute4(ix) + iy);

  vec4 gx = fract(i * (1.0 / 41.0)) * 2.0 - 1.0 ;
  vec4 gy = abs(gx) - 0.5 ;
  vec4 tx = floor(gx + 0.5);
  gx = gx - tx;

  vec2 g00 = vec2(gx.x,gy.x);
  vec2 g10 = vec2(gx.y,gy.y);
  vec2 g01 = vec2(gx.z,gy.z);
  vec2 g11 = vec2(gx.w,gy.w);

  vec4 norm = taylor_inv_sqrt4(vec4(dot(g00, g00), dot(g01, g01), dot(g10, g10), dot(g11, g11)));
  g00 *= norm.x;
  g01 *= norm.y;
  g10 *= norm.z;
  g11 *= norm.w;

  float n00 = dot(g00, vec2(fx.x, fy.x));
  float n10 = dot(g10, vec2(fx.y, fy.y));
  float n01 = dot(g01, vec2(fx.z, fy.z));
  float n11 = dot(g11, vec2(fx.w, fy.w));

  vec2 fade_xy = fade2(Pf.xy);
  vec2 n_x = mix(vec2(n00, n01), vec2(n10, n11), fade_xy.x);
  float n_xy = mix(n_x.x, n_x.y, fade_xy.y);
  return 2.3 * n_xy;` 'fade2 'taylor_inv_sqrt4 'mod289_4 'permute4)

(define type/float 'perlin3 ["vec3 P"] `
  vec3 Pi0 = floor(P); // Integer part for indexing
  vec3 Pi1 = Pi0 + vec3(1.0); // Integer part + 1
  Pi0 = mod289_3(Pi0);
  Pi1 = mod289_3(Pi1);
  vec3 Pf0 = fract(P); // Fractional part for interpolation
  vec3 Pf1 = Pf0 - vec3(1.0); // Fractional part - 1.0
  vec4 ix = vec4(Pi0.x, Pi1.x, Pi0.x, Pi1.x);
  vec4 iy = vec4(Pi0.yy, Pi1.yy);
  vec4 iz0 = Pi0.zzzz;
  vec4 iz1 = Pi1.zzzz;

  vec4 ixy = permute4(permute4(ix) + iy);
  vec4 ixy0 = permute4(ixy + iz0);
  vec4 ixy1 = permute4(ixy + iz1);

  vec4 gx0 = ixy0 * (1.0 / 7.0);
  vec4 gy0 = fract(floor(gx0) * (1.0 / 7.0)) - 0.5;
  gx0 = fract(gx0);
  vec4 gz0 = vec4(0.5) - abs(gx0) - abs(gy0);
  vec4 sz0 = step(gz0, vec4(0.0));
  gx0 -= sz0 * (step(0.0, gx0) - 0.5);
  gy0 -= sz0 * (step(0.0, gy0) - 0.5);

  vec4 gx1 = ixy1 * (1.0 / 7.0);
  vec4 gy1 = fract(floor(gx1) * (1.0 / 7.0)) - 0.5;
  gx1 = fract(gx1);
  vec4 gz1 = vec4(0.5) - abs(gx1) - abs(gy1);
  vec4 sz1 = step(gz1, vec4(0.0));
  gx1 -= sz1 * (step(0.0, gx1) - 0.5);
  gy1 -= sz1 * (step(0.0, gy1) - 0.5);

  vec3 g000 = vec3(gx0.x,gy0.x,gz0.x);
  vec3 g100 = vec3(gx0.y,gy0.y,gz0.y);
  vec3 g010 = vec3(gx0.z,gy0.z,gz0.z);
  vec3 g110 = vec3(gx0.w,gy0.w,gz0.w);
  vec3 g001 = vec3(gx1.x,gy1.x,gz1.x);
  vec3 g101 = vec3(gx1.y,gy1.y,gz1.y);
  vec3 g011 = vec3(gx1.z,gy1.z,gz1.z);
  vec3 g111 = vec3(gx1.w,gy1.w,gz1.w);

  vec4 norm0 = taylor_inv_sqrt4(vec4(dot(g000, g000), dot(g010, g010), dot(g100, g100), dot(g110, g110)));
  g000 *= norm0.x;
  g010 *= norm0.y;
  g100 *= norm0.z;
  g110 *= norm0.w;
  vec4 norm1 = taylor_inv_sqrt4(vec4(dot(g001, g001), dot(g011, g011), dot(g101, g101), dot(g111, g111)));
  g001 *= norm1.x;
  g011 *= norm1.y;
  g101 *= norm1.z;
  g111 *= norm1.w;

  float n000 = dot(g000, Pf0);
  float n100 = dot(g100, vec3(Pf1.x, Pf0.yz));
  float n010 = dot(g010, vec3(Pf0.x, Pf1.y, Pf0.z));
  float n110 = dot(g110, vec3(Pf1.xy, Pf0.z));
  float n001 = dot(g001, vec3(Pf0.xy, Pf1.z));
  float n101 = dot(g101, vec3(Pf1.x, Pf0.y, Pf1.z));
  float n011 = dot(g011, vec3(Pf0.x, Pf1.yz));
  float n111 = dot(g111, Pf1);

  vec3 fade_xyz = fade3(Pf0);
  vec4 n_z = mix(vec4(n000, n100, n010, n110), vec4(n001, n101, n011, n111), fade_xyz.z);
  vec2 n_yz = mix(n_z.xy, n_z.zw, fade_xyz.y);
  float n_xyz = mix(n_yz.x, n_yz.y, fade_xyz.x);
  return 2.2 * n_xyz;

` 'fade3 'taylor_inv_sqrt4 'mod289_3 'permute4)

(define type/float 'perlin4 ["vec4 P"] `
  vec4 Pi0 = floor(P); // Integer part for indexing
  vec4 Pi1 = Pi0 + 1.0; // Integer part + 1
  Pi0 = mod289_4(Pi0);
  Pi1 = mod289_4(Pi1);
  vec4 Pf0 = fract(P); // Fractional part for interpolation
  vec4 Pf1 = Pf0 - 1.0; // Fractional part - 1.0
  vec4 ix = vec4(Pi0.x, Pi1.x, Pi0.x, Pi1.x);
  vec4 iy = vec4(Pi0.yy, Pi1.yy);
  vec4 iz0 = vec4(Pi0.zzzz);
  vec4 iz1 = vec4(Pi1.zzzz);
  vec4 iw0 = vec4(Pi0.wwww);
  vec4 iw1 = vec4(Pi1.wwww);

  vec4 ixy = permute4(permute4(ix) + iy);
  vec4 ixy0 = permute4(ixy + iz0);
  vec4 ixy1 = permute4(ixy + iz1);
  vec4 ixy00 = permute4(ixy0 + iw0);
  vec4 ixy01 = permute4(ixy0 + iw1);
  vec4 ixy10 = permute4(ixy1 + iw0);
  vec4 ixy11 = permute4(ixy1 + iw1);

  vec4 gx00 = ixy00 * (1.0 / 7.0);
  vec4 gy00 = floor(gx00) * (1.0 / 7.0);
  vec4 gz00 = floor(gy00) * (1.0 / 6.0);
  gx00 = fract(gx00) - 0.5;
  gy00 = fract(gy00) - 0.5;
  gz00 = fract(gz00) - 0.5;
  vec4 gw00 = vec4(0.75) - abs(gx00) - abs(gy00) - abs(gz00);
  vec4 sw00 = step(gw00, vec4(0.0));
  gx00 -= sw00 * (step(0.0, gx00) - 0.5);
  gy00 -= sw00 * (step(0.0, gy00) - 0.5);

  vec4 gx01 = ixy01 * (1.0 / 7.0);
  vec4 gy01 = floor(gx01) * (1.0 / 7.0);
  vec4 gz01 = floor(gy01) * (1.0 / 6.0);
  gx01 = fract(gx01) - 0.5;
  gy01 = fract(gy01) - 0.5;
  gz01 = fract(gz01) - 0.5;
  vec4 gw01 = vec4(0.75) - abs(gx01) - abs(gy01) - abs(gz01);
  vec4 sw01 = step(gw01, vec4(0.0));
  gx01 -= sw01 * (step(0.0, gx01) - 0.5);
  gy01 -= sw01 * (step(0.0, gy01) - 0.5);

  vec4 gx10 = ixy10 * (1.0 / 7.0);
  vec4 gy10 = floor(gx10) * (1.0 / 7.0);
  vec4 gz10 = floor(gy10) * (1.0 / 6.0);
  gx10 = fract(gx10) - 0.5;
  gy10 = fract(gy10) - 0.5;
  gz10 = fract(gz10) - 0.5;
  vec4 gw10 = vec4(0.75) - abs(gx10) - abs(gy10) - abs(gz10);
  vec4 sw10 = step(gw10, vec4(0.0));
  gx10 -= sw10 * (step(0.0, gx10) - 0.5);
  gy10 -= sw10 * (step(0.0, gy10) - 0.5);

  vec4 gx11 = ixy11 * (1.0 / 7.0);
  vec4 gy11 = floor(gx11) * (1.0 / 7.0);
  vec4 gz11 = floor(gy11) * (1.0 / 6.0);
  gx11 = fract(gx11) - 0.5;
  gy11 = fract(gy11) - 0.5;
  gz11 = fract(gz11) - 0.5;
  vec4 gw11 = vec4(0.75) - abs(gx11) - abs(gy11) - abs(gz11);
  vec4 sw11 = step(gw11, vec4(0.0));
  gx11 -= sw11 * (step(0.0, gx11) - 0.5);
  gy11 -= sw11 * (step(0.0, gy11) - 0.5);

  vec4 g0000 = vec4(gx00.x,gy00.x,gz00.x,gw00.x);
  vec4 g1000 = vec4(gx00.y,gy00.y,gz00.y,gw00.y);
  vec4 g0100 = vec4(gx00.z,gy00.z,gz00.z,gw00.z);
  vec4 g1100 = vec4(gx00.w,gy00.w,gz00.w,gw00.w);
  vec4 g0010 = vec4(gx10.x,gy10.x,gz10.x,gw10.x);
  vec4 g1010 = vec4(gx10.y,gy10.y,gz10.y,gw10.y);
  vec4 g0110 = vec4(gx10.z,gy10.z,gz10.z,gw10.z);
  vec4 g1110 = vec4(gx10.w,gy10.w,gz10.w,gw10.w);
  vec4 g0001 = vec4(gx01.x,gy01.x,gz01.x,gw01.x);
  vec4 g1001 = vec4(gx01.y,gy01.y,gz01.y,gw01.y);
  vec4 g0101 = vec4(gx01.z,gy01.z,gz01.z,gw01.z);
  vec4 g1101 = vec4(gx01.w,gy01.w,gz01.w,gw01.w);
  vec4 g0011 = vec4(gx11.x,gy11.x,gz11.x,gw11.x);
  vec4 g1011 = vec4(gx11.y,gy11.y,gz11.y,gw11.y);
  vec4 g0111 = vec4(gx11.z,gy11.z,gz11.z,gw11.z);
  vec4 g1111 = vec4(gx11.w,gy11.w,gz11.w,gw11.w);

  vec4 norm00 = taylor_inv_sqrt4(vec4(dot(g0000, g0000), dot(g0100, g0100), dot(g1000, g1000), dot(g1100, g1100)));
  g0000 *= norm00.x;
  g0100 *= norm00.y;
  g1000 *= norm00.z;
  g1100 *= norm00.w;

  vec4 norm01 = taylor_inv_sqrt4(vec4(dot(g0001, g0001), dot(g0101, g0101), dot(g1001, g1001), dot(g1101, g1101)));
  g0001 *= norm01.x;
  g0101 *= norm01.y;
  g1001 *= norm01.z;
  g1101 *= norm01.w;

  vec4 norm10 = taylor_inv_sqrt4(vec4(dot(g0010, g0010), dot(g0110, g0110), dot(g1010, g1010), dot(g1110, g1110)));
  g0010 *= norm10.x;
  g0110 *= norm10.y;
  g1010 *= norm10.z;
  g1110 *= norm10.w;

  vec4 norm11 = taylor_inv_sqrt4(vec4(dot(g0011, g0011), dot(g0111, g0111), dot(g1011, g1011), dot(g1111, g1111)));
  g0011 *= norm11.x;
  g0111 *= norm11.y;
  g1011 *= norm11.z;
  g1111 *= norm11.w;

  float n0000 = dot(g0000, Pf0);
  float n1000 = dot(g1000, vec4(Pf1.x, Pf0.yzw));
  float n0100 = dot(g0100, vec4(Pf0.x, Pf1.y, Pf0.zw));
  float n1100 = dot(g1100, vec4(Pf1.xy, Pf0.zw));
  float n0010 = dot(g0010, vec4(Pf0.xy, Pf1.z, Pf0.w));
  float n1010 = dot(g1010, vec4(Pf1.x, Pf0.y, Pf1.z, Pf0.w));
  float n0110 = dot(g0110, vec4(Pf0.x, Pf1.yz, Pf0.w));
  float n1110 = dot(g1110, vec4(Pf1.xyz, Pf0.w));
  float n0001 = dot(g0001, vec4(Pf0.xyz, Pf1.w));
  float n1001 = dot(g1001, vec4(Pf1.x, Pf0.yz, Pf1.w));
  float n0101 = dot(g0101, vec4(Pf0.x, Pf1.y, Pf0.z, Pf1.w));
  float n1101 = dot(g1101, vec4(Pf1.xy, Pf0.z, Pf1.w));
  float n0011 = dot(g0011, vec4(Pf0.xy, Pf1.zw));
  float n1011 = dot(g1011, vec4(Pf1.x, Pf0.y, Pf1.zw));
  float n0111 = dot(g0111, vec4(Pf0.x, Pf1.yzw));
  float n1111 = dot(g1111, Pf1);

  vec4 fade_xyzw = fade4(Pf0);
  vec4 n_0w = mix(vec4(n0000, n1000, n0100, n1100), vec4(n0001, n1001, n0101, n1101), fade_xyzw.w);
  vec4 n_1w = mix(vec4(n0010, n1010, n0110, n1110), vec4(n0011, n1011, n0111, n1111), fade_xyzw.w);
  vec4 n_zw = mix(n_0w, n_1w, fade_xyzw.z);
  vec2 n_yzw = mix(n_zw.xy, n_zw.zw, fade_xyzw.y);
  float n_xyzw = mix(n_yzw.x, n_yzw.y, fade_xyzw.x);
  return 2.2 * n_xyzw;
` 'mod289_4 'fade4 'permute4 'taylor_inv_sqrt4)
