(import ./glslisp/src/type :as type)

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

(define type/vec3 'sort3 ["vec3 p"] `
  float smallest = min3(p);
  float largest = max3(p);
  float middlest =
   p.x > smallest && p.x < largest ? p.x :
   p.y > smallest && p.y < largest ? p.y :
   p.z;
  return vec3(smallest, middlest, largest);
  ` 'min3 'max3)

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
