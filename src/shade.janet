(def comp-state-proto @{
  :function (fn [self key name-base args parameters body]
    (defn invocation [name] (string/format "%s(%s)" name (string/join args ", ")))
    # if we've already compiled this node, return it
    (if-let [cached ((self :cache) key)]
      (invocation (cached :name))
      (do
        (def name-index (get (self :names) name-base 0))
        (set ((self :names) name-base) (inc name-index))
        (def name (string/format "%s_%d" name-base name-index))
        (def entry {:name name :parameters parameters :body body})

        (array/push (self :functions) entry)
        (set ((self :cache) key) entry)
        (invocation name))))
  :sdf-3d (fn [self key name-base coord get-body]
    (:function self key name-base [coord] ["vec3 p"] (get-body "p")))
  :sdf-2d (fn [self key name-base coord get-body]
    (:function self key name-base [coord] ["vec2 p"] (get-body "p")))
  })

(defn compile-function [{:name name :parameters parameters :body body}]
  (string/format "float %s(%s) {\n%s\n}" name (string/join parameters ", ") body))

# TODO: this is duplicated
(defn- float [n]
  (if (int? n) (string n ".0") (string n)))

(defn make-fragment-shader [expr camera]
  (def comp-state @{:names @{} :cache @{} :functions @[]})
  (table/setproto comp-state comp-state-proto)
  (def top-level-expr (:compile expr comp-state "p"))
  (def function-defs (string/join (map compile-function (comp-state :functions)) "\n"))
  (set-fragment-shader
    (string `
#version 300 es
precision highp float;

const int MAX_STEPS = 256;
const float MINIMUM_HIT_DISTANCE = 0.5;
const float NORMAL_OFFSET = 0.005;
const float MAXIMUM_TRACE_DISTANCE = 4096.0;

struct Surface {
  float distance;
  vec3 color;
};
`
function-defs
`
Surface distance_field(vec3 p) {
  return Surface(`top-level-expr`, vec3(1.0));
}

vec3 calculate_normal(vec3 p) {
  const vec3 step = vec3(NORMAL_OFFSET, 0.0, 0.0);

  return normalize(vec3(
    distance_field(p + step.xyy).distance - distance_field(p - step.xyy).distance,
    distance_field(p + step.yxy).distance - distance_field(p - step.yxy).distance,
    distance_field(p + step.yyx).distance - distance_field(p - step.yyx).distance
  ));
}

float cast_light(vec3 destination, vec3 light, float radius) {
  vec3 direction = normalize(light - destination);

  float stop_at = distance(light, destination);

  float light_brightness = 1.0 - (stop_at / radius);
  if (light_brightness <= 0.0) {
    return 0.0;
  }

  float in_light = 1.0;
  float sharpness = 8.0;

  float last_distance = 1e20;
  float progress = MINIMUM_HIT_DISTANCE;
  for (int i = 0; i < MAX_STEPS; i++) {
    vec3 p = destination + progress * direction;
    float distance = distance_field(p).distance;

    if (distance < MINIMUM_HIT_DISTANCE) {
     if (progress + distance + MINIMUM_HIT_DISTANCE > stop_at - MINIMUM_HIT_DISTANCE) {
       // we hit the light
       return in_light * light_brightness;
     } else {
       // we hit something else
       return 0.0;
     }
    }

    float intersect_offset = distance * distance / (2.0 * last_distance);
    float intersect_distance = sqrt(distance * distance - intersect_offset * intersect_offset);
    if (distance < last_distance) {
      in_light = min(in_light, sharpness * intersect_distance / max(0.0, progress - intersect_offset));
    }
    progress += distance;
    last_distance = distance;
  }

  return in_light * light_brightness;
}

vec3 fog_color = vec3(0.15);

vec3 march(vec3 ray_origin, vec3 ray_direction) {
  float distance = 0.0;

  for (int i = 0; i < MAX_STEPS; i++) {
    vec3 p = ray_origin + distance * ray_direction;

    Surface nearest = distance_field(p);

    if (nearest.distance < MINIMUM_HIT_DISTANCE) {
      // a useful debug view
      // return vec3(float(i) / float(MAX_STEPS));

      vec3 hit = p + nearest.distance * ray_direction;
      vec3 normal = calculate_normal(hit);

      float depth = length(hit - ray_origin);
      vec3 normal_color = 0.5 * (normal + vec3(1.0));

      return mix(normal_color, fog_color, depth / MAXIMUM_TRACE_DISTANCE);

      vec3 light1 = vec3(200.0, 0.0, 200.0);
      vec3 light2 = vec3(0.0, -200.0, 100.0);

      float brightness1 = cast_light(hit + MINIMUM_HIT_DISTANCE * normal, light1, 500.0);
      float brightness2 = cast_light(hit + MINIMUM_HIT_DISTANCE * normal, light2, 500.0);

      float diffuse1 = brightness1 * max(0.0, dot(normal, normalize(light1 - hit)));
      
      float diffuse2 = 0.25 * brightness2 * max(0.0, dot(normal, normalize(light2 - hit)));

      float ambient = 0.2;

      float col = 0.0;
      //mat3 mat = align_matrix(normal);
      //for (int i = 0; i < 12; ++i) {
      //  vec3 m = mat * ambient_occlusion_dir[i];
      //  // col += ambient_occlusion(p, m) * (0.5 + 0.5 * dot(m, vec3(0.0, 0.0, 1.0)));
      //  col += ambient_occlusion(p, m);
      //}
      //col = clamp(pow(col / 5.0, 0.5), 0.0, 1.0);
      col = 1.0;

      // return 0.5 * (normal + 1.0);

      return nearest.color * clamp((diffuse1 + diffuse2 + ambient * col), 0.0, 1.0);
    }

    if (distance > MAXIMUM_TRACE_DISTANCE) {
      return fog_color;
    }
    distance += nearest.distance;
  }
  return vec3(1.0, 0.1, 0.1);
}

mat4 view_matrix(vec3 eye, vec3 target, vec3 up) {
  vec3 f = normalize(target - eye);
  vec3 s = normalize(cross(f, up));
  vec3 u = cross(s, f);
  return mat4(
      vec4(s, 0.0),
      vec4(f, 0.0),
      vec4(u, 0.0),
      vec4(0.0, 0.0, 0.0, 1.0)
  );
}

out vec4 frag_color;

const float PI = 3.14159265359;
const float DEG_TO_RAD = PI / 180.0;

vec3 ray_dir(float fov, vec2 size, vec2 pos) {
  vec2 xy = pos - size * 0.5;

  float cot_half_fov = tan((90.0 - fov * 0.5) * DEG_TO_RAD);
  float z = size.y * 0.5 * cot_half_fov;
  
  return normalize(vec3(xy, -z));
}

mat3 rotate_xy(vec2 angle) {
  vec2 c = cos(angle);
  vec2 s = sin(angle);
  
  return mat3(
    c.y      ,  0.0, -s.y,
    s.y * s.x,  c.x,  c.y * s.x,
    s.y * c.x, -s.x,  c.y * c.x
  );
}

void main() {
  const float zoom = 2.0;
  const float gamma = 2.2;
  const vec2 resolution = vec2(1024.0, 1024.0);

  vec2 rotation = vec2(`(float (camera :x))`, `(float (camera :y))`);
  mat3 camera_matrix = rotate_xy(rotation);

  vec3 dir = ray_dir(45.0, resolution, gl_FragCoord.xy);  
  vec3 eye = vec3(0.0, 0.0, `(float (* 256 (camera :zoom)))`);
  dir = camera_matrix * dir;
  eye = camera_matrix * eye;

  vec3 color = march(eye, dir);

  //if (int(gl_FragCoord.x + gl_FragCoord.y) % 2 == 0) {
  // color = floor(color * 16.0f) / 16.0f;
  //}

  frag_color = vec4(pow(color, vec3(1.0 / gamma)), 1.0);
}
`)))

# surely I can do better
(defn is-good-value? [value]
  (and (table? value)
       (not (nil? (value :compile)))))

(fiber/new (fn []
  (while true
    (let [[expr camera] (yield)]
      (if (is-good-value? expr)
        (try
          (make-fragment-shader expr camera)
          ([err fiber] 
            (debug/stacktrace fiber err)))
        (eprint "cannot compile " expr))))))


