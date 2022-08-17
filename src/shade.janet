# TODO: performance question: would the cached lookup here
# perform better if shapes were tables instead of structs?
# because we'd get pointer lookup instead of value lookup?
# then again, would that be *worse* because we'd end up
# generating identical functions for surface operations
# in some cases?
(def comp-state-proto @{
  :function (fn [self return-type key name-base args parameters body]
    (defn invocation [name] (string/format "%s(%s)" name (string/join args ", ")))
    # if we've already compiled this node, return it
    (if-let [cached ((self :cache) key)]
      (invocation (cached :name))
      (do
        (def name-index (get (self :names) name-base 0))
        (set ((self :names) name-base) (inc name-index))
        (def name (string/format "%s_%d" name-base name-index))
        (def entry {:name name :parameters parameters :body body :return-type return-type})

        (array/push (self :functions) entry)
        (set ((self :cache) key) entry)
        (invocation name))))
  :sdf-3d (fn [self key name-base coord get-body]
    (:function self "float" key name-base [coord] ["vec3 p"] (get-body "p")))
  :sdf-2d (fn [self key name-base coord get-body]
    (:function self "float" key name-base [coord] ["vec2 p"] (get-body "p")))
  })

(defn compile-function [{:name name :parameters parameters :body body :return-type return-type}]
  (string/format "%s %s(%s) {\n%s\n}" return-type name (string/join parameters ", ") body))

# TODO: this is duplicated
(defn- float [n]
  (if (int? n) (string n ".0") (string n)))

(defn make-fragment-shader [expr camera]
  (def comp-state @{:names @{} :cache @{} :functions @[]})
  (table/setproto comp-state comp-state-proto)
  (def top-level-shape (:compile expr comp-state "p"))
  (def top-level-color (:surface expr comp-state "p"))
  (def function-defs (string/join (map compile-function (comp-state :functions)) "\n"))
  (set-fragment-shader
    (string `
#version 300 es
precision highp float;

const int MAX_STEPS = 256;
const float CAMERA_RAY_HIT_SCALE = 0.001;
const float MINIMUM_SHADOW_HIT_DISTANCE = 0.1;
const float NORMAL_OFFSET = 0.005;
const float MAXIMUM_TRACE_DISTANCE = 8.0 * 1024.0;

float min3(vec3 p) {
  return min(p.x, min(p.y, p.z));
}

float max3(vec3 p) {
  return max(p.x, max(p.y, p.z));
}

struct Light {
  vec3 position;
  vec3 color;
  float radius;
};

// TODO: obviously these should be user-customizable,
// but it's kind of a whole thing and I'm working on
// it okay
const Light lights[3] = Light[3](
  Light(vec3(512.0, 512.0, 256.0), vec3(1.0), 2048.0),
  Light(vec3(0.0, 0.0, -512.0), vec3(0.0), 2048.0),
  Light(vec3(0.0, 0.0, 256.0), vec3(0.0), 2048.0)
);

vec3 calculate_normal(vec3 p);
float cast_light(vec3 destination, vec3 light, float radius);

`
function-defs
`
float nearest_distance(vec3 p) {
  return `top-level-shape`;
}

vec3 nearest_color(vec3 p, vec3 camera) {
  vec3 world_p = p;
  vec3 normal = calculate_normal(p);
  // Array initialization syntax doesn't work
  // on Google Pixel 6a (and maybe other Android
  // phones, not tested).
  float light_intensities[3];
  light_intensities[0] = cast_light(p + 2.0 * MINIMUM_SHADOW_HIT_DISTANCE * normal, lights[0].position, lights[0].radius);
  light_intensities[1] = cast_light(p + 2.0 * MINIMUM_SHADOW_HIT_DISTANCE * normal, lights[1].position, lights[1].radius);
  light_intensities[2] = cast_light(p + 2.0 * MINIMUM_SHADOW_HIT_DISTANCE * normal, lights[2].position, lights[2].radius);
  // TODO: for some reason the obvious thing just... doesn't work.
  // for (int i = 0; i < lights.length(); i++) {
  //   light_intensities[i] = cast_light(p + 2.0 * MINIMUM_SHADOW_HIT_DISTANCE * normal, lights[i].position, lights[i].radius);
  // }
  return `top-level-color`;
}

vec3 calculate_normal(vec3 p) {
  const vec3 step = vec3(NORMAL_OFFSET, 0.0, 0.0);

  return normalize(vec3(
    nearest_distance(p + step.xyy) - nearest_distance(p - step.xyy),
    nearest_distance(p + step.yxy) - nearest_distance(p - step.yxy),
    nearest_distance(p + step.yyx) - nearest_distance(p - step.yyx)
  ));
}

float cast_light(vec3 p, vec3 light, float radius) {
  vec3 direction = normalize(light - p);
  float light_distance = distance(light, p);

  float light_brightness = 1.0 - (light_distance / radius);
  if (light_brightness <= 0.0) {
    return 0.0;
  }

  float in_light = 1.0;
  float sharpness = 16.0;

  float last_distance = 1e20;
  // TODO: It would make more sense to start at
  // the light and cast towards the point, so that
  // we don't have to worry about this nonsense.
  float progress = MINIMUM_SHADOW_HIT_DISTANCE;
  for (int i = 0; i < MAX_STEPS; i++) {
    if (progress > light_distance) {
      return in_light * light_brightness;
    }

    float distance = nearest_distance(p + progress * direction);

    if (distance < MINIMUM_SHADOW_HIT_DISTANCE) {
      // we hit something
      return 0.0;
    }

    float intersect_offset = distance * distance / (2.0 * last_distance);
    float intersect_distance = sqrt(distance * distance - intersect_offset * intersect_offset);
    if (distance < last_distance) {
      in_light = min(in_light, sharpness * intersect_distance / max(0.0, progress - intersect_offset));
    }
    progress += distance;
    last_distance = distance;
  }
  // we never reached the light
  return 0.0;
}

vec3 march(vec3 ray_origin, vec3 ray_direction, out int steps) {
  float distance = 0.0;

  for (steps = 0; steps < MAX_STEPS; steps++) {
    vec3 p = ray_origin + distance * ray_direction;

    float nearest = nearest_distance(p);

    // TODO: this attenuation only works when we're
    // using march to render from the camera's point
    // of view, so we can't use the march function
    // as-is to render reflections. I don't know if
    // it's worth having.
    //if (nearest < distance * CAMERA_RAY_HIT_SCALE) {
    if (nearest < MINIMUM_SHADOW_HIT_DISTANCE || distance > MAXIMUM_TRACE_DISTANCE) {
      return p + nearest * ray_direction;
    }

    distance += nearest;
  }
  return ray_origin + distance * ray_direction;
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
  const float gamma = 2.2;
  const vec2 resolution = vec2(1024.0, 1024.0);

  vec2 rotation = vec2(`(float (camera :x))`, `(float (camera :y))`);
  mat3 camera_matrix = rotate_xy(rotation);

  vec3 dir = ray_dir(45.0, resolution, gl_FragCoord.xy);  
  vec3 eye = vec3(0.0, 0.0, `(float (* 256 (camera :zoom)))`);
  dir = camera_matrix * dir;
  eye = camera_matrix * eye;

  const vec3 fog_color = vec3(0.15);
  const vec3 abort_color = vec3(1.0, 0.0, 1.0);

  // TODO: we only need the steps out parameter when
  // we're rendering the debug view. Should try to
  // see if there's any performance difference between
  // an out parameter and a local variable.
  int steps;
  vec3 hit = march(eye, dir, steps);

  vec3 color = nearest_color(hit, eye);
  float depth = length(hit - eye);
  float attenuation = depth / MAXIMUM_TRACE_DISTANCE;
  color = mix(color, fog_color, clamp(attenuation * attenuation, 0.0, 1.0));

  // This is a view for debugging convergence, but it also just...
  // looks really cool on its own:
  // if (steps == MAX_STEPS) {
  //   color = abort_color;
  // } else {
  //   color = vec3(float(steps) / float(MAX_STEPS));
  // }

  frag_color = vec4(pow(color, vec3(1.0 / gamma)), 1.0);
}
`)))

# surely I can do better
(defn is-good-value? [value]
  (and (struct? value)
       (not (nil? (value :compile)))))

(fiber/new (fn []
  (while true
    (let [[expr camera] (yield)]
      (if (is-good-value? expr)
        (try
          (make-fragment-shader expr camera)
          ([err fiber] 
            (debug/stacktrace fiber err "")))
        (eprint "cannot compile " expr))))))
