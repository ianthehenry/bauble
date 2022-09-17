(import ./glslisp/src/comp-state :as comp-state)
(import ./glslisp/src/index :as glslisp)
(import ./glsl-helpers)
(import ./globals)

(def- debug? false)

(defn- compile-function [{:name name :params params :body body :return-type return-type}]
  (string/format "%s %s(%s) {\n%s\n}" return-type name (string/join params ", ") body))

(defn- compile-fragment-shader [expr]
  (var animated? false)
  (def comp-state (comp-state/new glsl-helpers/functions))

  (when debug?
    (pp (:compile expr (:new-scope comp-state)))
    (pp (:surface expr (:new-scope comp-state))))
  (def distance-scope (:new-scope comp-state))
  (def color-scope (:new-scope comp-state))
  (def [distance-statements distance-expression] (:compile-distance distance-scope expr))

  (def [color-statements color-expression] (:compile-color color-scope expr))
  (def function-defs (string/join (map compile-function (comp-state :functions)) "\n"))

  (def distance-prep-statements @[])
  (each free-variable (keys (distance-scope :free-variables))
    (case free-variable
      globals/p nil
      globals/t (set animated? true)
      globals/camera nil
      globals/P (array/push distance-prep-statements "vec3 P = p;")
      (errorf "cannot use %s in a distance expression" (free-variable :name))))

  (def color-prep-statements @[])
  # this statement must come first so that the light intensity can see it
  (if (or ((color-scope :free-variables) globals/normal)
          ((color-scope :free-variables) globals/occlusion)
          ((color-scope :free-variables) globals/light-intensities))
    (array/push color-prep-statements "vec3 normal = calculate_normal(p);"))
  (each free-variable (keys (color-scope :free-variables))
    (case free-variable
      globals/p nil
      globals/t (set animated? true)
      globals/camera nil
      globals/normal nil
      globals/P (array/push color-prep-statements "vec3 P = p;")
      globals/occlusion (array/push color-prep-statements "float occlusion = calculate_occlusion(p, normal);")
      globals/light-intensities (do
        # Array initialization syntax doesn't work on the Google
        # Pixel 6a, so we do this kinda dumb thing. Also a simple
        # for loop doesn't work on my mac. So I dunno.
        (array/push color-prep-statements "float light_intensities[3];")
        # A for loop would be obvious, but it doesn't work for some reason.
        (for i 0 1
          (array/push color-prep-statements
            (string `light_intensities[`i`] = cast_light(p + 2.0 * MINIMUM_HIT_DISTANCE * normal, lights[`i`].position, lights[`i`].radius);`)))
        (for i 1 3
          (array/push color-prep-statements
            (string `light_intensities[`i`] = 0.0;`))))
      (errorf "unexpected free variable %s" (free-variable :name))))

  (when debug?
    (print
      (string function-defs "\n"
        "float nearest_distance(vec3 p) {\n"
        (string/join distance-prep-statements "\n  ")"\n"
        (string/join distance-statements "\n  ")"\n"
        "return "distance-expression";\n}"))
    (print
      (string
        "vec3 nearest_color(vec3 p) {\n"
        (string/join color-prep-statements "\n  ") "\n"
        (string/join color-statements "\n  ") "\n"
        "return "color-expression";\n}")))

  [animated? (string `
#version 300 es
precision highp float;

uniform vec3 camera_origin;
uniform mat3 camera_matrix;
uniform float t;
uniform int render_type;
uniform vec4 viewport;

out vec4 frag_color;

const int MAX_STEPS = 256;
const float MINIMUM_HIT_DISTANCE = 0.1;
const float NORMAL_OFFSET = 0.005;
const float MAXIMUM_TRACE_DISTANCE = 64.0 * 1024.0;

const float PI = 3.14159265359;

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

`
function-defs
`
float nearest_distance(vec3 p) {
  `
  (string/join distance-prep-statements "\n  ") "\n  "
  (string/join distance-statements "\n  ")
  `
  return `distance-expression`;
}

vec3 calculate_normal(vec3 p) {
  const vec3 step = vec3(NORMAL_OFFSET, 0.0, 0.0);

  return normalize(vec3(
    nearest_distance(p + step.xyy) - nearest_distance(p - step.xyy),
    nearest_distance(p + step.yxy) - nearest_distance(p - step.yxy),
    nearest_distance(p + step.yyx) - nearest_distance(p - step.yyx)
  ));
}

float calculate_occlusion(vec3 p, vec3 normal) {
  const int step_count = 10;
  const float max_distance = 10.0;
  const float step_size = max_distance / float(step_count);
  float baseline = nearest_distance(p);
  float occlusion = 0.0;
  // TODO: this does some good to reduce the problem where a "neck" will
  // have band of completely unoccluded space, but it introduces some
  // terrible banding artifacts on flat surfaces.
  // vec3 sine_noise = sin(p * 43758.5453);
  // vec3 rand = sign(sine_noise) * fract(sine_noise);
  // vec3 step = normalize(normal + rand) * step_size;
  vec3 step = normal * step_size;
  for (int i = 1; i <= step_count; i++) {
    float expected_distance = baseline + float(i) * step_size;
    float actual_distance = max(nearest_distance(p + float(i) * step), 0.0);
    occlusion += actual_distance / expected_distance;
  }
  occlusion /= float(step_count);
  return clamp(occlusion, 0.0, 1.0);
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
  float progress = MINIMUM_HIT_DISTANCE;
  for (int i = 0; i < MAX_STEPS; i++) {
    if (progress > light_distance) {
      return in_light * light_brightness;
    }

    float distance = nearest_distance(p + progress * direction);

    if (distance < MINIMUM_HIT_DISTANCE) {
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
    // if (nearest < distance * MINIMUM_HIT_DISTANCE * 0.01) {
    if (nearest < MINIMUM_HIT_DISTANCE || distance > MAXIMUM_TRACE_DISTANCE) {
      return p + nearest * ray_direction;
    }

    distance += nearest;
  }
  return ray_origin + distance * ray_direction;
}

vec3 nearest_color(vec3 p) {
  `
  (string/join color-prep-statements "\n  ") "\n  "
  (string/join color-statements "\n  ")
  `
  return `color-expression`;
}

const float DEG_TO_RAD = PI / 180.0;
vec3 perspective(float fov, vec2 size, vec2 pos) {
  vec2 xy = pos - size * 0.5;

  float cot_half_fov = tan((90.0 - fov * 0.5) * DEG_TO_RAD);
  float z = min(size.x, size.y) * 0.5 * cot_half_fov;

  return normalize(vec3(xy, -z));
}

void main() {
  const float gamma = 2.2;

  vec2 local_coord = gl_FragCoord.xy - viewport.xy;
  vec2 resolution = viewport.zw;
  vec3 dir = camera_matrix * perspective(45.0, resolution, local_coord);

  const vec3 fog_color = vec3(0.15);

  int steps;
  vec3 hit = march(camera_origin, dir, steps);

  vec3 color;
  switch (render_type) {
    case 0: {
      float depth = distance(camera_origin, hit);
      if (depth >= MAXIMUM_TRACE_DISTANCE) {
        const vec3 light = pow(vec3(69.0, 72.0, 79.0) / vec3(255.0), vec3(gamma));
        const vec3 dark = pow(vec3(40.0, 42.0, 46.0) / vec3(255.0), vec3(gamma));
        color = vec3(mix(dark, light, (local_coord.x + local_coord.y) / (resolution.x + resolution.y)));
      } else {
        color = nearest_color(hit);
      }
      break;
    }
    case 1: {
      // convergence debugging
      if (steps == MAX_STEPS) {
        color = vec3(1.0, 0.0, 1.0);
      } else {
        color = vec3(float(steps) / float(MAX_STEPS));
      }
      break;
    }
    case 2: {
      // overshoot debugging
      float distance = nearest_distance(hit);
      float overshoot = max(-distance, 0.0) / MINIMUM_HIT_DISTANCE;
      float undershoot = max(distance, 0.0) / MINIMUM_HIT_DISTANCE;
      color = vec3(overshoot, 1.0 - undershoot - overshoot, 1.0 - step(1.0, undershoot));
      break;
    }
  }

  frag_color = vec4(pow(color, vec3(1.0 / gamma)), 1.0);
}
`)])

# surely I can do better
(defn- is-good-value? [value]
  (and (struct? value)
       (not (nil? (value :compile)))))

(defn compile-shape [expr]
  (if (is-good-value? expr)
    (try
      (compile-fragment-shader expr)
      ([err fiber]
        (debug/stacktrace fiber err "")
        :error))
    (do
      (eprintf "cannot compile %p" expr)
      :error)))
