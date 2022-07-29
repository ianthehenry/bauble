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

(defn make-fragment-shader [expr]
  (def comp-state @{:names @{} :cache @{} :functions @[]})
  (table/setproto comp-state comp-state-proto)
  (def top-level-expr (:compile expr comp-state "p"))
  (def function-defs (string/join (map compile-function (comp-state :functions)) "\n"))
  (set-fragment-shader
    (string `
#version 300 es
precision highp float;

const int MAX_STEPS = 64;
const float MINIMUM_HIT_DISTANCE = 0.1;
const float MAXIMUM_TRACE_DISTANCE = 1000.0;

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
  const vec3 step = vec3(MINIMUM_HIT_DISTANCE * 0.05, 0.0, 0.0);

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

vec3 march(vec3 ray_origin, vec3 ray_direction) {
  float distance = 0.0;

  for (int i = 0; i < MAX_STEPS; i++) {
    vec3 p = ray_origin + distance * ray_direction;

    Surface nearest = distance_field(p);

    if (nearest.distance < MINIMUM_HIT_DISTANCE) {
      vec3 hit = p + nearest.distance * ray_direction;
      vec3 normal = calculate_normal(hit);

      return 0.5 * (normal + vec3(1.0));
      
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
      break;
    }
    distance += nearest.distance;
  }
  return vec3(0.1);
}


out vec4 frag_color;

void main() {
  vec2 uv = (gl_FragCoord.xy - vec2(512.0, 384.0));

  const float zoom = 2.0;
  vec3 ray_origin = vec3(uv.x, -256.0, uv.y) / zoom;
  vec3 ray_direction = normalize(vec3(0.0, 1.0, 0.0));

  vec3 color = march(ray_origin, ray_direction);

  //if (int(gl_FragCoord.x + gl_FragCoord.y) % 2 == 0) {
  // color = floor(color * 16.0f) / 16.0f;
  //}

  frag_color = vec4(pow(color, vec3(1.0 / 2.0)), 1.0);
}
`)))

# surely I can do better
(defn is-good-value? [value]
  (and (table? value)
       (not (nil? (value :compile)))))

(fiber/new (fn []
  (while true
    (let [value (yield)]
      (if (is-good-value? value)
        (try
          (make-fragment-shader value)
          ([err fiber] 
            (debug/stacktrace fiber err)))
        (eprint "cannot compile " value))))))

