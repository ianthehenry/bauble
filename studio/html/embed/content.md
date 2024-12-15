Okay this is the embed part.

<canvas style="image-rendering: pixelated; width: 512px; height:256px;" width="256" height="128"></canvas>

<script type="text/javascript">
document.addEventListener('DOMContentLoaded', () => {
    // find the canvas element that you want to render it to
    const canvas = document.querySelector('canvas');

    const player = new Bauble(canvas, {
        isAnimated: true,
        source: `#version 300 es
precision highp float;

struct Ray {
  vec3 origin;
  vec3 direction;
};
struct Light {
  vec3 color;
  vec3 direction;
  float brightness;
};

out vec4 frag_color;

uniform int camera_type;
uniform vec3 free_camera_target;
uniform vec2 free_camera_orbit;
uniform float free_camera_zoom;
uniform vec2 origin_2d;
uniform float t;
uniform vec4 viewport;
uniform vec4 crosshairs_3d;

mat2 rotation_2d(float angle) {
  float s = sin(angle);
  float c = cos(angle);
  return mat2(c, s, -s, c);
}

float max_(vec2 v) {
  return max(v.x, v.y);
}

mat3 rotation_y(float angle) {
  float s = sin(angle);
  float c = cos(angle);
  return mat3(c, 0.0, -s, 0.0, 1.0, 0.0, s, 0.0, c);
}

mat3 rotation_x(float angle) {
  float s = sin(angle);
  float c = cos(angle);
  return mat3(1.0, 0.0, 0.0, 0.0, c, s, 0.0, -s, c);
}

vec3 perspective_vector(float fov, vec2 frag_coord) {
  float cot_half_fov = tan(radians(90.0 - (fov * 0.5)));
  float z = 0.5 * cot_half_fov;
  return normalize(vec3(frag_coord, z));
}

float sdf_sphere(float radius, vec3 p) {
  return length(p) - radius;
}

float nearest_distance(vec3 p) {
  return sdf_sphere(113.0, p);
}

float march(out uint steps, Ray ray) {
  float ray_depth = 0.0;
  for (steps = 0u; steps < 256u; ++steps) {
    {
      float depth = ray_depth;
      vec3 P = ray.origin + (ray_depth * ray.direction);
      vec3 p = P;
      float dist = nearest_distance(p);
      if (((dist >= 0.0) && (dist < 0.1)) || (ray_depth > 65536.0)) return ray_depth;
      float rate = (dist > 0.0) ? 0.95 : 1.05;
      ray_depth += dist * rate;
      if (ray_depth < 0.0) return 0.0;
    }
  }
  return ray_depth;
}

float with_outer(vec3 p) {
  {
    vec3 p1 = (vec2(1.0, -1.0).xyy * 0.005) + p;
    return nearest_distance(p1);
  }
}

float with_outer1(vec3 p) {
  {
    vec3 p1 = (vec2(1.0, -1.0).yyx * 0.005) + p;
    return nearest_distance(p1);
  }
}

float with_outer2(vec3 p) {
  {
    vec3 p1 = (vec2(1.0, -1.0).yxy * 0.005) + p;
    return nearest_distance(p1);
  }
}

float with_outer3(vec3 p) {
  {
    vec3 p1 = (vec2(1.0, -1.0).xxx * 0.005) + p;
    return nearest_distance(p1);
  }
}

vec3 do_(vec2 Frag_Coord, vec2 resolution) {
  const vec3 light = pow(vec3(69.0, 72.0, 79.0) / 255.0, vec3(2.2));
  const vec3 dark = pow(vec3(40.0, 42.0, 46.0) / 255.0, vec3(2.2));
  return vec3(mix(dark, light, (Frag_Coord.x + Frag_Coord.y) / (resolution.x + resolution.y)));
}

float with_outer4(float depth, vec3 light_position, vec3 ray_dir) {
  {
    vec3 P = light_position + (ray_dir * depth);
    vec3 p = P;
    return nearest_distance(p);
  }
}

Light cast_light_hard_shadow(vec3 light_color, vec3 light_position, vec3 P, vec3 normal) {
  if (light_position == P) return Light(light_color, vec3(0.0), 1.0);
  vec3 to_light = normalize(light_position - P);
  if (light_color == vec3(0.0)) return Light(light_color, to_light, 0.0);
  if (dot(to_light, normal) < 0.0) return Light(light_color, to_light, 0.0);
  vec3 target = (0.01 * normal) + P;
  float light_distance = length(target - light_position);
  vec3 ray_dir = (target - light_position) / light_distance;
  float depth = 0.0;
  for (uint i = 0u; i < 256u; ++i) {
    float nearest = with_outer4(depth, light_position, ray_dir);
    if (nearest < 0.01) break;
    depth += nearest;
  }
  if (depth >= light_distance) return Light(light_color, to_light, 1.0);
  else return Light(light_color, to_light, 0.0);
}

float with_outer5(float depth, vec3 light_position, vec3 ray_dir) {
  {
    vec3 P = light_position + (ray_dir * depth);
    vec3 p = P;
    return nearest_distance(p);
  }
}

Light cast_light_soft_shadow(vec3 light_color, vec3 light_position, float softness, vec3 P, vec3 normal) {
  if (softness == 0.0) return cast_light_hard_shadow(light_color, light_position, P, normal);
  if (light_position == P) return Light(light_color, vec3(0.0), 1.0);
  vec3 to_light = normalize(light_position - P);
  if (light_color == vec3(0.0)) return Light(light_color, to_light, 0.0);
  if (dot(to_light, normal) < 0.0) return Light(light_color, to_light, 0.0);
  vec3 target = (0.01 * normal) + P;
  float light_distance = length(target - light_position);
  vec3 ray_dir = (target - light_position) / light_distance;
  float brightness = 1.0;
  float sharpness = 1.0 / (softness * softness);
  float last_nearest = 1000000.0;
  float depth = 0.0;
  for (uint i = 0u; i < 256u; ++i) {
    float nearest = with_outer5(depth, light_position, ray_dir);
    if (nearest < 0.01) break;
    float intersect_offset = (nearest * nearest) / (2.0 * last_nearest);
    float intersect_distance = sqrt((nearest * nearest) - (intersect_offset * intersect_offset));
    brightness = min(brightness, (sharpness * intersect_distance) / max(0.0, (light_distance - depth) - intersect_offset));
    depth += nearest;
    last_nearest = nearest;
  }
  if (depth >= light_distance) return Light(light_color, to_light, brightness);
  else return Light(light_color, to_light, 0.0);
}

float with_outer6(vec3 P, uint i, vec3 step) {
  {
    vec3 P1 = (float(i) * step) + P;
    vec3 p = P1;
    return max(nearest_distance(p), 0.0);
  }
}

float calculate_occlusion(uint step_count, float max_distance, vec3 dir, vec3 P, vec3 p) {
  float step_size = max_distance / float(step_count);
  float baseline = nearest_distance(p);
  float occlusion = 0.0;
  vec3 step = dir * step_size;
  for (uint i = 1u; i <= step_count; ++i) {
    float expected_distance = (float(i) * step_size) + baseline;
    float actual_distance = with_outer6(P, i, step);
    occlusion += actual_distance / expected_distance;
  }
  return clamp(occlusion / float(step_count), 0.0, 1.0);
}

vec3 normalize_safe(vec3 v) {
  return (v == vec3(0.0)) ? v : normalize(v);
}

Light cast_light_no_shadow(vec3 light_color, vec3 light_position, vec3 P) {
  return Light(light_color, normalize_safe(light_position - P), 1.0);
}

Light do_1(vec3 P, vec3 normal, float occlusion) {
  Light light = cast_light_no_shadow(vec3(0.15), P + (normal * 0.1), P);
  light.brightness = light.brightness * mix(0.1, 1.0, occlusion);
  return light;
}

vec3 hsv(float hue, float saturation, float value) {
  vec3 c = abs(mod((hue * 6.0) + vec3(0.0, 4.0, 2.0), 6.0) - 3.0);
  return value * mix(vec3(1.0), clamp(c - 1.0, 0.0, 1.0), saturation);
}

vec3 blinn_phong(Light light, vec3 color, float shininess, float glossiness, vec3 normal, Ray ray) {
  if (light.direction == vec3(0.0)) return color * light.color * light.brightness;
  vec3 halfway_dir = normalize(light.direction - ray.direction);
  float specular_strength = shininess * pow(max(dot(normal, halfway_dir), 0.0), glossiness * glossiness);
  float diffuse = max(0.0, dot(normal, light.direction));
  return ((light.color * light.brightness) * specular_strength) + (color * diffuse * light.color * light.brightness);
}

vec3 shade(Light light, Light light1, vec3 normal, Ray ray, vec3 temp) {
  vec3 result = vec3(0.0);
  result += blinn_phong(light, temp, 0.25, 10.0, normal, ray);
  result += blinn_phong(light1, temp, 0.25, 10.0, normal, ray);
  return result;
}

vec3 shade_outer(Light light, Light light1, vec3 normal, Ray ray) {
  {
    vec3 temp = hsv(0.0, 0.98, 1.0);
    return shade(light, light1, normal, ray, temp);
  }
}

vec3 hoist_outer(vec3 P, vec3 normal, vec3 p, Ray ray) {
  {
    Light light = cast_light_soft_shadow(vec3(1.15), P - (normalize(vec3(-2.0, -2.0, -1.0)) * 2048.0), 0.25, P, normal);
    float occlusion = calculate_occlusion(8u, 20.0, normal, P, p);
    Light light1 = do_1(P, normal, occlusion);
    return shade_outer(light, light1, normal, ray);
  }
}

vec4 intersect_axis(vec3 origin, vec3 axis, Ray ray) {
  mat2x3 A = mat2x3(ray.direction, -axis);
  vec3 b = origin - ray.origin;
  mat3x2 At = transpose(A);
  vec2 ts = (inverse(At * A) * At) * b;
  if (ts.x < 0.0) return vec4(0.0, 0.0, 0.0, 10000.0);
  vec3 p1 = (ray.direction * ts.x) + ray.origin;
  vec3 p2 = (axis * ts.y) + origin;
  return vec4(p2, distance(p1, p2));
}

vec4 sample_(vec2 Frag_Coord, int camera_type, vec4 crosshairs_3d, vec2 frag_coord, vec2 free_camera_orbit, vec3 free_camera_target, float free_camera_zoom, vec2 resolution) {
  Ray ray_star = Ray(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 1.0));
  vec3 ortho_quad = vec3(1024.0, (frag_coord * 424.154687870049) * free_camera_zoom);
  float ortho_scale = 424.154687870049 * free_camera_zoom;
  float fov = 0.0;
  switch (camera_type) {
  case 0: case 1: {
    mat3 camera_rotation_matrix = rotation_y(6.28318530717959 * free_camera_orbit.x) * rotation_x(6.28318530717959 * free_camera_orbit.y);
    ray_star = Ray((camera_rotation_matrix * vec3(0.0, 0.0, 512.0 * free_camera_zoom)) + free_camera_target, camera_rotation_matrix * (perspective_vector(45.0, frag_coord) * vec3(1.0, 1.0, -1.0)));
    fov = 45.0;
    break;
  }
  case 2: {
    ray_star = Ray(ortho_quad.yxz + free_camera_target, vec3(0.0, -1.0, 0.0));
    break;
  }
  case 3: {
    ray_star = Ray(ortho_quad.yzx + free_camera_target, vec3(0.0, 0.0, -1.0));
    break;
  }
  case 4: {
    ray_star = Ray(ortho_quad.xzy + free_camera_target, vec3(-1.0, 0.0, 0.0));
    break;
  }
  }
  uint steps = 0u;
  {
    Ray ray = ray_star;
    float depth = march(steps, ray);
    vec3 P = ray.origin + (ray.direction * depth);
    vec3 p = P;
    float dist = nearest_distance(p);
    vec3 normal = normalize((vec2(1.0, -1.0).xyy * with_outer(p)) + (vec2(1.0, -1.0).yyx * with_outer1(p)) + (vec2(1.0, -1.0).yxy * with_outer2(p)) + (vec2(1.0, -1.0).xxx * with_outer3(p)));
    vec4 color = vec4(0.0);
    if (dist >= 10.0) color = vec4(do_(Frag_Coord, resolution), 1.0);
    else color = vec4(hoist_outer(P, normal, p, ray), 1.0);
    if (crosshairs_3d.w > 0.0) {
      vec4 x = intersect_axis(crosshairs_3d.xyz, vec3(1.0, 0.0, 0.0), ray);
      vec4 y = intersect_axis(crosshairs_3d.xyz, vec3(0.0, 1.0, 0.0), ray);
      vec4 z = intersect_axis(crosshairs_3d.xyz, vec3(0.0, 0.0, 1.0), ray);
      float xd = distance(x.xyz, ray.origin);
      float yd = distance(y.xyz, ray.origin);
      float zd = distance(z.xyz, ray.origin);
      vec3 thickness = vec3(1.0, 1.0, 1.0);
      if (fov > 0.0) {
        float angular_resolution = 2.0 * tan(radians(0.5 * fov));
        thickness *= (vec3(xd, yd, zd) * angular_resolution) / 512.0;
      } else thickness *= ortho_scale / 424.154687870049;
      if (x.w < thickness.x) color = mix(color, vec4(1.0, 0.1, 0.1, 1.0), ((xd >= 65536.0) || (xd < depth)) ? 1.0 : 0.5);
      if (y.w < thickness.y) color = mix(color, vec4(0.0, 1.0, 0.25, 1.0), ((yd >= 65536.0) || (yd < depth)) ? 1.0 : 0.5);
      if (z.w < thickness.z) color = mix(color, vec4(0.0, 0.5, 1.0, 1.0), ((zd >= 65536.0) || (zd < depth)) ? 1.0 : 0.5);
    }
    return color;
  }
}

vec3 pow_(vec3 v, float e) {
  return pow(v, vec3(e));
}

void main() {
  const float gamma = 2.2;
  vec3 color = vec3(0.0, 0.0, 0.0);
  float alpha = 0.0;
  const uint aa_grid_size = 1u;
  const float aa_sample_width = 1.0 / float(1u + aa_grid_size);
  const vec2 pixel_origin = vec2(0.5, 0.5);
  vec2 local_frag_coord = gl_FragCoord.xy - viewport.xy;
  mat2 rotation = rotation_2d(0.2);
  for (uint y = 1u; y <= aa_grid_size; ++y) {
    for (uint x = 1u; x <= aa_grid_size; ++x) {
      vec2 sample_offset = (aa_sample_width * vec2(float(x), float(y))) - pixel_origin;
      sample_offset = rotation * sample_offset;
      sample_offset = fract(sample_offset + pixel_origin) - pixel_origin;
      {
        vec2 Frag_Coord = local_frag_coord + sample_offset;
        vec2 resolution = viewport.zw;
        vec2 frag_coord = (Frag_Coord - (0.5 * resolution)) / max_(resolution);
        vec4 this_sample = clamp(sample_(Frag_Coord, camera_type, crosshairs_3d, frag_coord, free_camera_orbit, free_camera_target, free_camera_zoom, resolution), 0.0, 1.0);
        color += this_sample.rgb * this_sample.a;
        alpha += this_sample.a;
      }
    }
  }
  if (alpha > 0.0) {
    color = color / alpha;
    alpha /= float(aa_grid_size * aa_grid_size);
  }
  frag_color = vec4(pow_(color, 1.0 / gamma), alpha);
}
`,
    });
    player.draw();
});
</script>
