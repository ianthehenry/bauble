Okay this is the embed part.

<canvas style="image-rendering: pixelated; width: 512px; height: 256px;" width="256" height="128"></canvas>

<script type="text/javascript">
document.addEventListener('DOMContentLoaded', () => {
  // find the canvas element that you want to render it to
  const canvas = document.querySelector('canvas');

  const player = new Bauble(canvas, {
  animate: true,
  source: `#version 300 es
precision highp float;

struct Ray {
  vec3 origin;
  vec3 direction;
};

out vec4 frag_color;

uniform int camera_type;
uniform vec3 free_camera_target;
uniform vec2 free_camera_orbit;
uniform float free_camera_zoom;
uniform vec2 origin_2d;
uniform float t;
uniform vec4 viewport;

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
  return normalize(vec3(frag_coord, cot_half_fov));
}

float max_1(vec3 v) {
  return max(v.x, max(v.y, v.z));
}

float sdf_cube(float size, vec3 p) {
  vec3 d = abs(p) - size;
  return length(max(d, 0.0)) + min(max_1(d), 0.0);
}

float rotate_outer(vec3 p, float t) {
  {
    vec3 p1 = p * rotation_y(t);
    return sdf_cube(50.0, p1);
  }
}

float nearest_distance(vec3 p, float t) {
  return rotate_outer(p, t);
}

float march(out uint steps, Ray ray, float t) {
  float ray_depth = 0.0;
  for (steps = 0u; steps < 256u; ++steps) {
    {
      float depth = ray_depth;
      vec3 P = ray.origin + (ray_depth * ray.direction);
      vec3 p = P;
      float dist = nearest_distance(p, t);
      if (((dist >= 0.0) && (dist < 0.1)) || (ray_depth > 65536.0)) return ray_depth;
      float rate = (dist > 0.0) ? 0.95 : 1.05;
      ray_depth += dist * rate;
      if (ray_depth < 0.0) return 0.0;
    }
  }
  return ray_depth;
}

float with_outer(vec3 p, float t) {
  {
    vec3 p1 = (vec2(1.0, -1.0).xyy * 0.005) + p;
    return nearest_distance(p1, t);
  }
}

float with_outer1(vec3 p, float t) {
  {
    vec3 p1 = (vec2(1.0, -1.0).yyx * 0.005) + p;
    return nearest_distance(p1, t);
  }
}

float with_outer2(vec3 p, float t) {
  {
    vec3 p1 = (vec2(1.0, -1.0).yxy * 0.005) + p;
    return nearest_distance(p1, t);
  }
}

float with_outer3(vec3 p, float t) {
  {
    vec3 p1 = (vec2(1.0, -1.0).xxx * 0.005) + p;
    return nearest_distance(p1, t);
  }
}

vec3 do_(vec2 Frag_Coord, vec2 resolution) {
  const vec3 light = pow(vec3(69.0, 72.0, 79.0) / 255.0, vec3(2.2));
  const vec3 dark = pow(vec3(40.0, 42.0, 46.0) / 255.0, vec3(2.2));
  return vec3(mix(dark, light, (Frag_Coord.x + Frag_Coord.y) / (resolution.x + resolution.y)));
}

float fresnel(float exponent, vec3 normal, Ray ray) {
  return pow(1.0 + dot(normal, ray.direction), exponent);
}

vec4 sample_(vec2 Frag_Coord, int camera_type, vec2 frag_coord, vec2 free_camera_orbit, vec3 free_camera_target, float free_camera_zoom, vec2 resolution, float t) {
  Ray ray_star = Ray(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 1.0));
  vec3 ortho_quad = vec3(1024.0, (frag_coord * 212.077343935025) * free_camera_zoom);
  float ortho_scale = 212.077343935025 * free_camera_zoom;
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
    float depth = march(steps, ray, t);
    vec3 P = ray.origin + (ray.direction * depth);
    vec3 p = P;
    float dist = nearest_distance(p, t);
    vec3 normal = normalize((vec2(1.0, -1.0).xyy * with_outer(p, t)) + (vec2(1.0, -1.0).yyx * with_outer1(p, t)) + (vec2(1.0, -1.0).yxy * with_outer2(p, t)) + (vec2(1.0, -1.0).xxx * with_outer3(p, t)));
    vec4 color = vec4(0.0);
    color = (dist >= 10.0) ? vec4(do_(Frag_Coord, resolution), 1.0) : vec4(mix((normal + 1.0) * 0.5, vec3(1.0, 1.0, 1.0), fresnel(5.0, normal, ray)), 1.0);
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
  const uint aa_grid_size = 2u;
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
        vec2 frag_coord = ((Frag_Coord - (0.5 * resolution)) / max_(resolution)) * 2.0;
        vec4 this_sample = clamp(sample_(Frag_Coord, camera_type, frag_coord, free_camera_orbit, free_camera_target, free_camera_zoom, resolution, t), 0.0, 1.0);
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
}`
});
  canvas.addEventListener('click', () => {
    player.playPause();
  });
});
</script>
