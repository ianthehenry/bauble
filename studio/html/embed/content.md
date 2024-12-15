Okay this is the embed part.

<canvas width="384" height="128"></canvas>

<script type="text/javascript">
document.addEventListener('DOMContentLoaded', () => {
    // find the canvas element that you want to render it to
    const canvas = document.querySelector('canvas');

    const player = new Bauble(canvas, {
        isAnimated: true,
        resolution: [128, 128],
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
uniform int render_type;
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

float sdf_torus_z(float radius, float thickness, vec3 p) {
  vec2 other_axes = p.xy;
  float this_axis = p.z;
  return length(vec2(length(other_axes) - radius, this_axis)) - thickness;
}

float sdf_torus_x(float radius, float thickness, vec3 p) {
  vec2 other_axes = p.zy;
  float this_axis = p.x;
  return length(vec2(length(other_axes) - radius, this_axis)) - thickness;
}

float move_outer(vec3 p) {
  {
    vec3 p1 = p - (vec3(0.0, 1.0, 0.0) * 50.0);
    return sdf_torus_x(50.0, 25.0, p1);
  }
}

float smooth_min_distance(vec3 p) {
  float r = 15.0;
  float nearest = sdf_torus_z(50.0, 25.0, p);
  float dist = move_outer(p);
  float h = 0.5 + (0.5 * clamp((nearest - dist) / r, -1.0, 1.0));
  nearest = mix(nearest, dist, h) - (r * h * (1.0 - h));
  return nearest;
}

float nearest_distance(vec3 p) {
  return smooth_min_distance(p);
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

float fresnel(float exponent, vec3 normal, Ray ray) {
  return pow(1.0 + dot(normal, ray.direction), exponent);
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

vec4 sample_(vec2 Frag_Coord, int camera_type, vec4 crosshairs_3d, vec2 frag_coord, vec2 free_camera_orbit, vec3 free_camera_target, float free_camera_zoom, int render_type, vec2 resolution) {
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
    switch (render_type) {
    case 0: {
      if (dist >= 10.0) color = vec4(do_(Frag_Coord, resolution), 1.0);
      else color = vec4(mix(0.5 + (0.5 * normal), vec3(1.0, 1.0, 1.0), fresnel(5.0, normal, ray)), 1.0);
      break;
    }
    case 1: {
      if (dist >= 10.0) color = vec4(do_(Frag_Coord, resolution), 1.0);
      else color = vec4(mix(0.5 + (0.5 * normal), vec3(1.0, 1.0, 1.0), fresnel(5.0, normal, ray)), 1.0);
      break;
    }
    case 2: {
      color = (steps == 256u) ? vec4(1.0, 0.0, 1.0, 1.0) : vec4(vec3(float(steps) / float(256u)), 1.0);
      break;
    }
    case 3: {
      float overshoot = max(-dist, 0.0) / 0.1;
      float undershoot = max(dist, 0.0) / 0.1;
      color = vec4(overshoot, (1.0 - undershoot) - overshoot, 1.0 - step(1.0, undershoot), 1.0);
      break;
    }
    }
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
        vec4 this_sample = clamp(sample_(Frag_Coord, camera_type, crosshairs_3d, frag_coord, free_camera_orbit, free_camera_target, free_camera_zoom, render_type, resolution), 0.0, 1.0);
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
}`,
    });
    player.draw();
});
</script>
