#!/usr/bin/env bash

redo-ifchange mode rollup
mode=$(cat mode)

actual_outpath_jfc=$PWD/$3

cat >$3 rollup-artifacts/embeditor.js rollup-artifacts/embauble.iife.js <(cat <<EOF
if (typeof WorkerGlobalScope !== 'undefined' && self instanceof WorkerGlobalScope) {
} else {
  document.addEventListener('DOMContentLoaded', () => {
    const canvas = document.getElementById('expandy-balloon');
    const slider = canvas.nextElementSibling;
    const bounceCheckbox = slider.nextElementSibling.querySelector('input');
    canvas.width = canvas.clientWidth * window.devicePixelRatio;
    canvas.height = canvas.clientHeight * window.devicePixelRatio;
    const bauble = new Bauble(canvas, {
      source: "#version 300 es\nprecision highp float;\n\nstruct Ray {\n  vec3 origin;\n  vec3 direction;\n};\n\nout vec4 frag_color;\n\nuniform float separation;\nuniform int camera_type;\nuniform vec3 free_camera_target;\nuniform vec2 free_camera_orbit;\nuniform float free_camera_zoom;\nuniform vec2 origin_2d;\nuniform float t;\nuniform vec4 viewport;\n\nmat2 rotation_2d(float angle) {\n  float s = sin(angle);\n  float c = cos(angle);\n  return mat2(c, s, -s, c);\n}\n\nfloat max_(vec2 v) {\n  return max(v.x, v.y);\n}\n\nmat3 rotation_y(float angle) {\n  float s = sin(angle);\n  float c = cos(angle);\n  return mat3(c, 0.0, -s, 0.0, 1.0, 0.0, s, 0.0, c);\n}\n\nmat3 rotation_x(float angle) {\n  float s = sin(angle);\n  float c = cos(angle);\n  return mat3(1.0, 0.0, 0.0, 0.0, c, s, 0.0, -s, c);\n}\n\nvec3 perspective_vector(float fov, vec2 frag_coord) {\n  float cot_half_fov = tan(radians(90.0 - (fov * 0.5)));\n  return normalize(vec3(frag_coord, cot_half_fov));\n}\n\nmat3 rotation_z(float angle) {\n  float s = sin(angle);\n  float c = cos(angle);\n  return mat3(c, s, 0.0, -s, c, 0.0, 0.0, 0.0, 1.0);\n}\n\nvec3 safe_div(vec3 a, vec3 b) {\n  return vec3((b.x == 0.0) ? 0.0 : (a.x / b.x), (b.y == 0.0) ? 0.0 : (a.y / b.y), (b.z == 0.0) ? 0.0 : (a.z / b.z));\n}\n\nfloat atan2(float y, float x) {\n  return (x == 0.0) ? ((0.5 * 3.14159265358979) * sign(y)) : atan(y, x);\n}\n\nfloat atan21(vec2 p) {\n  return atan2(p.y, p.x);\n}\n\nfloat sdf_ellipsoid(vec3 size, vec3 p) {\n  float k0 = length(p / size);\n  float k1 = length(p / (size * size));\n  return (k0 * (k0 - 1.0)) / k1;\n}\n\nfloat sdf_cylinder_y(float radius, float height, vec3 p) {\n  vec2 other_axes = p.xz;\n  float this_axis = p.y;\n  vec2 d = abs(vec2(length(other_axes), this_axis)) - vec2(radius, height);\n  return min(max_(d), 0.0) + length(max(d, 0.0));\n}\n\nfloat move_outer(vec3 p) {\n  {\n    vec3 p1 = p - (vec3(0.0, -100.0, 0.0) * 1.0);\n    return sdf_cylinder_y(25.0, 50.0, p1);\n  }\n}\n\nfloat smooth_min_distance(vec3 p) {\n  float r = 50.0;\n  float nearest = sdf_ellipsoid(vec3(50.0, 100.0, 100.0), p);\n  float dist = move_outer(p);\n  float h = (clamp((nearest - dist) / r, -1.0, 1.0) + 1.0) * 0.5;\n  nearest = mix(nearest, dist, h) - (r * h * (1.0 - h));\n  return nearest;\n}\n\nfloat scale_outer(vec3 factor, vec3 p) {\n  {\n    vec3 p1 = p / factor;\n    return smooth_min_distance(p1);\n  }\n}\n\nfloat min_(vec3 v) {\n  return min(v.x, min(v.y, v.z));\n}\n\nfloat map_distance(vec3 factor, vec3 p) {\n  float dist = scale_outer(factor, p);\n  return min_(abs(factor)) * dist;\n}\n\nfloat let_outer(vec3 p) {\n  {\n    vec3 factor = (1.0 - vec3(0.0, 1.0, 0.0)) + (vec3(0.0, 1.0, 0.0) * ((smoothstep(-100.0, 100.0, p.y) * -0.2) + 1.0));\n    return map_distance(factor, p);\n  }\n}\n\nfloat rotate_outer(vec3 p, float parity, vec3 tile_index) {\n  {\n    vec3 p1 = p * rotation_z(((parity * 2.0) - 1.0) * 0.785398163397448);\n    return abs(dot(p1, vec3(0.0, 1.0, 0.0)) - (tile_index.y * 25.0)) - (25.0 * 0.5);\n  }\n}\n\nfloat let_outer1(vec3 p, float radial_index, vec3 tile_index) {\n  {\n    float parity = mod(radial_index, 2.0);\n    return rotate_outer(p, parity, tile_index);\n  }\n}\n\nfloat smooth_min_distance1(vec3 p, float radial_index, vec3 tile_index) {\n  float r = 1.0;\n  float nearest = let_outer(p);\n  float dist = let_outer1(p, radial_index, tile_index);\n  float h = 1.0 - ((clamp((nearest - dist) / r, -1.0, 1.0) + 1.0) * 0.5);\n  nearest = mix(nearest, dist, h) + (r * h * (1.0 - h));\n  return nearest;\n}\n\nfloat with_outer(float angular_size, vec3 p, vec3 tile_index) {\n  {\n    float radial_index = floor(mod(atan21(p.zx) + (angular_size * 0.5), 6.28318530717959) / angular_size);\n    vec3 p1 = rotation_y(-1.0 * angular_size * radial_index) * p;\n    return smooth_min_distance1(p1, radial_index, tile_index);\n  }\n}\n\nfloat let_outer2(vec3 p, vec3 tile_index) {\n  {\n    float count = 12.0;\n    float angular_size = 6.28318530717959 / count;\n    return with_outer(angular_size, p, tile_index);\n  }\n}\n\nfloat move_outer1(vec3 p, vec3 tile_index) {\n  {\n    vec3 p1 = p - (vec3(0.0, 1.0, 0.0) * (tile_index.y * -25.0));\n    return let_outer2(p1, tile_index);\n  }\n}\n\nfloat move_outer2(vec3 p, float separation, vec3 tile_index) {\n  {\n    vec3 p1 = p - (vec3(0.0, 1.0, 0.0) * ((tile_index.y + 1.0) * separation));\n    return move_outer1(p1, tile_index);\n  }\n}\n\nfloat do_(vec3 p, float separation) {\n  vec3 size = vec3(0.0, separation + 25.0, 0.0);\n  vec3 base_index = round(safe_div(p, size));\n  vec3 look_direction = sign(p - (size * base_index));\n  vec3 start_logical = ((vec3(0.0, -2.0, 0.0) * sign(size)) * look_direction) + base_index;\n  vec3 end_logical = ((vec3(0.0, 2.0, 0.0) * sign(size)) * look_direction) + base_index;\n  vec3 start = min(start_logical, end_logical);\n  vec3 end = max(start_logical, end_logical);\n  float nearest = 1000000.0;\n  for (float z = start.z; z <= end.z; ++z) {\n    for (float y = start.y; y <= end.y; ++y) {\n      for (float x = start.x; x <= end.x; ++x) {\n        {\n          vec3 tile_index = vec3(x, y, z);\n          vec3 p1 = p - (size * tile_index);\n          nearest = min(nearest, move_outer2(p1, separation, tile_index));\n        }\n      }\n    }\n  }\n  return nearest;\n}\n\nfloat move_outer3(vec3 p, float separation) {\n  {\n    vec3 p1 = p - (vec3(0.0, 1.0, 0.0) * 40.0);\n    return do_(p1, separation);\n  }\n}\n\nfloat rotate_outer1(vec3 p, float separation) {\n  {\n    vec3 p1 = p * rotation_z(1.5707963267949);\n    return move_outer3(p1, separation);\n  }\n}\n\nfloat nearest_distance(vec3 p, float separation) {\n  return rotate_outer1(p, separation);\n}\n\nfloat march(out uint steps, Ray ray, float separation) {\n  float ray_depth = 0.0;\n  for (steps = 0u; steps < 256u; ++steps) {\n    {\n      float depth = ray_depth;\n      vec3 P = ray.origin + (ray_depth * ray.direction);\n      vec3 p = P;\n      float dist = nearest_distance(p, separation);\n      if (((dist >= 0.0) && (dist < 0.1)) || (ray_depth > 65536.0)) return ray_depth;\n      float rate = (dist > 0.0) ? 0.95 : 1.05;\n      ray_depth += dist * rate;\n      if (ray_depth < 0.0) return 0.0;\n    }\n  }\n  return ray_depth;\n}\n\nfloat with_outer1(vec3 p, float separation) {\n  {\n    vec3 p1 = (vec2(1.0, -1.0).xyy * 0.005) + p;\n    return nearest_distance(p1, separation);\n  }\n}\n\nfloat with_outer2(vec3 p, float separation) {\n  {\n    vec3 p1 = (vec2(1.0, -1.0).yyx * 0.005) + p;\n    return nearest_distance(p1, separation);\n  }\n}\n\nfloat with_outer3(vec3 p, float separation) {\n  {\n    vec3 p1 = (vec2(1.0, -1.0).yxy * 0.005) + p;\n    return nearest_distance(p1, separation);\n  }\n}\n\nfloat with_outer4(vec3 p, float separation) {\n  {\n    vec3 p1 = (vec2(1.0, -1.0).xxx * 0.005) + p;\n    return nearest_distance(p1, separation);\n  }\n}\n\nvec3 pow_(vec3 v, float e) {\n  return pow(v, vec3(e));\n}\n\nfloat fresnel(float exponent, vec3 normal, Ray ray) {\n  return pow(1.0 + dot(normal, ray.direction), exponent);\n}\n\nvec3 with_outer5(vec3 nearest_index, vec3 normal, vec3 p, Ray ray, vec3 size) {\n  {\n    vec3 tile_index = nearest_index;\n    vec3 p1 = p - (size * tile_index);\n    return (mod(tile_index.y, 2.0) == 0.0) ? pow_(mix((normal + 1.0) * 0.5, vec3(1.0, 1.0, 1.0), fresnel(5.0, normal, ray)), 1.5) : mix((normal + 1.0) * 0.5, vec3(1.0, 1.0, 1.0), fresnel(5.0, normal, ray));\n  }\n}\n\nvec3 do_1(vec3 normal, vec3 p, Ray ray, float separation) {\n  vec3 size = vec3(0.0, separation + 25.0, 0.0);\n  vec3 base_index = round(safe_div(p, size));\n  vec3 look_direction = sign(p - (size * base_index));\n  vec3 start_logical = ((vec3(0.0, -2.0, 0.0) * sign(size)) * look_direction) + base_index;\n  vec3 end_logical = ((vec3(0.0, 2.0, 0.0) * sign(size)) * look_direction) + base_index;\n  vec3 start = min(start_logical, end_logical);\n  vec3 end = max(start_logical, end_logical);\n  float nearest = 1000000.0;\n  vec3 nearest_index = vec3(0.0, 0.0, 0.0);\n  for (float z = start.z; z <= end.z; ++z) {\n    for (float y = start.y; y <= end.y; ++y) {\n      for (float x = start.x; x <= end.x; ++x) {\n        {\n          vec3 tile_index = vec3(x, y, z);\n          vec3 p1 = p - (size * tile_index);\n          float dist = move_outer2(p1, separation, tile_index);\n          if (dist < nearest) {\n            nearest = dist;\n            nearest_index = tile_index;\n          }\n        }\n      }\n    }\n  }\n  return with_outer5(nearest_index, normal, p, ray, size);\n}\n\nvec3 move_outer4(vec3 normal, vec3 p, Ray ray, float separation) {\n  {\n    vec3 p1 = p - (vec3(0.0, 1.0, 0.0) * 40.0);\n    return do_1(normal, p1, ray, separation);\n  }\n}\n\nvec3 rotate_outer2(vec3 normal, vec3 p, Ray ray, float separation) {\n  {\n    vec3 p1 = p * rotation_z(1.5707963267949);\n    return move_outer4(normal, p1, ray, separation);\n  }\n}\n\nvec4 sample_(int camera_type, vec2 frag_coord, vec2 free_camera_orbit, vec3 free_camera_target, float free_camera_zoom, float separation) {\n  Ray ray_star = Ray(vec3(0.0, 0.0, 0.0), vec3(0.0, 0.0, 1.0));\n  vec3 ortho_quad = vec3(1024.0, (frag_coord * 212.077343935025) * free_camera_zoom);\n  float ortho_scale = 212.077343935025 * free_camera_zoom;\n  float fov = 0.0;\n  switch (camera_type) {\n  case 0: case 1: {\n    mat3 camera_rotation_matrix = rotation_y(6.28318530717959 * free_camera_orbit.x) * rotation_x(6.28318530717959 * free_camera_orbit.y);\n    ray_star = Ray((camera_rotation_matrix * vec3(0.0, 0.0, 512.0 * free_camera_zoom)) + free_camera_target, camera_rotation_matrix * (perspective_vector(45.0, frag_coord) * vec3(1.0, 1.0, -1.0)));\n    fov = 45.0;\n    break;\n  }\n  case 2: {\n    ray_star = Ray(ortho_quad.yxz + free_camera_target, vec3(0.0, -1.0, 0.0));\n    break;\n  }\n  case 3: {\n    ray_star = Ray(ortho_quad.yzx + free_camera_target, vec3(0.0, 0.0, -1.0));\n    break;\n  }\n  case 4: {\n    ray_star = Ray(ortho_quad.xzy + free_camera_target, vec3(-1.0, 0.0, 0.0));\n    break;\n  }\n  }\n  uint steps = 0u;\n  {\n    Ray ray = ray_star;\n    float depth = march(steps, ray, separation);\n    vec3 P = ray.origin + (ray.direction * depth);\n    vec3 p = P;\n    float dist = nearest_distance(p, separation);\n    vec3 normal = normalize((vec2(1.0, -1.0).xyy * with_outer1(p, separation)) + (vec2(1.0, -1.0).yyx * with_outer2(p, separation)) + (vec2(1.0, -1.0).yxy * with_outer3(p, separation)) + (vec2(1.0, -1.0).xxx * with_outer4(p, separation)));\n    vec4 color = vec4(0.0);\n    color = (dist >= 10.0) ? vec4(0.0) : vec4(rotate_outer2(normal, p, ray, separation), 1.0);\n    return color;\n  }\n}\n\nvoid main() {\n  const float gamma = 2.2;\n  vec3 color = vec3(0.0, 0.0, 0.0);\n  float alpha = 0.0;\n  const uint aa_grid_size = 1u;\n  const float aa_sample_width = 1.0 / float(1u + aa_grid_size);\n  const vec2 pixel_origin = vec2(0.5, 0.5);\n  vec2 local_frag_coord = gl_FragCoord.xy - viewport.xy;\n  mat2 rotation = rotation_2d(0.2);\n  for (uint y = 1u; y <= aa_grid_size; ++y) {\n    for (uint x = 1u; x <= aa_grid_size; ++x) {\n      vec2 sample_offset = (aa_sample_width * vec2(float(x), float(y))) - pixel_origin;\n      sample_offset = rotation * sample_offset;\n      sample_offset = fract(sample_offset + pixel_origin) - pixel_origin;\n      {\n        vec2 Frag_Coord = local_frag_coord + sample_offset;\n        vec2 resolution = viewport.zw;\n        vec2 frag_coord = ((Frag_Coord - (0.5 * resolution)) / max_(resolution)) * 2.0;\n        vec4 this_sample = clamp(sample_(camera_type, frag_coord, free_camera_orbit, free_camera_target, free_camera_zoom, separation), 0.0, 1.0);\n        color += this_sample.rgb * this_sample.a;\n        alpha += this_sample.a;\n      }\n    }\n  }\n  if (alpha > 0.0) {\n    color = color / alpha;\n    alpha /= float(aa_grid_size * aa_grid_size);\n  }\n  frag_color = vec4(pow_(color, 1.0 / gamma), alpha);\n}\n",
      freeCamera: true,
      uniforms: {
        separation: "float"
      }
    });
    bauble.setCamera({rotation: [0, 0]});
    let separation = 0;
    let velocity = 0;
    let editing = false;
    let bounce = true;
    bauble.set({separation});

    const dampen = () => {
      if (!editing && bounce) {
        const oldSeparation = separation;
        separation = separation + velocity;
        const acceleration = -0.05 * separation;
        if (separation < 0) {
          separation *= -1;
          velocity *= -1;
        }
        velocity += acceleration;
        velocity *= 0.9;
        slider.valueAsNumber = separation;
        // only set if it's different enough, so we
        // don't redraw the canvas endlessly
        if (Math.abs(separation - oldSeparation) > 0.01) {
          bauble.set({separation});
        }
      }
      requestAnimationFrame(dampen);
    }
    dampen();

    canvas.addEventListener('dblclick', () => {
      bauble.setCamera({rotation: [0, 0]});
    });

    bounceCheckbox.addEventListener('input', (e) => {
      bounce = bounceCheckbox.checked;
    });

    slider.addEventListener('input', (e) => {
      editing = true;
      separation = slider.valueAsNumber;
      bauble.set({separation});
    });
    slider.addEventListener('change', (e) => {
      editing = false;
      velocity = 0;
    });
  });
}
EOF
)

# So SolidJS performs a top-level effect at load time that reads from
# the window. In a worker, this throws. So we don't perform the effect
# in a window.
sed -E "s/^delegateEvents/if (typeof window !== 'undefined') delegateEvents/" -i "$3"

# Similar, but this time it's emscripten that wants to read the current script path for some reason.
sed -E "s|document\.baseURI|(typeof document == 'undefined' ? 'https://example.org/' : document.baseURI)|" -i "$3"

cd ../studio

if [[ $mode == "prod" ]]; then
  ./node_modules/.bin/terser "$actual_outpath_jfc" -o "$actual_outpath_jfc"
fi
