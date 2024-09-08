(use ../jlsl)

# available in distance or color fields:

(jlsl/defdyn t :float "The current time in seconds.")
(jlsl/defdyn ray-origin :vec3 "A point in the global coordinate space that represents the origin of the ray -- in other words, the location of the camera.")
(jlsl/defdyn ray-dir :vec3 "A normalized vector that represents the direction of the current ray.")
(jlsl/defdyn depth :float "The distance that the current ray has marched, equal to `(distance ray-origin P)`. Not defined in 2D.")

(jlsl/defdyn p :vec3 "The local point in 3D space. This is position of the current ray, with any transformations applied to it.")
(jlsl/defdyn P :vec3 "The global point in 3D space. This is the position of the current ray before any transformations are applied to it.")
(jlsl/defdyn q :vec2 "The local point in 2D space. This is the position being shaded, with any transformations applied.")
(jlsl/defdyn Q :vec2 "The global point in 2D space.")

# only available in color fields:

(jlsl/defdyn normal :vec3 "(Color only!) A normalized vector that approximates the 3D distance field gradient at `P` (in other words, the surface normal for shading).")
(jlsl/defdyn gradient :vec2 "(Color only!) An approximation of the 2D distance field gradient at `Q`.")
(jlsl/defdyn dist :float "(Color only!) The value of the global distance field at `P`. In 3D, this should be a very small positive number, assuming the ray was able to converge correctly. In 2D, this gives a more useful value.")
