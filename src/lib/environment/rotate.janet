(use ./import)

(defhelper :mat3 rotation-x [:float angle]
  "A rotation matrix about the X axis."
  (var s (sin angle))
  (var c (cos angle))
  (return (mat3
    1 0 0
    0 c s
    0 (- s) c)))

(defhelper :mat3 rotation-y [:float angle]
  "A rotation matrix about the Y axis."
  (var s (sin angle))
  (var c (cos angle))
  (return (mat3
    c 0 (- s)
    0 1 0
    s 0 c)))

(defhelper :mat3 rotation-z [:float angle]
  "A rotation matrix about the Z axis."
  (var s (sin angle))
  (var c (cos angle))
  (return (mat3
    c s 0
    (- s) c 0
    0 0 1)))

(defhelper :mat3 cross-matrix [:vec3 vec]
  "Returns the matrix such that `(* (cross-matrix vec1) vec2)` = `(cross vec1 vec2)`."
  (return (mat3
    0 vec.z (- vec.y)
    (- vec.z) 0 vec.x
    vec.y (- vec.x) 0)))

(defhelper :mat3 rotation-around [:vec3 axis :float angle]
  "A rotation matrix about an arbitrary axis. More expensive to compute than the axis-aligned rotation matrices."
  (return (+
    (cos angle * mat3 1 0 0 0 1 0 0 0 1)
    (sin angle * cross-matrix axis)
    (1 - cos angle * outer-product axis axis))))

(defhelper :mat3 alignment-matrix [:vec3 from :vec3 to]
  ```
  Return a 3D rotation matrix that aligns one normalized vector to another.

  Both input vectors must have a unit length!

  If `from` = `(- to)`, the result is undefined.
  ```
  (var axis (cross to from))
  (var cosA (dot to from))
  (var k (1 + cosA /))
  (return (mat3
    (axis.x * axis.x * k + cosA)
    (axis.y * axis.x * k - axis.z)
    (axis.z * axis.x * k + axis.y)
    (axis.x * axis.y * k + axis.z)
    (axis.y * axis.y * k + cosA)
    (axis.z * axis.y * k - axis.x)
    (axis.x * axis.z * k - axis.y)
    (axis.y * axis.z * k + axis.x)
    (axis.z * axis.z * k + cosA))))

(defn- floaty? [x] (= (jlsl/expr/type (jlsl/coerce-expr x)) jlsl/type/float))

(defhelper- :mat2 rotation-2d [:float angle]
  (var s (sin angle))
  (var c (cos angle))
  (return (mat2 c s (- s) c)))

(defn- rotation-matrix-3d [multiplier args]
  (reduce2 * (seq [[axis angle] :in (partition 2 args)]
    (assert angle "angle required")
    (def angle (* multiplier angle))
    (case axis
      x (rotation-x angle)
      y (rotation-y angle)
      z (rotation-z angle)
      -x (rotation-x (- angle))
      -y (rotation-y (- angle))
      -z (rotation-z (- angle))
      (rotation-around axis angle)))))

(defn- rotation-matrix-2d [multiplier args]
  (each arg args (assertf (floaty? arg) "expected angle, got %q" (jlsl/show arg)))
  (rotation-2d (* multiplier (reduce2 + args))))

(defn rotation-matrix
  "Return a rotation matrix. Takes the same arguments as `rotate`, minus the initial thing to rotate."
  [& args]
  (if (floaty? (first args))
    (rotation-matrix-2d 1 args)
    (rotation-matrix-3d 1 args)))

(defn- rotate-shape [shape args]
  (case (shape/type shape)
    jlsl/type/vec2 (transform shape "rotate" q (* ,(rotation-matrix-2d -1 args) q))
    jlsl/type/vec3 (transform shape "rotate" p (* ,(rotation-matrix-3d -1 args) p))))

(defn- rotate-vector [vector args]
  (def v (jlsl/coerce-expr vector))
  (case (jlsl/expr/type v)
    jlsl/type/vec2 (* (rotation-matrix-2d 1 args) v)
    jlsl/type/vec3 (* (rotation-matrix-3d 1 args) v)
    (errorf "I don't know how to rotate %q" (jlsl/expr/to-sexp v))))

(defn- align-shape [shape from to]
  (transform shape "align" p (* (alignment-matrix to from) p)))

(defn- align-vector [vector from to]
  (def v (typecheck vector jlsl/type/vec3))
  jlsl/type/vec3 (* (alignment-matrix from to) v))

(defn rotate
  ````
  Rotate a shape or a vector. Positive angles are counter-clockwise rotations.

  In 3D, the arguments should be pairs of `axis angle`. For example:

  ```
  (rotate (box 50) x 0.1 y 0.2)
  ```

  All `axis` arguments must be unit vectors. There are built-in axis variables `x`/`+y`/`-z`
  for the cardinal directions, and these produce optimized rotation matrices. But you can
  rotate around an arbitrary axis:

  ```
  (rotate (box 50) (normalize [1 1 1]) t)
  ```

  The order of the arguments is significant, as rotations are not commutative.

  In 2D, the arguments should just be angles; no axis is allowed.
  ````
  [target & args]
  (assert (> (@length args) 0) "not enough arguments")
  (if (shape/is? target)
    (rotate-shape target args)
    (rotate-vector target args)))

(defn align
  ````
  Align a shape or a vector to another vector. Both the `from` and `to` vectors must have unit length.

  This function is useful for "pointing" one shape towards another. For example:

  ```
  (def pos [(sin (t * 2) * 50) (sin (t * 3) * 100) (cos (t * 5) * 50)])
  (union
    (cone y 10 80 | align y (normalize pos))
    (box 10 | move pos))
  ```

  The tip of the cone points towards the moving target. In this case the `from` vector is equal to the
  axis of the cone.

  If `from` = `(- to)`, the result is undefined: there are infinitely many rotation matrices that reverse
  a vector's direction.
  ````
  [target from to]
  (if (shape/is? target)
    (align-shape target from to)
    (align-vector target from to)))
