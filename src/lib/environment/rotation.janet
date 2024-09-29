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

(defn- rotation-matrix-3d [args]
  (reduce2 * (seq [[axis angle] :in (partition 2 args)]
    (assert angle "angle required")
    (case axis
      x (rotation-x angle)
      y (rotation-y angle)
      z (rotation-z angle)
      -x (rotation-x (- angle))
      -y (rotation-y (- angle))
      -z (rotation-z (- angle))
      (rotation-around axis angle)))))

(defn- rotation-matrix-2d [args]
  (each arg args (assertf (floaty? arg) "expected angle, got %q" (jlsl/show arg)))
  (rotation-2d (reduce2 + args)))

(defn rotation-matrix
  "Return a rotation matrix. Takes the same arguments as `rotate`, minus the initial thing to rotate."
  [& args]
  (if (floaty? (first args))
    (rotation-matrix-2d args)
    (rotation-matrix-3d args)))
