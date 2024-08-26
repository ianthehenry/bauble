(use ./import)

(defhelper :mat3 rotate-x [:float angle]
  "A rotation matrix about the X axis."
  (var s (sin angle))
  (var c (cos angle))
  (return (mat3
    1 0 0
    0 c s
    0 (- s) c)))

(defhelper :mat3 rotate-y [:float angle]
  "A rotation matrix about the Y axis."
  (var s (sin angle))
  (var c (cos angle))
  (return (mat3
    c 0 (- s)
    0 1 0
    s 0 c)))

(defhelper :mat3 rotate-z [:float angle]
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

(defhelper :mat3 rotate-around [:vec3 axis :float angle]
  "A rotation matrix about an arbitrary axis. More expensive to compute than the axis-aligned rotation matrices."
  (return (+
    (cos angle * mat3 1 0 0 0 1 0 0 0 1)
    (sin angle * cross-matrix axis)
    (1 - cos angle * outer-product axis axis))))

(defn- floaty? [x] (= (jlsl/expr/type (jlsl/coerce-expr x)) jlsl/type/float))

(defhelper- :mat2 rotate-2d [:float angle]
  (var s (sin angle))
  (var c (cos angle))
  (return (mat2 c s (- s) c)))

(defn- rotation-matrix-3d [multiplier args]
  (reduce2 * (seq [[axis angle] :in (partition 2 args)]
    (assert angle "angle required")
    (def angle (* multiplier angle))
    (case axis
      x (rotate-x angle)
      y (rotate-y angle)
      z (rotate-z angle)
      -x (rotate-x (- angle))
      -y (rotate-y (- angle))
      -z (rotate-z (- angle))
      (rotate-around axis angle)))))

(defn- show [x] (if (jlsl/expr? x) (jlsl/expr/to-sexp x) x))

(defn- rotation-matrix-2d [multiplier args]
  (each arg args (assertf (floaty? arg) "expected angle, got %q" (show arg)))
  (rotate-2d (* multiplier (reduce2 + args))))

(defn rotation-matrix
  "Return a rotation matrix. Takes the same arguments as `rotate`, minus the initial thing to rotate."
  [& args]
  (if (floaty? (first args))
    (rotation-matrix-2d 1 args)
    (rotation-matrix-3d 1 args)))

(defn- rotate-shape [shape args]
  (case (field-set/type shape)
    jlsl/type/vec2 (transform shape "rotate" q (* ,(rotation-matrix-2d -1 args) q))
    jlsl/type/vec3 (transform shape "rotate" p (* ,(rotation-matrix-3d -1 args) p))))

(defn- rotate-vector [vector args]
  (def v (jlsl/coerce-expr vector))
  (case (jlsl/expr/type v)
    jlsl/type/vec2 (* (rotation-matrix-2d 1 args) v)
    jlsl/type/vec3 (* (rotation-matrix-3d 1 args) v)
    (errorf "I don't know how to rotate %q" (jlsl/expr/to-sexp v))))

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
  (if (field-set/is? target)
    (rotate-shape target args)
    (rotate-vector target args)))
