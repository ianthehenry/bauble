(use ./import)

(import ./prelude :prefix "" :export true)
(import ./rotate :prefix "" :export true)
(import ./noise :prefix "" :export true)
(import ./shapes-2d :prefix "" :export true)
(import ./shapes-3d :prefix "" :export true)
(import ./dimensions :prefix "" :export true)
(import ./boolean :prefix "" :export true)
(import ../dynvars :prefix "" :export true)
(import ../../jlsl/prelude :prefix "" :export true)

(defmacro view
  "A shorthand for `(set subject _)` that fits nicely into pipe notation, e.g. `(sphere 50 | view)`."
  [subject]
  ~(set subject ,subject))

(var subject
  "A variable that determines what Bauble will render.\n\nYou can set this variable explicitly to change your focus, or use the `view` macro to change your focus. If you don't set a subject, Bauble will render the last shape in your script."
  nil)

# This is a minor convenience that lets us use the 3D vector
# x/y/-x/-y vectors as arguments to move
# TODO: should we just truncate the vector instead? That's... easier and maybe better?
(defn- coerce-axis-vector [type vector]
  (typecheck
    (jlsl/coerce-expr
      (if (= type jlsl/type/vec2)
        (case vector
          x [1 0]
          y [0 1]
          -x [-1 0]
          -y [0 -1]
          vector)
        vector))
    type))

# TODO: maybe this should be, like, a vector sum, but you peek forward
# each time to see if the next argument is a scalar? would that be better?
# it's a backwards-compatible change...
(defn- sum-scaled-vectors [dimension args]
  (reduce2 + (seq [[direction scale] :in (partition 2 args)]
    (* (coerce-axis-vector dimension direction) (jlsl/coerce-expr (@or scale 1))))))

(defn move
  ````
  Translate a shape. Usually you'd use this with a vector offset:

  ```
  (move (box 50) [0 100 0])
  ```

  But you can also provide a vector and a scalar:

  ```
  (move (box 50) y 100)
  ```

  Which is the same as `(move (box 50) (y * 100))`.

  If you provide multiple vector-scalar pairs, their sum is the final offset:

  ```
  (move (box 50) x 100 y 100 -z 20)
  ```

  That is the same as `(move (box 50) (+ (x * 100) (y * 100) (-z * 100)))`.
  ````
  [shape & args]
  (def offset (sum-scaled-vectors (field-set/type shape) args))
  (if (= (field-set/type shape) jlsl/type/vec2)
    (transform shape "move" q (- q offset))
    (transform shape "move" p (- p offset))))

(deftransform color [shape color]
  "Set the color field of a shape."
  (typecheck color jlsl/type/vec3)
  (field-set/with shape :color color))

(defhelper :float sum [:vec2 v]
  "Add the components of a vector."
  (return (+ v.x v.y)))
(overload :float sum [:vec3 v] (return (+ v.x v.y v.z)))
(overload :float sum [:vec4 v] (return (+ v.x v.y v.z v.w)))

(defhelper :float product [:vec2 v]
  "Multiply the components of a vector."
  (return (* v.x v.y)))
(overload :float product [:vec3 v] (return (* v.x v.y v.z)))
(overload :float product [:vec4 v] (return (* v.x v.y v.z v.w)))