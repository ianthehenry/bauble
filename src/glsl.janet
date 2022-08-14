# TODO: this is an embarrassing stub.
# eventually i plan to support a full
# expression compliation language.

(defn float [n]
  (if (int? n)
    (string n ".0")
    (string/format "%f" n)))

(defn vec3 [[x y z]]
  (string/format `vec3(%s, %s, %s)` (float x) (float y) (float z)))

(defn vec2 [[x y]]
  (string/format `vec2(%s, %s)` (float x) (float y)))

(defn mat3 [m]
  (string/format "mat3(%s, %s, %s, %s, %s, %s, %s, %s, %s)"
    (float (m 0)) (float (m 1)) (float (m 2))
    (float (m 3)) (float (m 4)) (float (m 5))
    (float (m 6)) (float (m 7)) (float (m 8))))
