(defn other-axes [axis]
  (case axis
    :x [:y :z]
    :y [:x :z]
    :z [:x :y]
    (errorf "unknown axis %p" axis)))

(defn string-of-axis [axis]
  (case axis
    :x "x"
    :y "y"
    :z "z"
    (errorf "unknown axis %p" axis)))

(defn string-of-axes [axes]
  (def result (buffer/new (length axes)))
  (each axis axes
    (buffer/push-string result (string-of-axis axis)))
  result)

(defn split-signed-axis [axis]
  (case axis
    :-x [-1 :x] :x [1 :x] :+x [1 :x]
    :-y [-1 :y] :y [1 :y] :+y [1 :y]
    :-z [-1 :z] :z [1 :z] :+z [1 :z]
    (errorf "unknown signed axis %p" axis)))

(defn transpose-other-axes [axis]
  (case axis
    :x [:x :z :y]
    :y [:z :y :x]
    :z [:y :x :z]
    (errorf "unknown axis %p" axis)))

(defn negate-other-axes [axis]
  (case axis
    :x [1 -1 -1]
    :y [-1 1 -1]
    :z [-1 -1 1]
    (errorf "unknown axis %p" axis)))
