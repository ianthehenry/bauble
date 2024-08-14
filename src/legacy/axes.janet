(defn axis-vec
  "Return a `vec3` that projects along the given axis with `0` for both other coordinates. `axis` can be signed, e.g. `:-x`. `scale` defaults to `1`."
  [axis &opt scale]
  (default scale 1)
  (case axis
    :x [scale 0 0]
    :y [0 scale 0]
    :z [0 0 scale]
    :+x [scale 0 0]
    :+y [0 scale 0]
    :+z [0 0 scale]
    :-x [(- scale) 0 0]
    :-y [0 (- scale) 0]
    :-z [0 0 (- scale)]
    (errorf "unknown axis %p" axis)))

(defn other-axes [axis]
  (case axis
    :x [:y :z]
    :y [:x :z]
    :z [:x :y]
    (errorf "unknown axis %p" axis)))

(defn other-axis [axis1 axis2]
  (if (= axis1 axis2)
    (errorf "duplicate axis %p" axis1))
  (var x false)
  (var y false)
  (var z false)
  (each axis [axis1 axis2]
    (case axis
      :x (set x true)
      :y (set y true)
      :z (set z true)
      (errorf "unknown axis %p" axis)))
  (cond
    (and y z) :x
    (and x z) :y
    (and x y) :z
    (error "impossible")))

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
