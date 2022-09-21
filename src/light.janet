(import ./glslisp/src/type)
(import ./globals)

(def- proto @{})

(def- point-proto
  (table/setproto
    @{:apply (fn [self shape comp-state]
      (def {:position position :color color :brightness brightness :radius radius} self)
      (defn wrap [get-expr]
        (if radius
          (let [$position (:temp-var comp-state type/vec3 'position)]
            ~(with ,$position ,position
              ,(get-expr $position ~(* ,brightness (clamp (- 1 (/ (distance ,globals/P ,$position) ,radius)) 0 1)))))
          (get-expr position brightness)))

      (wrap (fn [position brightness]
        ~(extend ,globals/lights
          (cast_point_light ,globals/P ,globals/normal ,position ,color ,brightness)
          ,(:surface shape comp-state)))))}
    proto))

(def- ambient-proto
  (table/setproto
    @{:apply (fn [self shape comp-state]
      (def {:color color :brightness brightness} self)
      ~(extend ,globals/lights (LightIncidence (vec3 0) (* ,color ,brightness))
        ,(:surface shape comp-state)))}
    proto))

(defn- has-proto [x target]
  (if (nil? x)
    false
    (let [proto (table/getproto x)]
      (or (= proto target)
          (has-proto proto target)))))

(defn instance? [x]
  (and
    (= (type x) :table)
    (has-proto x proto)))

(defn point/new [position color brightness radius]
  (table/setproto
    @{:position position
      :color color
      :brightness brightness
      :radius radius}
    point-proto))

(defn ambient/new [color brightness]
  (table/setproto
    @{:color color
      :brightness brightness}
    ambient-proto))
