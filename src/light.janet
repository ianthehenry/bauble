(import ./glslisp/src/type)
(import ./globals)

(def- proto @{})

(def- point-proto
  (table/setproto
    @{:apply (fn [self shape comp-state]
      (def {:position position :color color :brightness brightness :shadow shadow} self)
      (defn wrap [get-expr]
        (if (function? brightness)
          (let [$position (:temp-var comp-state type/vec3 'position)]
            ~(with ,$position ,position
              ,(get-expr $position (brightness $position))))
          (get-expr position brightness)))

      (def [function shadow-args] (if shadow
        (if (or (= shadow 0) (= shadow true))
          ['cast_light_hard_shadow []]
          ['cast_light_soft_shadow [shadow]])
        ['cast_light_no_shadow []]))

      (wrap (fn [position brightness]
        ~(extend ,globals/lights
          (,function ,globals/P ,globals/normal ,position ,color ,brightness ,;shadow-args)
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

(defn point/new [position color brightness shadow]
  (table/setproto
    @{:position position
      :color color
      :brightness brightness
      :shadow shadow}
    point-proto))

(defn ambient/new [color brightness]
  (table/setproto
    @{:color color
      :brightness brightness}
    ambient-proto))
