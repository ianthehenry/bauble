(def- proto @{})

(defn instance? [x]
  (and (= (type x) :table) (= (table/getproto x) proto)))

(defn new [position color brightness radius]
  (table/setproto
    @{:position position
      :color color
      :brightness brightness
      :radius radius}
    proto))
