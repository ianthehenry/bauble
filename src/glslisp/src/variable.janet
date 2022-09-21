(def- proto @{
  :type (fn [self] (self :-type))
  :intrinsic-name (fn [self] (self :-name))
  })

(defn new [name &opt type]
  (table/setproto @{:-name name :-type type} proto))

(defn instance? [x]
  (and (= (type x) :table)
       (= (table/getproto x) proto)))
