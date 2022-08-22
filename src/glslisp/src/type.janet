(def bool :bool)
(def vec2 :vec2)
(def vec3 :vec3)
(def vec4 :vec4)
(def float :float)
(defn array [len t] [:array len t])
(def unknown :unknown)

# (defn is-array? [t] (and (tuple? t) (= (length t) 3) (= (first t) :array)))
