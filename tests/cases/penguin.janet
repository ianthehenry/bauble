(def yellow (mix yellow orange 0.57))

(def head
  (union :r 11
    (ellipsoid [25 35 25] | move [0 113 0] | rotate :x -1.02 :pivot [0 90 0] | shade black)
    (cone :z 5 50 :r 1
      | resurface (shade black | union (ellipsoid [7 1.5 36] | move [0 -1 -4] | shade yellow))
      | move [0 (ss (distance p.z 25) 0 50 0 -10) 0] | move [0 126 43])
  ))
(def body-shape (half-space :z | rotate :x -0.23 | move [0 0 22]))
(def body
  (ellipsoid [50 100 50]
  | resurface
    ((resurface
      body-shape
      (union :r 10 (shade white) (ellipsoid [144 50 50] | move [0 94 56] | shade yellow)))
    | union ((map-distance body-shape @-) | shade black))))
(def eyes (sphere 4 | move [10 125 41] | mirror :x
  | shade (white * (step 0.95 (dot (normalize (- camera P)) normal)))
))
(def floor (ground -100))
(def size [5 60 (ss p.y 60 0 20 10)])
(def wing (ellipsoid size | move
   [(ss (abs p.y) 0 101 10 0) 0 (ss (abs p.y) 0 80 0 -5)]
| rotate :z -0.05 :x -0.19))
(def feet (ellipsoid ([20 20 40] * 0.61) | move [15 -100 23] | rotate :y -0.47 | mirror :x | subtract floor :r 3 | shade black))
(union :r 10 head body
| union eyes (floor | shade gray) feet
| union :r (ss p.y 60 0 10 0) (wing | move [45 0 0] | mirror :x | shade black)
  | slow 0.9
)
(def lights [(in lights 0) (ambient (occlusion * 0.5))])
