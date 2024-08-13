(def spots (p * 0.103 | perlin+ + 0.15))
(def outline (0.5 - spots | abs | step 0.016))
(def brown (hsv 0.01 0.63 0.5))
(def tan   (hsv 0.07 0.63 0.9))
(def leppard (shade (mix brown tan (round spots) * (max outline 0.05))))
(def eye (sphere 5 | shade white | union (sphere 2 | move :z 4 | shade [0.1 0.1 0.1])))
(line [20 0 32] [50 -50 50] 5
| mirror :x :z
| union :r 10
  (line [0 9 -31] [0 -14 -90] 5)
  (box [33 20 41] :r 10)
  (sphere 20 | move :z 51 :y 24)
| resurface leppard
| union
  (eye | rotate :x 0.63 :y -0.47 | move [7 34 67])
  (eye | scale 0.95 | rotate :x -0.09 :y 0.52 | move [-7 33 67])
  (ground -55 | shade dark-gray)
| scale 1.5
| move :y 20)
