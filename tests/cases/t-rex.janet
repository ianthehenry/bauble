(def lights [;lights (ambient (occlusion * 0.25))])

(def wedge
(box [100 24 107]
| rotate :x 0.249
| mirror :y
| move :z 79
| move :y (ss p.z 100 0 0 -54)
| rotate :x -0.45
| move :y 40
))

(def eye-ridge (cylinder :x 28 8 :r 8 | move :x (cos (p.y / 23) * 10)))

(def eye (sphere 11 | move [28 35 -26]))

(def skull
(morph (ss p.z 75 -75 0.75 0.25)
  (box [23 50 75] :r 25)
  (ellipsoid [50 50 75])
| union :r 10
  (sphere 23 | move [0 30 42])
| subtract wedge :r (ss p.z 100 0 10 2)
| union :r 15
  (eye-ridge
  | rotate :z 1.29
  | rotate :y 0.46
  | move [12 48 -30])
| subtract :r 7 eye
| mirror :x :r 3
))

(def head
  (skull
  | union :r 3 (offset (intersect (offset wedge 20) skull :r 30) (ss p.z 75 -75 4 0))
  ))

(def neck (union :r 20
  (ellipsoid [(ss p.y 50 0 40 25) 53 35])
  (ellipsoid [25 40 25] | move [0 -56 -10])
| rotate :x -0.43
| move [0 18 -44]
))

(def body (ellipsoid [50 60 80]
  | rotate :x 0.43
  | move [0 -65 -151]
))

(def tail (cone :-z 40 240 :r 10
  | move :y (sin (p.z / 75) * 40)
  #| move :x (sin (p.z / 75) * 40)
  | move [0 -80 -414]

))

(defn plastic [shape color]
  (shape
  | shade (remap+ normal / 2 + color / 2) :gloss (10 + 10 * perlin+ (p * 5))
  | fresnel :exponent 0.5 blue
  | fresnel red))

(def tooth (cone :y (ss p.y 0 20 5 10) 10 :r 2 | move :z (sin (ss p.y 0 10 0 pi/2) * 2)
| reflect :y))

(defn make-tooth [s angle pos]
  (tooth | scale s | rotate :y angle | move pos))

(def leg (union :r 30
  (ellipsoid [25 50 40] | rotate :z -0.19 :x 0.61 | move [50 -76 -176])
  (ellipsoid [20 40 20] | rotate :z -0.12 :x -0.93 | move [68 -141 -159])
  (ellipsoid [15 40 20] | rotate :z -0.22 :y -1.91 :x 0.93 | move [78 -192 -159])))
(def foot (union :r 5
  (ellipsoid [5 5 40] | rotate :y -0.22 :pivot [0 0 -40] | move [75 -223 -119])
  (ellipsoid [5 5 40] | rotate :y 0.22 :pivot [0 0 -40] | move [75 -223 -119])
  (ellipsoid [5 5 40] | rotate :y 0.00 :pivot [0 0 -40] | move [75 -223 -119])))

(def teeth
  (union
    (make-tooth 1 0 [6 20 68] | rotate :x 0.35 :pivot [5 20 68])
    (make-tooth [1 1 1] -0.94 [16 18 63])
    (make-tooth [1 1.5 1] -0.35 [25 16 53])
    (make-tooth [1 1.2 1] 0.15 [31 12 41])
    (make-tooth [1 1 1] 0.15 [35 6 29])
    | mirror :x
  ))

(def smooth-head (union head (intersect head teeth :r 4 | offset 2) :r 1 | subtract teeth :r 1))
(def eyes (eye | mirror :x | plastic (mix yellow black (dot (normalize [193 -59 200]) normal | step 0.9))))

(union
  (union (smooth-head | slow 0.9)
  neck :r 10
  | plastic green
  | union (teeth | plastic white) eyes
  | rotate :x (sin+ (t / 4 + 7) * -0.05 * tau) :y (sin (t / 11) * 0.1 * tau) :pivot [0 -49 -113])
  (union body tail (mirror (union leg foot :r 10) :x) :r 20 | plastic green)
  :r 10
  | rotate :y pi/8 :x pi/4
  | move [-50 40 0]
  )
