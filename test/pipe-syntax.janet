(use judge)
(use /src/pipe-syntax)

# Sadly, we have to perform macro expansion at compile time...
(def- x (macex1 '(pipe (box 50 | scale 2) )))
(def- y (macex x))
(test "pipe syntax"
  (expect x [do [-> [box 50] [scale 2]]])
  (expect y [do [scale [box 50] 2]]))

(def- x (macex1 '(pipe (box | scale 2))))
(def- y (macex x))
(test "first argument is a single atom"
  (expect x [do [-> box [scale 2]]])
  (expect y [do [scale box 2]]))

(def- x (macex1 '(pipe (box | something | scale 2))))
(def- y (macex x))
(test "intermediate argument is a single atom"
  (expect x [do [-> box [something] [scale 2]]])
  (expect y [do [scale [something box] 2]]))

(def- x (macex1 '(pipe (box | morph (sphere 50 | scale) | rotate | scale 2))))
(def- y (macex x))
(test "nested pipes"
  (expect x [do [-> box [morph [-> [sphere 50] [scale]]] [rotate] [scale 2]]])
  (expect y [do [scale [rotate [morph box [scale [sphere 50]]]] 2]]))
