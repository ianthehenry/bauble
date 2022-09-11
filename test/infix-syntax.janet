(use judge)
(use /src/infix-syntax)

# Sadly, we have to perform macro expansion at compile time...
(def- x (macex1 '(pipe-syntax (box 50 | scale 2) )))
(def- y (macex x))
(test "pipe-syntax syntax"
  (expect x [do [-> [box 50] [scale 2]]])
  (expect y [do [scale [box 50] 2]]))

(def- x (macex1 '(pipe-syntax (box | scale 2))))
(def- y (macex x))
(test "first argument is a single atom"
  (expect x [do [-> box [scale 2]]])
  (expect y [do [scale box 2]]))

(def- x (macex1 '(pipe-syntax (box | foo | bar 2))))
(def- y (macex x))
(test "intermediate argument is a single atom"
  (expect x [do [-> box [foo] [bar 2]]])
  (expect y [do [bar [foo box] 2]]))

(def- x (macex1 '(pipe-syntax (box | foo 2 | bar))))
(def- y (macex x))
(test "final argument is a single atom"
  (expect x [do [-> box [foo 2] [bar]]])
  (expect y [do [bar [foo box 2]]]))

(def- x (macex1 '(pipe-syntax (box | morph (sphere 50 | scale) | rotate | scale 2))))
(def- y (macex x))
(test "nested pipes"
  (expect x [do [-> box [morph [-> [sphere 50] [scale]]] [rotate] [scale 2]]])
  (expect y [do [scale [rotate [morph box [scale [sphere 50]]]] 2]]))

(def- x (macex1 '(pipe-syntax (x + y))))
(def- y (macex x))
(test "infix operators"
  (expect x [do [-> x [+ y]]])
  (expect y [do [+ x y]]))

(def- x (macex1 '(pipe-syntax (+ x y z))))
(def- y (macex x))
(test "prefix math still works normally"
  (expect x [do [+ x y z]])
  (expect y [do [+ x y z]]))


(def- x (macex1 '(pipe-syntax (x + sin y))))
(def- y (macex x))
(test "infix operators will invoke multiple arguments"
  (expect x [do [-> x [+ [sin y]]]])
  (expect y [do [+ x [sin y]]]))


(def- x (macex1 '(pipe-syntax (x + (sin y) + z))))
(def- y (macex x))
(test "infix operators will not invoke single arguments"
  (expect x [do [-> x [+ [sin y]] [+ z]]])
  (expect y [do [+ [+ x [sin y]] z]]))

(def- x (macex1 '(pipe-syntax (x + sin y | abs))))
(def- y (macex x))
(test "infix operators chain with pipe"
  (expect x [do [-> x [+ [sin y]] [abs]]])
  (expect y [do [abs [+ x [sin y]]]]))

(def- x (macex1 '(pipe-syntax (x + sin y -))))
(def- y (macex x))
(test "you can end with an infix operator"
  (expect x [do [-> x [+ [sin y]] [-]]])
  (expect y [do [- [+ x [sin y]]]]))

(def- x (macex1 '(pipe-syntax (x + y * z))))
(def- y (macex x))
(test "no operator precedence"
  (expect x [do [-> x [+ y] [* z]]])
  (expect y [do [* [+ x y] z]]))
