(use judge)
(use /src/infix-syntax)

# Sadly, we have to perform macro expansion at compile time...
(def- x (macex1 '(pipe-syntax (box 50 | scale 2) )))
(def- y (macex x))
(deftest "pipe-syntax syntax"
  (test x [do [-> [box 50] [scale 2]]])
  (test y [do [scale [box 50] 2]]))

(def- x (macex1 '(pipe-syntax (box | scale 2))))
(def- y (macex x))
(deftest "first argument is a single atom"
  (test x [do [-> box [scale 2]]])
  (test y [do [scale box 2]]))

(def- x (macex1 '(pipe-syntax (box | foo | bar 2))))
(def- y (macex x))
(deftest "intermediate argument is a single atom"
  (test x [do [-> box [foo] [bar 2]]])
  (test y [do [bar [foo box] 2]]))

(def- x (macex1 '(pipe-syntax (box | foo 2 | bar))))
(def- y (macex x))
(deftest "final argument is a single atom"
  (test x [do [-> box [foo 2] [bar]]])
  (test y [do [bar [foo box 2]]]))

(def- x (macex1 '(pipe-syntax (box | morph (sphere 50 | scale) | rotate | scale 2))))
(def- y (macex x))
(deftest "nested pipes"
  (test x [do [-> box [morph [-> [sphere 50] [scale]]] [rotate] [scale 2]]])
  (test y [do [scale [rotate [morph box [scale [sphere 50]]]] 2]]))

(def- x (macex1 '(pipe-syntax (x + y))))
(def- y (macex x))
(deftest "infix operators"
  (test x [do [-> x [+ y]]])
  (test y [do [+ x y]]))

(def- x (macex1 '(pipe-syntax (+ x y z))))
(def- y (macex x))
(deftest "prefix math still works normally"
  (test x [do [+ x y z]])
  (test y [do [+ x y z]]))


(def- x (macex1 '(pipe-syntax (x + sin y))))
(def- y (macex x))
(deftest "infix operators will invoke multiple arguments"
  (test x [do [-> x [+ [sin y]]]])
  (test y [do [+ x [sin y]]]))


(def- x (macex1 '(pipe-syntax (x + (sin y) + z))))
(def- y (macex x))
(deftest "infix operators will not invoke single arguments"
  (test x [do [-> x [+ [sin y]] [+ z]]])
  (test y [do [+ [+ x [sin y]] z]]))

(def- x (macex1 '(pipe-syntax (x + sin y | abs))))
(def- y (macex x))
(deftest "infix operators chain with pipe"
  (test x [do [-> x [+ [sin y]] [abs]]])
  (test y [do [abs [+ x [sin y]]]]))

(def- x (macex1 '(pipe-syntax (x + sin y -))))
(def- y (macex x))
(deftest "you can end with an infix operator"
  (test x [do [-> x [+ [sin y]] [-]]])
  (test y [do [- [+ x [sin y]]]]))

(def- x (macex1 '(pipe-syntax (x + y * z))))
(def- y (macex x))
(deftest "no operator precedence"
  (test x [do [-> x [+ y] [* z]]])
  (test y [do [* [+ x y] z]]))
