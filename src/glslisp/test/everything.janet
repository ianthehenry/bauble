(use judge)
(use ../src/index)
(import ../src/type)
(import ../src/variable)
(import ../src/comp-state)

(defn c&t [expr]
  (def state (:new-scope (comp-state/new {})))
  (def result (compile! state expr))
  [(typecheck expr)
   ;(state :statements)
   result
   ;(map |[(:type $) (:intrinsic-name $)] (keys (state :free-variables)))])

(def p (variable/new 'p type/vec3))
(def lights (variable/new 'lights (type/array 1 type/float)))

(deftest something
  (test (c&t 10) [:float "10.0"])
  (test (c&t 0.5) [:float "0.500000"])
  (test (c&t '(+ 1 2)) [:float "(1.0 + 2.0)"])
  (test (c&t '(+ 1 (+ 2 3))) [:float "(1.0 + (2.0 + 3.0))"])
  (test (c&t '[1 2 3]) [:vec3 "vec3(1.0, 2.0, 3.0)"])
  (test (c&t [1 2 3]) [:vec3 "vec3(1.0, 2.0, 3.0)"])
  (test (c&t '(- 1)) [:float "(-1.0)"])
  (test (c&t '(/ 2)) [:float "(1.0 / 2.0)"])
  (test (c&t '(cos 1)) [:float "cos(1.0)"])
  (test (c&t '(abs [1 2])) [:vec2 "abs(vec2(1.0, 2.0))"])
  (test (c&t '(and (< 1 2) (and (> 3 4) (= 2 2))))
    [:bool
     "((1.0 < 2.0) && ((3.0 > 4.0) && (2.0 == 2.0)))"])
  (test (c&t '(. [1 2 3] :xy)) [:vec2 "vec3(1.0, 2.0, 3.0).xy"])
  (test (c&t '(. [1 2] :xrya)) [:vec4 "vec2(1.0, 2.0).xrya"])
  (test (c&t '(. [1 2] :x)) [:float "vec2(1.0, 2.0).x"])
  (test (c&t p) [:vec3 "p" [:vec3 p]])
  (test (c&t ~(atan (. ,p :x) (. ,p :y))) [:float "atan(p.x, p.y)" [:vec3 p]])
  (test (c&t [p 1 2]) [:vec3 "vec3(p, 1.0, 2.0)" [:vec3 p]])
  (test (c&t ~(- (length ,p) 10)) [:float "(length(p) - 10.0)" [:vec3 p]])
  )

# TODO: this is currently broken, obviously
(deftest "unused variables in with expressions don't get assigned"
  (test (c&t ~(with ,p 10 (+ 1 2))) [:float "vec3 p1 = 10.0;" "(1.0 + 2.0)"]))

# TODO: this is currently broken, obviously
(deftest "with elision can cause skipped numbers"
  (test (c&t ~(with ,p 10 (with ,p 20 (+ ,p 1))))
    [:vec3
     "vec3 p1 = 10.0;"
     "vec3 p2 = 20.0;"
     "(p2 + 1.0)"]))

(deftest with
  (test (c&t ~(with ,p 10 ,p)) [:vec3 "vec3 p1 = 10.0;" "p1"])
  (test (c&t ~(with ,p (+ ,p 1) (+ ,p 1)))
    [:vec3
     "vec3 p1 = (p + 1.0);"
     "(p1 + 1.0)"
     [:vec3 p]])
  (test (c&t ~(with ,p (+ ,p 1) (with ,p (+ ,p 2) (+ ,p 3))))
    [:vec3
     "vec3 p1 = (p + 1.0);"
     "vec3 p2 = (p1 + 2.0);"
     "(p2 + 3.0)"
     [:vec3 p]]))

(deftest "complicated expression involving with"
  (test (c&t ~(mix
    (with ,p (- ,p [10 0 0])
      (s3d_box_0 ,p (vec3 50)))
    (with ,p (- ,p [20 0 0])
      (- (length ,p) 62))
    0.5))
    [:vec3
     "vec3 p1 = (p - vec3(10.0, 0.0, 0.0));"
     "vec3 p2 = (p - vec3(20.0, 0.0, 0.0));"
     "mix(s3d_box_0(p1, vec3(50.0)), (length(p2) - 62.0), 0.500000)"
     [:vec3 p]]))

(deftest "invalid types"
  (test-error (c&t [1]) "there is no vec1"))

(deftest "extending arrays"
  (test (c&t ~(extend ,lights 10 ,lights))
    [[:array 1 :float]
     "float lights1[2];"
     "lights1[0] = lights[0];"
     "lights1[1] = 10.0;"
     "lights1"
     [[:array 1 :float] lights]]))
