(use judge)
(use /src/index)
(import /src/type)

(defn c&t [expr]
  (def free-vars @{})
  (def result (compile! |(do (set (free-vars $) true) (string ($ :name))) expr))
  (if (not (empty? free-vars))
    (errorf "expression %p had free variables %p" expr free-vars))
  [(typecheck expr) result])

(defn cft [expr]
  (def free-vars @{})
  (def result (compile! |(do (set (free-vars $) true) (string ($ :name))) expr))
  [(typecheck expr) (map |[($ :type) ($ :name)] (keys free-vars)) result])

(def p (variable 'p type/vec3))

(test something
  (expect (c&t 10) [:float "10.0"])
  (expect (c&t 0.5) [:float "0.500000"])
  (expect (c&t '(+ 1 2)) [:float "(1.0 + 2.0)"])
  (expect (c&t '(+ 1 2 3 4)) [:float "(((1.0 + 2.0) + 3.0) + 4.0)"])
  (expect (c&t '[1 2 3]) [:vec3 "vec3(1.0, 2.0, 3.0)"])
  (expect (c&t [1 2 3]) [:vec3 "vec3(1.0, 2.0, 3.0)"])
  (expect (c&t '(- 1)) [:float "(-1.0)"])
  (expect (c&t '(/ 2)) [:float "(1.0 / 2.0)"])
  (expect (c&t '(cos 1)) [:float "cos(1.0)"])
  (expect (c&t '(abs [1 2])) [:vec2 "abs(vec2(1.0, 2.0))"])
  (expect (c&t '(and (< 1 2) (> 3 4) (= 2 2))) [:bool "(((1.0 < 2.0) && (3.0 > 4.0)) && (2.0 == 2.0))"])
  (expect (c&t '(. [1 2 3] :xy)) [:vec2 "vec3(1.0, 2.0, 3.0).xy"])
  (expect (c&t '(. [1 2] :xrya)) [:vec4 "vec2(1.0, 2.0).xrya"])
  (expect (c&t '(. [1 2] :x)) [:float "vec2(1.0, 2.0).x"])
  (expect (cft p) [:vec3 [[:vec3 p]] "p"])
  (expect (cft ~(atan (. ,p :x) (. ,p :y))) [:float [[:vec3 p]] "atan(p.x, p.y)"])
  (expect (cft [p 1 2]) [:vec3 [[:vec3 p]] "vec3(p, 1.0, 2.0)"])
  (expect (cft ~(- (length ,p) 10)) [:float [[:vec3 p]] "(length(p) - 10.0)"])
  )

(test "invalid types"
  (expect-error (c&t [1]) "there is no vec1"))
