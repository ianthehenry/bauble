(use judge)
(use ../util)

(defn- op-node? [ast] (case ast
  '+ true
  '* true
  '/ true
  '- true))

(defn with-sourcemap [source dest]
  (tuple/setmap dest ;(tuple/sourcemap source))
  dest)

(defn expand [ast]
  (prewalk (fn [ast]
    (when (tuple? ast)
      (def subject-length (find-index op-node? ast 1))
      (unless subject-length (break ast))

      (var subject
        (if (and (= subject-length 1) (ptuple? ast))
          (in ast 0)
          (keep-syntax! ast (tuple/slice ast 0 subject-length))))

      (var current-op-index subject-length)
      (forever
        (def expr-start (inc current-op-index))
        (def next-op-index (find-index op-node? ast expr-start))
        (def this-length (- (or next-op-index (length ast)) expr-start))
        (set subject (with-sourcemap ast
          [(in ast current-op-index)
           subject
           ;(case this-length
            0 []
            1 [(in ast expr-start)]
            [(tuple/slice ast expr-start next-op-index)])]))
        (unless next-op-index (break))
        (set current-op-index next-op-index))
      (break subject))
    ast)
    ast))

(test (expand '(a + b)) [+ a b])
(test (expand '(a + b c)) [+ a [b c]])
(test (expand '(a + b c + d)) [+ [+ a [b c]] d])
(test (expand '(a + b / + c)) [+ [/ [+ a b]] c])
(test (expand '(a - 1)) [- a 1])
(test (expand '(+ a b)) [+ a b])
(test (expand '(+ a b + c)) [+ [+ a b] c])

(test-stdout (pp (expand '[1 2 + 3])) `
  (+ [1 2] 3)
`)
(test-stdout (pp (expand '[1 (2 3) 4 + foo bar - 1])) `
  (- (+ [1 (2 3) 4] (foo bar)) 1)
`)
(test-stdout (pp (expand '[1 + 2])) `
  (+ [1] 2)
`)
(test-stdout (pp (expand '[1 -])) `
  (- [1])
`)
(test-stdout (pp (expand '(1 2 + 3))) `
  (+ (1 2) 3)
`)

(deftest "expand preserves sourcemaps"
  (def form '(a + b))

  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) form) `
    (68 14)
  `
    [a + b])
  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) (expand form)) `
    (68 14)
  `
    [+ a b])

  (def form '(a b + c d))

  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) form) `
    (79 14)
  `
    [a b + c d])
  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) (expand form)) `
    (79 14)
    (79 14)
    (-1 -1)
  `
    [+ [a b] [c d]])

  (def form '[a b + c d])

  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) form) `
    (92 14)
  `
    [a b + c d])
  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) (expand form)) `
    (92 14)
    (92 14)
    (-1 -1)
  `
    [+ [a b] [c d]])
  )
