(use judge)
(use ../util)

(defn- op-node? [ast] (case ast
  '+ true
  '* true
  '/ true
  '- true))

(defn expand [ast]
  (prewalk (fn [ast]
    (when (ptuple? ast)
      (def subject-length (find-index op-node? ast 1))
      (unless subject-length (break ast))

      (var subject
        (case subject-length
          1 (in ast 0)
          (keep-syntax! ast (tuple/slice ast 0 subject-length))))

      (var current-op-index subject-length)
      (forever
        (def expr-start (inc current-op-index))
        (def next-op-index (find-index op-node? ast expr-start))
        (def this-length (- (or next-op-index (length ast)) expr-start))
        (set subject (keep-syntax! ast
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

(deftest "expand preserves sourcemaps"
  (def form '(a + b))

  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) form) `
    (48 14)
  `
    [a + b])
  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) (expand form)) `
    (48 14)
  `
    [+ a b])

  (def form '(a b + c d))

  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) form) `
    (59 14)
  `
    [a b + c d])
  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) (expand form)) `
    (59 14)
    (59 14)
    (-1 -1)
  `
    [+ [a b] [c d]]))
