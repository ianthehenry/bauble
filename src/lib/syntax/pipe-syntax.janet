(use judge)
(use ../util)

(def- pipe (gensym))

(defn- pipe-node? [ast] (and (ptuple? ast) (= (first ast) pipe)))

(def- unshort-fn (partial prewalk (fn [ast]
  (if (tuple? ast)
    (keep-syntax! ast (catseq [ast :in ast]
      (if (and (ptuple? ast) (= (first ast) 'short-fn))
        [(keep-syntax! ast [pipe]) ;(tuple/slice ast 1)]
        [ast]
        )))
    ast)
  )))

(defn expand [ast]
  (prewalk (fn [ast]
    (when (tuple? ast)
      (def subject-length (find-index pipe-node? ast 0))
      (unless subject-length (break ast))

      (var subject
        (cond
          (= subject-length 0) (error "form cannot begin with a pipe")
          (and (= subject-length 1) (ptuple? ast)) (in ast 0)
          (keep-syntax! ast (tuple/slice ast 0 subject-length))))

      (var current-pipe-index subject-length)
      (forever
        (def expr-start (inc current-pipe-index))
        (def next-pipe-index (find-index pipe-node? ast expr-start))
        (def replacement-index (find-index |(= $ '_) ast expr-start next-pipe-index))
        (set subject (keep-syntax! (in ast current-pipe-index)
          (if replacement-index
            [;(tuple/slice ast expr-start replacement-index)
             subject
             ;(tuple/slice ast (inc replacement-index) next-pipe-index)]
            [(in ast expr-start)
             subject
             ;(tuple/slice ast (inc expr-start) next-pipe-index)])))

        (unless next-pipe-index (break))
        (set current-pipe-index next-pipe-index))
      (break subject))
    ast)
    (unshort-fn ast)))

(test (unshort-fn '(foo bar)) [foo bar])
(test (unshort-fn '(foo | bar)) [foo [<1>] bar])
(test (unshort-fn '(foo | bar baz)) [foo [<1>] bar baz])
(deftest "unshort-fn preserves sourcemaps"
  (def form '(foo))
  (test (tuple/sourcemap form) [54 14])
  (test (tuple/sourcemap (unshort-fn form)) [54 14])

  (test (map |[(if (tuple? $) (tuple/sourcemap $)) $]
    (unshort-fn '(foo | bar)))
    @[[nil foo] [[59 23] [<1>]] [nil bar]])

  (test (map |[(if (tuple? $) (tuple/sourcemap $)) $]
    (unshort-fn '(foo | bar baz)))
    @[[nil foo]
      [[63 23] [<1>]]
      [nil bar]
      [nil baz]]))

(defmacro- expand: [form] (expand form))
(defmacro*- test-expansion [form & args]
  ~(test-macro (expand: ,form) ,;args))

(test-expansion (foo)
  (foo))
(test-expansion (foo | bar)
  (bar foo))
(test-expansion (foo x | bar)
  (bar (foo x)))
(test-expansion (foo | bar baz)
  (bar foo baz))
(test-expansion (foo | bar _ baz)
  (bar foo baz))
(test-expansion (foo | bar baz _)
  (bar baz foo))
(test-expansion (foo | bar baz | qux)
  (qux (bar foo baz)))
(test-expansion (foo | bar baz _ | qux)
  (qux (bar baz foo)))
(test-expansion (foo x y | bar baz _ | qux)
  (qux (bar baz (foo x y))))
(test-expansion ((foo bar) | baz)
  (baz (foo bar)))
(test-expansion ((foo | bar) | baz)
  (baz (bar foo)))
(test-expansion (foo | (bar | baz))
  ((baz bar) foo))
(test-expansion (foo x | (bar | baz) y)
  ((baz bar) (foo x) y))
(test-expansion (foo x | (bar | baz 1 _) y _)
  ((baz 1 bar) y (foo x)))

(deftest "expand preserves sourcemaps"
  (def form '(foo | bar baz | qux))

  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) form) `
    (103 14)
    (103 19)
    (103 29)
  `
    [foo [short-fn bar] baz [short-fn qux]])
  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) (expand form)) `
    (103 29)
    (103 19)
  `
    [qux [bar foo baz]])

  (def form '(foo x | bar baz | qux))

  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) form) `
    (117 14)
    (117 21)
    (117 31)
  `
    [foo
     x
     [short-fn bar]
     baz
     [short-fn qux]])
  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) (expand form)) `
    (117 31)
    (117 21)
    (117 14)
  `
    [qux [bar [foo x] baz]]))

(test-expansion (a + b | sin * 2)
  (sin (a + b) * 2))
(test-expansion (a + b | sin * foo bar)
  (sin (a + b) * foo bar))
(test-expansion (a + b | + 1 2)
  (+ (a + b) 1 2))
(test-expansion (a + b | sin + 2 | pow 2)
  (pow (sin (a + b) + 2) 2))
(test-expansion [1 1 1 | normalize]
  (normalize [1 1 1]))
(test-expansion [1 1 1 | normalize foo bar | baz]
  (baz (normalize [1 1 1] foo bar)))
(test-expansion [1 | foo]
  (foo [1]))
(test-expansion [1 2 | foo bar _]
  (foo bar [1 2]))
