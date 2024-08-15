(use judge)
(use ./util)

(def- pipe (gensym))

(defn- pipe-node? [ast] (and (ptuple? ast) (= (first ast) pipe)))

(def- unshort-fn (partial prewalk (fn [ast]
  (if (ptuple? ast)
    (keep-syntax! ast (catseq [ast :in ast]
      (if (and (ptuple? ast) (= (first ast) 'short-fn))
        [(keep-syntax! ast [pipe]) ;(tuple/slice ast 1)]
        [ast]
        )))
    ast)
  )))

(defn expand [ast]
  (prewalk (fn [ast]
    (when (ptuple? ast)
      (def subject-length (find-index pipe-node? ast 0))
      (unless subject-length (break ast))

      (var subject
        (case subject-length
          0 (error "form cannot begin with a pipe")
          1 (in ast 0)
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

(test (expand '(foo)) [foo])
(test (expand '(foo | bar)) [bar foo])
(test (expand '(foo x | bar)) [bar [foo x]])
(test (expand '(foo | bar baz)) [bar foo baz])
(test (expand '(foo | bar _ baz)) [bar foo baz])
(test (expand '(foo | bar baz _)) [bar baz foo])
(test (expand '(foo | bar baz | qux)) [qux [bar foo baz]])
(test (expand '(foo | bar baz _ | qux)) [qux [bar baz foo]])
(test (expand '(foo x y | bar baz _ | qux)) [qux [bar baz [foo x y]]])
(test (expand '((foo bar) | baz)) [baz [foo bar]])
(test (expand '((foo | bar) | baz)) [baz [bar foo]])
(test (expand '(foo | (bar | baz))) [[baz bar] foo])
(test (expand '(foo x | (bar | baz) y)) [[baz bar] [foo x] y])
(test (expand '(foo x | (bar | baz 1 _) y _)) [[baz 1 bar] y [foo x]])

(deftest "expand preserves sourcemaps"
  (def form '(foo | bar baz | qux))

  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) form) `
    (85 14)
    (85 19)
    (85 29)
  `
    [foo [short-fn bar] baz [short-fn qux]])
  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) (expand form)) `
    (85 29)
    (85 19)
  `
    [qux [bar foo baz]])

  (def form '(foo x | bar baz | qux))

  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) form) `
    (99 14)
    (99 21)
    (99 31)
  `
    [foo
     x
     [short-fn bar]
     baz
     [short-fn qux]])
  (test-stdout (prewalk (fn [ast] (when (tuple? ast) (pp (tuple/sourcemap ast))) ast) (expand form)) `
    (99 31)
    (99 21)
    (99 14)
  `
    [qux [bar [foo x] baz]]))


(test (expand '(a + b | sin * 2)) [sin [a + b] * 2])
(test (expand '(a + b | sin * foo bar)) [sin [a + b] * foo bar])
(test (expand '(a + b | + 1 2)) [+ [a + b] 1 2])
(test (expand '(a + b | sin + 2 | pow 2)) [pow [sin [a + b] + 2] 2])
