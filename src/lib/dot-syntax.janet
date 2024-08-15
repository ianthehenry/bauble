(use judge)

(defn- string/find-last [str c]
  (var i (dec (length str)))
  (var result nil)
  (while (>= i 0)
    (when (= (in str i) c)
      (set result i)
      (break))
    (-- i))
  result)

(def expand (partial prewalk (fn [ast]
  (when (and (symbol? ast))
    (when-let [dot-index (string/find-last ast (chr "."))]
      (case dot-index
        # TODO: I dunno, is this anything?
        0 ast # ~(. ,(symbol/slice ast 1))
        (dec (length ast)) ast
        (break ~(. ,(symbol/slice ast 0 dot-index) ,(symbol/slice ast (+ dot-index 1)))))))
  ast)))

(test (expand ~(+ 1 2)) [+ 1 2])
(test (expand ~(+ foo.bar 2)) [+ [. foo bar] 2])
(test (expand ~(+ foo . bar 2)) [+ foo . bar 2])
(test (expand ~(.bar 2)) [.bar 2])
(test (expand ~(bar. 2)) [bar. 2])
(test (expand ~(+ foo.bar.baz 2)) [+ [. [. foo bar] baz] 2])
(test (expand ~(+ foo (+ bar.xyz 3))) [+ foo [+ [. bar xyz] 3]])

(deftest "preserves sourcemaps"
  (def form '(+ 1 foo.x))
  (assert (= (tuple/sourcemap form) (tuple/sourcemap (expand form)))))
