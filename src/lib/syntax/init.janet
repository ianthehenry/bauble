(use judge)
(import ./dot-syntax)
(import ./pipe-syntax)
(import ./infix-syntax)

(def expand (comp infix-syntax/expand pipe-syntax/expand dot-syntax/expand))

(test (expand '(a + b | sin * 2)) [* [sin [+ a b]] 2])
(test (expand '(a + b | sin * foo bar)) [* [sin [+ a b]] foo bar])
(test (expand '(a + b | + 1 2)) [+ [+ a b] 1 2])
(test (expand '(a + b | sin + 10 | pow 2))
  [pow [+ [sin [+ a b]] 10] 2])

(test (expand '(a + b | - 1)) [- [+ a b] 1])
(test (expand '(foo.x + foo.y * foo.z)) [* [+ [. foo x] [. foo y]] [. foo z]])
(test (expand '(a + b | - 1 _)) [- 1 [+ a b]])
(test (expand '(a + b | - 1 2 + _)) [- 1 2 + [+ a b]])
