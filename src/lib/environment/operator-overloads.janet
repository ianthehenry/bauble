(use judge)
(import ../../jlsl/prelude :as jlsl)

(defn- numeric-vec? [x]
  (and (or (tuple? x) (array? x)) (all |(or (number? $) (numeric-vec? $)) x)))

(test (numeric-vec? 1) false)
(test (numeric-vec? []) true)
(test (numeric-vec? [1 2]) true)
(test (numeric-vec? [1 [2 3]]) true)
(test (numeric-vec? [1 2 :foo]) false)
(test (numeric-vec? [1 2 :foo 3]) false)

(defn- compatible? [a b]
  (and (numeric-vec? a) (numeric-vec? b) (= (length a) (length b))))

(defn- match-type [source dest]
  (if (= (type source) (type dest))
    dest
    (cond
      (tuple? source) (tuple/slice dest)
      (array? source) (array/slice dest)
      (errorf "unknown vec type %q" source))))

(defn- apply-unary [op a]
  (cond
    (numeric-vec? a) (match-type a (map |(op $) a))
    (op a)))

(defn- apply-binary [op a b]
  (def na? (numeric-vec? a))
  (def nb? (numeric-vec? b))
  (cond
    (and na? nb?)
      (if (= (length a) (length b))
        (match-type a (map (partial apply-binary op) a b))
        (errorf "vector length mismatch: %q %q" a b))
    (and na? (number? b)) (match-type a (map |(apply-binary op $ b) a))
    (and nb? (number? a)) (match-type b (map |(apply-binary op a $) b))
    (op a b)))

(defn- make-variadic [op id]
  (fn [& args]
    (case (length args)
      0 id
      1 (apply-unary op ;args)
      2 (apply-binary op ;args)
      (reduce2 |(apply-binary op $0 $1) args))))

(def + `(+ & xs)

Overloaded to work with tuples, arrays, and expressions.
`
(make-variadic jlsl/+ 0))

(def - `(- & xs)

Overloaded to work with tuples, arrays, and expressions.
`
(make-variadic jlsl/- 0))

(def / `(/ & xs)

Overloaded to work with tuples, arrays, and expressions.
`
(make-variadic jlsl// 1))

(def * `(* & xs)

Overloaded to work with tuples, arrays, and expressions.
`
(make-variadic jlsl/* 1))

(test (+) 0)
(test (+ 1) 1)
(test (+ [1 2] 3) [4 5])
(test (+ [1 2] 3 4) [8 9])
(test (+ [1 2] 3 [4 5]) [8 10])
(test (+ [1 2] 3 @[4 5]) [8 10])
(test (+ @[1 2] 3 [4 5]) @[8 10])
(test (+ 1 [2 3]) [3 4])
(test (+ 1 [2 3] @[4 5]) [7 9])
(test (+ 1 @[2 3] [4 5]) @[7 9])
(test-error (+ [1 2] [1 2 3]) "vector length mismatch: (1 2) (1 2 3)")
(test (+ [[1 2] 3] [4 5]) [[5 6] 8])
