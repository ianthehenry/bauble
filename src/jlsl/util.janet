(use judge)
(use module)
(import pat)
(import ../util :prefix "" :export true)

(defn >> [f g] |(g (f ;$&)))
(defn << [f g] |(f (g ;$&)))

(defn contents= [a b]
  (and
    (= (length a) (length b))
    (all = a b)))

# useful for invoking macros as regular functions
(defn call [f & args]
  (f ;args))

(defmacro get-or-put [t k v]
  (with-syms [$t $k $v]
    ~(let [,$t ,t ,$k ,k]
      (if (,has-key? ,$t ,$k)
        (,in ,$t ,$k)
        (let [,$v ,v]
          (,put ,$t ,$k ,$v)
          ,$v)))))

(test-macro (get-or-put @{} 1 2)
  (let [<1> @{} <2> 1]
    (if (@has-key? <1> <2>)
      (@in <1> <2>)
      (let [<3> 2]
        (@put <1> <2> <3>)
        <3>))))

(defmodule ref
  (defn new [& args]
    (case (length args)
      0 @[]
      1 @[(in args 0)]
      (error "too many arguments to ref/new")))
  (defn get [t &opt dflt] (if (empty? t) dflt (in t 0)))
  (defn set [t x] (put t 0 x))
  (defmacro get-or-put [t v]
    (with-syms [$t $v]
      ~(let [,$t ,t]
        (if (,empty? ,$t)
          (let [,$v ,v]
            (,put ,$t 0 ,$v)
            ,$v)
          (,in ,$t 0))))))

(test-macro (ref/get-or-put x y)
  (let [<1> x]
    (if (@empty? <1>)
      (let [<2> y]
        (@put <1> 0 <2>)
        <2>)
      (@in <1> 0))))

(defn zip [& xs]
  (assert (> (length xs) 0) "nothing to zip")
  (def len (length (in xs 0)))
  (assertf (all |(= len (length $)) xs) "zip length mismatch")
  (apply map tuple xs))

(defn tmap [& args]
  (tuple/slice (map ;args)))

(defn const [x] (fn [&] x))
