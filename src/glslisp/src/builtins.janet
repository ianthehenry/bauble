(use ./util)

(defn- map-vec-l [f vec x]
  (def len (length vec))
  (def result (array/new len))
  (for i 0 len
    (set (result i) (f (vec i) x)))
  result)

(defn- map-vec-r [f x vec]
  (def len (length vec))
  (def result (array/new len))
  (for i 0 len
    (set (result i) (f x (vec i))))
  result)

(defn- zip-with2 [f a b]
  (def len (length a))
  (def result (array/new len))
  (for i 0 len
    (set (result i) (f (a i) (b i))))
  result)

(defn- zip-with3 [f a b c]
  (def len (length a))
  (def result (array/new len))
  (for i 0 len
    (set (result i) (f (a i) (b i) (c i))))
  result)

(defn- same-length? [a b]
  (= (length a) (length b)))

(defn- find-vec-len [args]
  (var result nil)
  (each arg args
    (unless (number? arg)
      (def len (length arg))
      (if (nil? result)
        (set result len)
        (assert (= result len) "vector length mismatch"))))
  result)

(defn- num-to-array [x len]
  (if (number? x)
    (array/new-filled len x)
    x))

(defmacro make-generic-1 [name &opt f]
  (assert (symbol? name))
  (assert (not= name 'a))
  (def $prim-f (gensym))
  ~(upscope
    (def- ,$prim-f ,(or f name))
    (defn ,name [a]
      (cond
        (number? a) (,$prim-f a)
        (vec? a) (map ,name a)
        ~(,',name ,a)))))

(defmacro make-generic-2 [name &opt f]
  (assert (symbol? name))
  (assert (not= name 'a))
  (assert (not= name 'b))
  (def $prim-f (gensym))
  ~(upscope
    (def- ,$prim-f ,(or f name))
    (defn ,name [a b]
      (cond
        (and (number? a) (number? b)) (,$prim-f a b)
        (and (vec? a) (number? b)) (map-vec-l ,name a b)
        (and (number? a) (vec? b)) (map-vec-r ,name a b)
        (and (vec? a) (vec? b))
          (if (same-length? a b)
            (zip-with2 ,name a b)
            (error "vector length mismatch"))
        ~(,',name ,a ,b)))))

(defmacro make-generic-3 [name &opt f]
  (assert (symbol? name))
  (assert (not- in? name ['a 'b 'c 'args]))
  (def $prim-f (gensym))
  ~(upscope
    (def- ,$prim-f ,(or f name))
    (defn ,name [a b c]
      (def args [a b c])
      (if (all number? args)
        (,$prim-f a b c)
        (if (all |(or (number? $) (vec? $)) args)
          (let [len (find-vec-len args)]
            (zip-with3 ,name
              (num-to-array a len)
              (num-to-array b len)
              (num-to-array c len)))
          ~(,',name ,a ,b ,c))))))

(defn- variadic [f one]
  (fn [& args]
    (cond
      (= (length args) 1) (one (args 0))
      (= (length args) 2) (f (args 0) (args 1))
      (reduce2 f args))))

(defmacro make-variadic [name one]
  ~(upscope
    (make-generic-2 ,name)
    (def ,name (,variadic ,name ,one))))

(make-generic-1 sin math/sin)
(make-generic-1 cos math/cos)
(make-generic-1 abs math/abs)

# TODO: so this is a tricky one. this is overloaded
# on the GPU to accept one or two arguments, so it
# should work with one or two arguments. But... it
# doesn't, yet.
(make-generic-2 atan math/atan2)

(defn- neg [x] (- x))
(defn- recip [x] (/ x))

(make-variadic + identity)
(make-variadic - neg)
(make-variadic * identity)
(make-variadic / recip)

(make-generic-3 clamp
  (fn [x lo hi]
    (cond
      (< x lo) lo
      (> x hi) hi
      x)))

# TODO: these don't really belong here?

(defn . [x k] ~(. ,x ,k))

(defn sin+ [x]
  (* 0.5 (+ (sin x) 1)))

(defn sin- [x]
  (* 0.5 (- (sin x) 1)))

(defn cos+ [x]
  (* 0.5 (+ (cos x) 1)))

(defn cos- [x]
  (* 0.5 (- (cos x) 1)))
