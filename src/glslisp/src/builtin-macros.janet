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

# TODO: so much copy and pasted code here...

(defmacro make-generic-1 [name &named f glf expr]
  (assert (symbol? name))
  (assert (not= name 'a))
  (def $prim-f (gensym))
  (default f name)
  (default glf name)
  (default expr ~~(,',glf ,a))
  ~(upscope
    (def- ,$prim-f ,f)
    (defn ,name [a]
      (cond
        (number? a) (,$prim-f a)
        (,vec? a) (map ,name a)
        ,expr))))

(defmacro make-vec-1 [name &named f glf expr]
  (assert (symbol? name))
  (assert (not= name 'a))
  (def $prim-f (gensym))
  (default f name)
  (default glf name)
  (default expr ~~(,',glf ,a))
  ~(upscope
    (def- ,$prim-f ,f)
    (defn ,name [a]
      (cond
        (,simple-vec? a) (,$prim-f a)
        ,expr))))

(defmacro make-vec-2 [name &named f glf expr]
  (assert (symbol? name))
  (assert (not= name 'a 'b))
  (def $prim-f (gensym))
  (default f name)
  (default glf name)
  (default expr ~~(,',glf ,a ,b))
  ~(upscope
    (def- ,$prim-f ,f)
    (defn ,name [a b]
      (cond
        (and (,simple-vec? a) (,simple-vec? b))
          (if (,same-length? a b)
            (,$prim-f a b)
            (error "vector length mismatch"))
        ,expr))))

(defmacro make-generic-2 [name &named f glf expr]
  (assert (symbol? name))
  (assert (not= name 'a))
  (assert (not= name 'b))
  (def $prim-f (gensym))
  (default f name)
  (default glf name)
  (default expr ~~(,',glf ,a ,b))
  ~(upscope
    (def- ,$prim-f ,f)
    (defn ,name [a b]
      (cond
        (and (number? a) (number? b)) (,$prim-f a b)
        (and (,vec? a) (number? b)) (,map-vec-l ,name a b)
        (and (number? a) (,vec? b)) (,map-vec-r ,name a b)
        (and (,vec? a) (,vec? b))
          (if (,same-length? a b)
            (,zip-with2 ,name a b)
            (error "vector length mismatch"))
        ,expr))))

# TODO: this is kinda not amazing. seems
# like i should be able to make this
# more general.
(defmacro make-matrix-2 [name &named f glf]
  (assert (symbol? name))
  (assert (not= name 'a))
  (assert (not= name 'b))
  (def $prim-f (gensym))
  (default f name)
  (default glf name)
  ~(upscope
    (def- ,$prim-f ,f)
    (defn ,name [a b]
      (cond
        (and (,vec? a) (,vec? b))
          (if (,same-length? a b)
            (,$prim-f a b)
            (error "vector length mismatch"))
        (vec? a) ~(,',glf (mat3 ,;a) ,b)
        (vec? b) ~(,',glf ,a (mat3 ,;b))
        ~(,',glf ,a ,b)))))

(defmacro make-generic-3 [name &named f glf expr]
  (assert (symbol? name))
  (assert (not- in? name ['a 'b 'c 'args]))
  (def $prim-f (gensym))
  (default f name)
  (default glf name)
  (default expr ~~(,',glf ,a ,b ,c))
  ~(upscope
    (def- ,$prim-f ,f)
    (defn ,name [a b c]
      (def args [a b c])
      (if (all number? args)
        (,$prim-f a b c)
        (if (all |(or (number? $) (,vec? $)) args)
          (let [len (,find-vec-len args)]
            (,zip-with3 ,name
              (,num-to-array a len)
              (,num-to-array b len)
              (,num-to-array c len)))
          ,expr)))))

(defmacro make-numeric-3 [name &named f glf expr]
  (assert (symbol? name))
  (assert (not= name 'a 'b 'c))
  (def $prim-f (gensym))
  (default f name)
  (default glf name)
  (default expr ~~(,',glf ,a ,b ,c))
  ~(upscope
    (def- ,$prim-f ,f)
    (defn ,name [a b c]
      (cond
        (and (number? a) (number? b) (number? c)) (,$prim-f a b c)
        ,expr))))

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
