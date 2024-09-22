(import ../util :prefix "" :export true)
# it's faster to allocate an array than a tuple, and look, i'm not
# happy about this either
(defn vec+ [[x1 y1 z1] [x2 y2 z2]] @[(+ x1 x2) (+ y1 y2) (+ z1 z2)])
(defn vec- [[x1 y1 z1] [x2 y2 z2]] @[(- x1 x2) (- y1 y2) (- z1 z2)])
(defn vec* [[x1 y1 z1] [x2 y2 z2]] @[(* x1 x2) (* y1 y2) (* z1 z2)])
(defn vec*s [[x1 y1 z1] s] @[(* x1 s) (* y1 s) (* z1 s)])
(defn vec/ [[x1 y1 z1] [x2 y2 z2]] @[(/ x1 x2) (/ y1 y2) (/ z1 z2)])

# like for, but unrolled at compile time
(defmacro for! [sym start end & body]
  (assert (and (number? start) (number? end)))
  ['upscope
    ;(catseq [i :range [start end]]
      (prewalk |(if (= $ sym) i $) body))])

(defmacro blshift! [x y]
  ~(comptime (blshift ,x ,y)))

(defmacro bor= [sym expr]
  ~(set ,sym (,bor ,sym ,expr)))

(defn lerp [left right amount-right]
  (+ (* left (- 1 amount-right))
     (* right amount-right)))

