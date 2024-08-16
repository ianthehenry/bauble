(use judge)

(defn get-var [env sym]
  (-> env
    (in sym)
    (in :ref)
    (in 0)))

(defn set-var [env sym value]
  (-> env
    (in sym)
    (in :ref)
    (put 0 value))
  value)

(defn find-index [pred? ast &opt start-index end-index]
  (default start-index 0)
  (default end-index (length ast))
  (var result nil)
  (for i start-index end-index
    (when (pred? (in ast i))
      (set result i)
      (break)))
  result)
(test (find-index odd? [1 2 3]) 0)
(test (find-index odd? [1 2 3] 0) 0)
(test (find-index odd? [1 2 3] 0 3) 0)
(test (find-index odd? [1 2 3] 1) 2)
(test (find-index odd? [2 2 2 2 2 3] 1 3) nil)


(defn string/find-last [str c]
  (var i (dec (length str)))
  (var result nil)
  (while (>= i 0)
    (when (= (in str i) c)
      (set result i)
      (break))
    (-- i))
  result)

(defn ptuple? [x]
  (and (tuple? x) (= (tuple/type x) :parens)))
