(use ./util)
(import ./forward-declarations)
# we create a copy here so that we don't import private bindings
(def- root (merge-module (make-env root-env) (require ".")))

(var- thunks-realized nil)
(defn new []
  (def thunk-env (make-env root))
  (when thunks-realized
    (assert (= thunks-realized (length *thunks*)) "ian the number of realized thunks changed from one environment creation to the next, which means you introduced some terrible bug that's causing the default environment to fall out of the module cache")
    (set thunks-realized (length *thunks*)))
  (each thunk *thunks* (eval thunk thunk-env))

  (def env (make-env thunk-env))

  (loop [[sym entry] :pairs root
         :when (and (symbol? sym) (table? entry))
         :let [ref (in entry :ref)]
         :when (and (array? ref) (= (length ref) 1))]
    (put env sym @{:doc (in entry :doc) :ref @[(in ref 0)]}))

  env)
