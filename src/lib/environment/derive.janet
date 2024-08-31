(use ./util)
(import ./forward-declarations)
(use ./deferred-evaluation)

(def- root (merge-module (make-env root-env) (require ".")))

(var- thunks-realized nil)
(defn new []
  (def thunk-env (make-env root))
  (if thunks-realized
    (assert (= thunks-realized (length *thunks*)) "ian the number of realized thunks changed from one environment creation to the next, which means you introduced some terrible bug that's causing the default environment to fall out of the module cache")
    (set thunks-realized (length *thunks*)))
  (resume (fiber/new (fn []
    (each thunk *thunks*
      (eval thunk))) : thunk-env))
  (def env (make-env thunk-env))
  env)
