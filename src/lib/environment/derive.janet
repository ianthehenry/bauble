(import ./forward-declarations)
# we create a copy here so that we don't import private bindings
(def- root (merge-module (make-env root-env) (require ".")))

(defn new []
  (def env (make-env root))

  (loop [[sym entry] :pairs root
         :when (and (symbol? sym) (table? entry))
         :let [ref (in entry :ref)]
         :when (and (array? ref) (= (length ref) 1))]
    (put env sym @{:doc (in entry :doc) :ref @[(in ref 0)]}))

  (forward-declarations/make-forward-declarations env)
  env)
