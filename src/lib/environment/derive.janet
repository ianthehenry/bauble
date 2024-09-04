(use ./util)
(import ./forward-declarations)
(use ./deferred-evaluation)

(def static-environment (require "."))

(def excluded-symbols (tabseq [sym :in '(
  dofile
  cli-main
  flycheck
  getline
  import
  import*
  use
  require
  native
  quit
  repl
  root-env
  sandbox
  short-fn
  slurp
  spit
  stdin
  )] sym true))
(def censored-root-env (tabseq [[sym entry] :pairs root-env
  :unless (string/has-prefix? "os/" sym)
  :unless (string/has-prefix? "net/" sym)
  :unless (string/has-prefix? "file/" sym)
  :unless (string/has-prefix? "ffi/" sym)
  :unless (string/has-prefix? "bundle/" sym)
  :unless (string/has-prefix? "parser/" sym)
  :unless (string/has-prefix? "module/" sym)
  :unless (string/has-prefix? "debug/" sym)
  :unless (string/has-prefix? "debug" sym)
  :unless (string/has-prefix? "load-image" sym)
  :unless (string/has-prefix? "make-image" sym)
  :unless (and (string/has-prefix? "*" sym) (string/has-suffix? "*" sym))
  :unless (has-key? excluded-symbols sym)
  ]
  sym entry))

(def- root (merge-module (make-env censored-root-env) static-environment))

(var- thunks-realized nil)
(defn new []
  (def thunk-env (make-env root))
  (if thunks-realized
    (assert (= thunks-realized (length *thunks*)) "ian the number of realized thunks changed from one environment creation to the next, which means you introduced some terrible bug that's causing the default environment to fall out of the module cache")
    (set thunks-realized (length *thunks*)))
  (with-env thunk-env
    (each thunk *thunks*
      (eval thunk)))
  (def env (make-env thunk-env))
  env)
