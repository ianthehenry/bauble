(use judge)

(defn without-sourcemap [t]
  (if (table? t)
    (put (table/proto-flatten t) :source-map nil)
    t))

(defn without-sourcemaps [t]
  (tabseq [[k v] :pairs t :when (symbol? k)]
    k (without-sourcemap v)))

(test (without-sourcemaps (require "./environment"))
  @{subject @{:doc "A variable that determines what Bauble will render.\n\nYou can set this variable explicitly to change your focus, or use the `view` macro to change your focus. If you don't set a subject, Bauble will render the last expression in your script that it knows how to render."
              :ref @[nil]}
    view @{:doc "(view subject)\n\nA shorthand for `(set subject _)` that fits nicely into pipe notation, e.g. `(sphere 50 | view)`."
           :macro true
           :value @view}})
