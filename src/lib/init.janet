(import ./evaluator :export true)
(import ./renderer :export true)
(import ./completer :export true)

# TODO: these are shims for compatibility with the old Bauble API;
# we should expose a more sane interface
(defn bauble-evaluator/evaluate [script]
  (def env (evaluator/evaluate script))
  [(get-in env [:subject :value]) env])
(defn shade/compile-shape [_ env glsl-version]
  (renderer/render env glsl-version))
