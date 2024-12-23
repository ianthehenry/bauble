(import ./evaluator :export true)
(import ./renderer :export true)
(import ./completer :export true)

(defn compile-to-glsl [render-type env glsl-version]
  (renderer/render render-type env glsl-version))
