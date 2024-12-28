(import ./evaluator :export true)
(import ./renderer :export true)
(import ./completer :export true)

(defn compile-to-glsl [render-type crosshairs env glsl-version]
  (renderer/render env glsl-version
    :render-type render-type
    :crosshairs crosshairs))
