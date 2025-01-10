(import ../jlsl)
(import ../glsl)
(import ./evaluator :export true)
(import ./renderer :export true)
(import ./completer :export true)
(import ./environment/uniforms-private)

(defn compile-to-glsl [render-type crosshairs dynamic-camera? env glsl-version]
  (renderer/render env glsl-version
    :render-type render-type
    :crosshairs crosshairs
    :dynamic-camera? dynamic-camera?))

(defn- type-to-string [type]
  (jlsl/type/match type
    (void) (error "no, how did you even do that")
    (primitive t)
      (jlsl/primitive-type/match t
        (float) nil
        (double) (error "listen i don't really understand how doubles work so you can't do that yet")
        (int) (error "yes, int uniforms would be an extremely reasonable thing to support, eventually")
        (uint) (error "yes, uint uniforms would be an extremely reasonable thing to support, eventually")
        (bool) nil)
    (vec t _)
      (jlsl/primitive-type/match t
        (float) nil
        (double) (error "no dvecs")
        (int) (error "no ivecs yet")
        (uint) (error "no uvecs yet")
        (bool) (error "no bvecs yet"))
    (mat _ _) (error "okay sorry look no matrices yet, maybe one day")
    (array _ _) (error "yeah arrays would be nice wouldn't they")
    (struct _ _) (error "you can't make a struct uniform; where did you even get a struct from anyway"))
  (string (jlsl/type/to-glsl type)))

# returns an array of [name type-string initial-value]
# type-string will be float, vec2, vec3, etc
(defn get-uniforms [env]
  (seq [[variable value] :pairs (in env uniforms-private/*uniforms*)]
    [(glsl/identifier (jlsl/variable/name variable))
     (type-to-string (jlsl/variable/type variable))
     value]))
