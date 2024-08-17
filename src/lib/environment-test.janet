(use judge)

(defn cleanse-environment-entry [t]
  (if (table? t) (do
    (def t (table/proto-flatten t))
    (put t :source-map nil)
    (put t :private nil)
    (when (function? (t :value))
      (put t :value nil))
    (when (function? (get-in t [:ref 0]))
      (put t :ref nil))
    t)
  t))

(defn cleanse-environment [t]
  (tabseq [[k v] :pairs t :when (symbol? k) :when (not (v :private))]
    k (cleanse-environment-entry v)))

(test (cleanse-environment (require "./environment"))
  @{% @{}
    * @{}
    + @{}
    - @{}
    / @{}
    < @{}
    <= @{}
    = @{}
    > @{}
    >= @{}
    @and @{:doc "(and & forms)\n\nEvaluates to the last argument if all preceding elements are truthy, otherwise\nevaluates to the first falsey argument."
           :macro true}
    @in @{:doc "(in ds key &opt dflt)\n\nGet value in ds at key, works on associative data structures. Arrays, tuples, tables, structs, strings, symbols, and buffers are all associative and can be used. Arrays, tuples, strings, buffers, and symbols must use integer keys that are in bounds or an error is raised. Structs and tables can take any value as a key except nil and will return nil or dflt if not found."}
    @length @{:doc "(length ds)\n\nReturns the length or count of a data structure in constant time as an integer. For structs and tables, returns the number of key-value pairs in the data structure."}
    @or @{:doc "(or & forms)\n\nEvaluates to the last argument if all preceding elements are falsey, otherwise\nevaluates to the first truthy element."
          :macro true}
    abs @{}
    acos @{}
    acosh @{}
    and @{}
    asin @{}
    asinh @{}
    atan @{}
    atanh @{}
    ceil @{}
    circle @{:doc "(circle r)\n\n"}
    clamp @{}
    color @{:doc "(color fields color-expression)\n\n"}
    cos @{}
    cosh @{}
    cross @{}
    distance @{}
    dot @{}
    equal @{}
    exp @{}
    exp2 @{}
    faceforward @{}
    floor @{}
    fma @{}
    fract @{}
    in @{:doc "(in & args)\n\n"}
    inversesqrt @{}
    length @{}
    log @{}
    log2 @{}
    mat2 @{}
    mat2x2 @{}
    mat2x3 @{}
    mat2x4 @{}
    mat3 @{}
    mat3x2 @{}
    mat3x3 @{}
    mat3x5 @{}
    mat4 @{}
    mat4x2 @{}
    mat4x3 @{}
    mat4x4 @{}
    max @{}
    min @{}
    mix @{}
    mod @{}
    normalize @{}
    not @{}
    not-equal @{}
    not= @{}
    or @{}
    pow @{}
    reflect @{}
    refract @{}
    round @{}
    round-even @{}
    sign @{}
    sin @{}
    sinh @{}
    smoothstep @{}
    sqrt @{}
    step @{}
    subject @{:doc "A variable that determines what Bauble will render.\n\nYou can set this variable explicitly to change your focus, or use the `view` macro to change your focus. If you don't set a subject, Bauble will render the last expression in your script that it knows how to render."
              :ref @[nil]}
    tan @{}
    tanh @{}
    trunc @{}
    vec @{}
    vec2 @{}
    vec3 @{}
    vec4 @{}
    view @{:doc "(view subject)\n\nA shorthand for `(set subject _)` that fits nicely into pipe notation, e.g. `(sphere 50 | view)`."
           :macro true}
    xor @{}})