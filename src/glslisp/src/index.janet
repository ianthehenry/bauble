(import ./type)
(use ./util)

(defn- glsl-float-string [n]
  (if (int? n)
    (string n ".0")
    (string/format "%f" n)))

(defn- xyzw? [charcode]
  (and (>= charcode 119) (<= charcode 122)))

(defn- rgba? [charcode]
  (case charcode
    103 true
    114 true
    98 true
    97 true
    false))

(defn- vec-field-char? [charcode]
  (or (xyzw? charcode) (rgba? charcode)))

(defn- vec-fields? [keyword]
  (all vec-field-char? keyword))

(defn- unop? [symbol]
  (case symbol
    '- true
    '/ true
    'not true
    false))

(defn- binop? [symbol]
  (case symbol
    '+ true
    '- true
    '* true
    '/ true

    '= true
    'not= true
    '< true
    '> true
    '<= true
    '>= true

    'and true
    'or true
    'xor true
    false))

(defn- binop-to-string [symbol]
  (case symbol
    '= "=="
    'not= "!="
    'and "&&"
    'or "||"
    'xor "^^"
    (string symbol)))

(defn- unop-to-string [symbol arg]
  (case symbol
    'not (string "(!" arg ")")
    '- (string "(-" arg ")")
    '/ (string "(1.0 / " arg ")")
    (errorf "unknown unop %s" symbol)))

(defn- binop [f args]
  (cond
    (< (length args) 2) (errorf "not enough arguments for binary operator %s" f)
    (= (length args) 2) (string "("(args 0)" "(binop-to-string f)" "(args 1)")")
    (> (length args) 2) (errorf "too many arguments for binary operator %s" f)))

(defn- vector-type? [type]
  (case type
    type/vec2 true
    type/vec3 true
    type/vec4 true
    false))

(var compile! nil)
(var typecheck nil)

(defn- typeof-generic-application [args]
  (var vector-type nil)
  (each arg args
    (def type (typecheck arg))
    (if (vector-type? type)
      (if (nil? vector-type)
        (set vector-type type)
        (assertf (= vector-type type) "mismatched vectors: %p != %p" vector-type type))
      (assertf (= type type/float) "expected vector or float, got %p" type)))
  (or vector-type type/float))

(defn- vec-type-of-length [len]
  (case len
    2 type/vec2
    3 type/vec3
    4 type/vec4
    (errorf "there is no vec%d" len)))

(defn- typeof-access [[record prop]]
  (if (vec-fields? prop)
    (let [len (length prop)]
      (if (= len 1)
        type/float
        (vec-type-of-length (length prop))))
    type/unknown))

(defn- typecheck-application [f args]
  (case f
    '. (typeof-access args)
    'vec2 type/vec2
    'vec3 type/vec3
    'vec4 type/vec4
    'float type/float
    'length type/float
    'distance type/float
    'dot type/float
    'not type/bool
    '= type/bool
    'not= type/bool
    'and type/bool
    'or type/bool
    'xor type/bool

    # TODO: this is kind of a gross hack
    'hsv type/vec3
    'hsl type/vec3
    'perlin2 type/float
    'perlin3 type/float
    'perlin4 type/float

    (typeof-generic-application args)))

(defn- compile-function-application [state f args]
  (def args (map |(compile! state $) args))
  (cond
    (and (unop? f) (= (length args) 1)) (unop-to-string f (args 0))
    (binop? f) (binop f args)
    (do
      (:require-function state f)
      (string f "(" (string/join args ", ") ")"))))

(defn- compile-access [state args]
  (assert (= (length args) 2) "wrong number of arguments to .")
  (def [expr field] args)
  (string (compile! state expr) "." field))

(defn- compile-with [state args]
  (assert (= (length args) 3) "wrong number of arguments to with")
  (def [variable value expr] args)
  (def new-name (:new-name state variable))
  (:push-var state variable (fn []
    (:statement state (string (variable :type)` `new-name` = `(compile! state value)`;`))
    new-name))
  (def result (compile! state expr))
  (:pop-var state variable)
  result)

(defn- compile-application [state f args]
  (case f
    '. (compile-access state args)
    'with (compile-with state args)
    (compile-function-application state f args)))

(defn- vec-constructor [tuple]
  (case (length tuple)
    2 'vec2
    3 'vec3
    4 'vec4
    (errorf "there is no vec%d" (length tuple))))

(defn- vec-type [seq] (vec-type-of-length (length seq)))

(def- head first)
(defn- tail [x] (drop 1 x))

(defn variable [name type] @{:name name :type type})

(set compile!
(defn compile! [state expr]
  (cond
    (number? expr) (glsl-float-string expr)
    # TODO: this is sort of a hack that shouldn't be here and i don't remember
    # why it's necessary in the first place.
    #(symbol? expr) (string expr)
    (table? expr) (string (:get-var state expr))
    (application? expr) (compile-application state (head expr) (tail expr))
    (vec? expr) (compile-function-application state (vec-constructor expr) expr)
    (errorf "cannot compile %p" expr))))

(set typecheck
(defn typecheck [expr]
  (cond
    (number? expr) type/float
    (table? expr) (expr :type)
    (application? expr) (typecheck-application (head expr) (tail expr))
    (vec? expr) (vec-type expr)
    type/unknown)))
