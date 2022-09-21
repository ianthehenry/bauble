(import ./type)
(import ./variable)
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
    'with (typecheck (args 2))
    'extend (typecheck (args 2))

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
    'sum2 type/float
    'sum3 type/float
    'sum4 type/float
    'product2 type/float
    'product3 type/float
    'product4 type/float
    'min2 type/float
    'min3 type/float
    'min4 type/float
    'max2 type/float
    'max3 type/float
    'max4 type/float

    'hash11 type/float
    'hash12 type/float
    'hash13 type/float
    'hash14 type/float
    'hash21 type/vec2
    'hash22 type/vec2
    'hash23 type/vec2
    'hash24 type/vec2
    'hash31 type/vec3
    'hash32 type/vec3
    'hash33 type/vec3
    'hash34 type/vec3
    'hash41 type/vec4
    'hash42 type/vec4
    'hash43 type/vec4
    'hash44 type/vec4

    (typeof-generic-application args)))

(defn- compile-function-application [state f args]
  (cond
    (and (unop? f) (= (length args) 1)) (unop-to-string f (compile! state (args 0)))
    (binop? f) (binop f (map |(compile! state $) args))
    (do
      (def [f args] (:require-function state f args))
      (string f "(" (string/join (map |(compile! state $) args) ", ") ")"))))

(defn- compile-access [state args]
  (assert (= (length args) 2) "wrong number of arguments to .")
  (def [expr field] args)
  (string (compile! state expr) "." field))

(defn- compile-with [state args]
  (assert (= (length args) 3) "wrong number of arguments to with")
  (def [variable value expr] args)
  (def new-name (:new-name state variable))
  (def type (:type variable))
  (:statement state (string type` `new-name` = `(compile! state value)`;`))
  (:push-var-name state variable new-name)
  (def result (compile! state expr))
  (:pop-var-name state variable)
  result)

# TODO: support variadic extensions?
# also, it's probably worth unrolling the assignment loop here, right?
(defn- compile-extend [state args]
  (assert (= (length args) 3) "wrong number of arguments to extend")
  (def [variable value expr] args)
  (def old-name (:get-var-name state variable))
  (def new-name (:new-name state variable))
  (def type (:get-var-type state variable))
  (assert type/is-array? type)
  (def underlying (type/array-underlying type))
  (def old-length (type/array-length type))
  (def new-length (+ 1 old-length))
  (def new-type (type/array new-length underlying))
  (:statement state (string underlying` `new-name`[`new-length`];`))
  (for i 0 old-length
    (:statement state (string new-name`[`i`] = `old-name`[`i`];`)))
  #(:statement state (string `for (int i = 0; i < `old-length`; i++) {`))
  #(:statement state (string new-name`[i] = `old-name`[i];`))
  #(:statement state `}`)
  (:statement state (string new-name`[`old-length`] = `(compile! state value)`;`))
  (:push-var-name state variable new-name)
  (:push-var-type state variable new-type)
  (def result (compile! state expr))
  (:pop-var-name state variable)
  (:pop-var-type state variable)
  result)

(defn- compile-application [state f args]
  (case f
    '. (compile-access state args)
    'with (compile-with state args)
    'extend (compile-extend state args)
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

(set compile!
(defn compile! [state expr]
  (cond
    (number? expr) (glsl-float-string expr)
    (variable/instance? expr) (string (:get-var-name state expr))
    (application? expr) (compile-application state (head expr) (tail expr))
    (vec? expr) (compile-function-application state (vec-constructor expr) expr)
    (errorf "cannot compile %p" expr))))

(set typecheck
(defn typecheck [expr]
  (cond
    (number? expr) type/float
    # TODO: it's a little weird that variables of variable type don't
    # get known types here but it doesn't matter in practice because
    # lights are not really first-class entities
    (variable/instance? expr) (or (:type expr) type/unknown)
    (application? expr) (typecheck-application (head expr) (tail expr))
    (vec? expr) (vec-type expr)
    type/unknown)))
