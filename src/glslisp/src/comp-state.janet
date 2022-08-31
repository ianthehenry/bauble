(import ./index :as glslisp)
(use ./util)

# TODO: performance question: would the cached lookup here
# perform better if shapes were tables instead of structs?
# because we'd get pointer lookup instead of value lookup?
# then again, would that be *worse* because we'd end up
# generating identical functions for surface operations
# in some cases?

(defn- param-string [{:type type :name name}]
  (match type
    [:array len elem-type] (string/format "%s %s[%d]" elem-type name len)
    (string type " " name)))

(defn- variable? [x]
  (and (table? x) (x :name) (x :type)))

(def- comp-state-proto @{
  :push-var (fn [self variable value]
    (array/push (get-or-insert (self :bindings) variable |@[]) value))
  :pop-var (fn [self variable]
    (array/pop (get-or-error (self :bindings) variable)))
  :get-var (fn [self variable]
    (if-let [stack ((self :bindings) variable)
             binding (last stack)]
      (if (function? binding)
        (do
          (array/pop stack)
          (def binding (binding))
          (array/push stack binding)
          binding)
        binding)
      (do
        (set ((self :free-variables) variable) true)
        (string (variable :name)))))
  :statement (fn [self statement]
    (array/push (self :statements) statement))

  :compile-distance (fn [self shape]
    (def expression (glslisp/compile! self (:compile shape self)))
    [(self :statements) expression])
  :compile-color (fn [self shape]
    (def expression (glslisp/compile! self (:surface shape self)))
    [(self :statements) expression])

  # TODO: this is a little weird right? one is for compiling
  # with, one is for creating temp variables to use for with
  :new-name (fn [self variable]
    (def index (+ (or ((self :var-names) variable) 0) 1))
    (set ((self :var-names) variable) index)
    (string (variable :name) index))
  :temp-var (fn [self type name]
    (def index (or ((self :temp-vars) name) 0))
    (set ((self :temp-vars) name) (inc index))
    @{:type type :name (symbol (string name index "_"))})

  :require-function (fn [self f]
    (when-let [registration ((self :glsl-functions) f)]
      (def {:entry entry :deps deps} registration)
      (unless ((self :glsl-functions-used) f)
        (set ((self :glsl-functions-used) f) true)
        (each dep deps (:require-function self dep))
        (array/push (self :functions) entry))))

  :generate-function (fn [self return-type key name-base args get-body]
    (def [deps explicit-params-and-args] (group-bool variable? args))
    (def explicit-args (map |($ 1) explicit-params-and-args))

    (def new-scope (:new-scope self))
    (def body (if (function? get-body) (get-body new-scope) get-body))
    (assert (empty? (new-scope :statements)) "function produced statements")
    (assert (empty? (new-scope :bindings)) "function produced bindings")

    (def free-vars (new-scope :free-variables))
    (each dep deps (set (free-vars dep) true))
    (def variables-required (keys free-vars))
    (def implicit-args variables-required)

    (def args [;implicit-args ;explicit-args])
    (defn invocation [name] ~(,name ,;args))

    # So this assumes that, if your implicit parameters can vary,
    # then your body will also vary, so you're going to use a
    # reference-equality key here. so we can assume that
    # we don't need to check that the implicit parameters are
    # the same -- we assume that information is redundant with
    # the key.
    (if-let [cached ((self :function-cache) key)]
      (invocation (cached :name))
      (do
        (def implicit-params (map param-string variables-required))
        (def explicit-params (map |($ 0) explicit-params-and-args))
        (def params [;implicit-params ;explicit-params])
        (def name (if (keyword? key)
          (symbol name-base)
          (do
            (def name-index (get (self :function-names) name-base 0))
            (set ((self :function-names) name-base) (inc name-index))
            (symbol (string/format "%s_%d" name-base name-index)))))
        (def entry {:name name :params params :body body :return-type return-type})
        (array/push (self :functions) entry)
        (set ((self :function-cache) key) entry)
        (invocation name))))

  :new-scope (fn [self]
    (table/setproto
      @{:bindings @{}
        :free-variables @{}
        :statements @[]
        :var-names @{}
        :temp-vars @{}}
      self))
  })

(defn new [glsl-functions]
  (table/setproto
    @{:function-names @{}
      :glsl-functions glsl-functions
      :glsl-functions-used @{}
      :function-cache @{}
      :functions @[]}
    comp-state-proto))
