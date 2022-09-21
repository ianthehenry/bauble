(import ./index :as glslisp)
(import ./variable)
(use ./util)

# TODO: performance question: would the cached lookup here
# perform better if shapes were tables instead of structs?
# because we'd get pointer lookup instead of value lookup?
# then again, would that be *worse* because we'd end up
# generating identical functions for surface operations
# in some cases?

(def- comp-state-proto @{
  :push-var-name (fn [self variable name]
    (array/push (get-or-insert (self :bindings) variable |@[]) name))
  :push-var-type (fn [self variable type]
    (array/push (get-or-insert (self :type-bindings) variable |@[]) type))
  :pop-var-name (fn [self variable]
    (array/pop (get-or-error (self :bindings) variable)))
  :pop-var-type (fn [self variable]
    (array/pop (get-or-error (self :type-bindings) variable)))
  :get-var-name (fn [self variable]
    (if-let [stack ((self :bindings) variable)
             binding (last stack)]
      binding
      (do
        (set ((self :free-variables) variable) true)
        (:intrinsic-name variable))))
  :get-var-type (fn [self variable]
    (if-let [type (:type variable)]
      type
      (if-let [stack ((self :type-bindings) variable)
               binding (last stack)]
        binding
        (errorf "no type known for %s" (:intrinsic-name variable)))))
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
    (def index (inc (or ((self :var-names) variable) 0)))
    (put (self :var-names) variable index)
    (string (:intrinsic-name variable) index))
  :temp-var (fn [self type name]
    (def index (or ((self :temp-vars) name) 0))
    (set ((self :temp-vars) name) (inc index))
    (variable/new (symbol (string name index "_")) type))

  :instantiate-template (fn [self template args]
    (def {:variable-params variable-params
          :constant-params constant-params
          :get-body get-body
          :name name
          :return-type return-type} template)
    (def variable-args (take (length variable-params) args))
    (def constant-args (drop (length variable-params) args))

    (def new-scope (:new-scope self))
    (def body (get-body new-scope))
    (assert (empty? (new-scope :statements)) "function produced statements")
    (assert (empty? (new-scope :bindings)) "function produced bindings")
    # cheap way to depup
    (def free-var-set (new-scope :free-variables))
    (each arg variable-args (put free-var-set arg true))
    (def variable-args (keys free-var-set))
    # TODO: We use the same name at the moment, and rely on the fact that
    # GLSL allows us to overload functions with differently typed arguments.
    # But we could generate a unique name here.
    (def key [name (freeze (map |(:get-var-type self $) variable-args))])

    (defn param-string [variable]
      (def name (:intrinsic-name variable))
      (def type (:get-var-type self variable))
      (match type
        [:array len elem-type] (string/format "%s %s[%d]" elem-type name len)
        (string type " " name)))

    (unless (in (self :compiled-function-cache) key)
      (put (self :compiled-function-cache) key true)
      (array/push (self :compiled-functions)
        {:name name
         :params [;(map param-string variable-args) ;constant-params]
         :body body
         :return-type return-type}))

    [name [;variable-args ;constant-args]])

  :instantiate-glsl-helper (fn [self f]
    (when-let [registration ((self :glsl-functions) f)]
      (def {:entry entry :deps deps} registration)
      (unless ((self :glsl-functions-used) f)
        (set ((self :glsl-functions-used) f) true)
        (each dep deps (:require-function self dep []))
        (array/push (self :compiled-functions) entry))))

  :require-function (fn [self f args]
    (if-let [template ((self :function-templates) f)]
      (:instantiate-template self template args)
      (do
        (:instantiate-glsl-helper self f)
        [f args])))

  :generate-function (fn [self return-type key name-base args get-body]
    (def [variable-args constant-params-and-args] (group-bool variable/instance? args))
    (def constant-args (map |($ 1) constant-params-and-args))
    (def get-body (if (function? get-body) get-body (fn [_] get-body)))
    (def args [;variable-args ;constant-args])

    # TODO: putting the name in here is a little bit roundabout. we could instead return
    # the template itself and not have to look it up later.
    (defn invocation [name] ~(,name ,;args))

    (if-let [name (in (self :template-key-to-name) key)]
      (invocation name)
      (do
        (def name (cond
          # TODO: be smarter with tuple-of-keywords, which are very common for axis-specific functions
          (keyword? key) (symbol name-base)
          (do
            (def name-index (in (self :function-name-indices) name-base 0))
            (put (self :function-name-indices) name-base (inc name-index))
            (symbol (string/format "%s_%d" name-base name-index)))))
        (def template
          {:variable-params variable-args
           :constant-params (map |($ 0) constant-params-and-args)
           :name name
           :get-body get-body
           :return-type return-type})
        (put (self :function-templates) name template)
        (put (self :template-key-to-name) key name)
        (invocation name))))

  :new-scope (fn [self &named export-free-vars]
    (default export-free-vars false)
    (table/setproto
      @{:bindings @{}
        :free-variables (if-not export-free-vars @{})
        :statements @[]
        :var-names @{}
        :temp-vars @{}}
      self))
  })

(defn new [glsl-functions]
  (table/setproto
    @{:function-name-indices @{}
      :glsl-functions glsl-functions
      :glsl-functions-used @{}
      :function-templates @{}
      :template-key-to-name @{}
      :compiled-function-cache @{}
      :compiled-functions @[]
      :type-bindings @{}}
    comp-state-proto))
