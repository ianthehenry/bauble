(use judge)
(use module)
(use ./adt)
(use ./util)
(import ./type :prefix "" :export true)
(import pat)

(defn impl/return-type [{:declared-return-type declared-return-type}] declared-return-type)

(defn function/return-type [t]
  (function/match t
    (builtin _ return-type _) return-type
    (custom impl) (impl/return-type impl)))

(defn expr/type [t]
  (expr/match t
    (literal type _) type
    (identifier variable) (variable/type variable)
    (call function _) (function/return-type function)
    (dot expr field) (type/field-type (expr/type expr) field)
    (length _) type/int
    (in expr _) (type/element-type (expr/type expr))
    (if cond then else) (expr/type then)
    (crement _ expr) (expr/type expr)))

(defmodule param
  (defn new [lexical-variable sig]
    (assert (= (param-sig/type sig) (variable/type lexical-variable)) "BUG: parameter signature type mismatch")
    [lexical-variable sig])
  (defn var [t] (in t 0))
  (defn name [t] (variable/name (in t 0)))
  (defn sig [t] (in t 1))
  (defn type [t] (param-sig/type (sig t)))
  (defn access [t] (param-sig/access (sig t))))

(var function/param-sigs nil)

# an impl is an implementation of a custom function
(defmodule impl
  (defn new [name return-type param-sigs]
    {:name name
     :declared-return-type return-type
     :declared-param-sigs param-sigs
     :params (ref/new)
     :body (ref/new)
     :scan-ref (ref/new (free-vars/unscanned))
     :free-var-access-ref (ref/new)
     :implicit-params-ref (ref/new)})

  (defn- /name [x] (in x :name))
  (def name /name)
  (defn param-sigs [{:declared-param-sigs param-sigs}] param-sigs)

  (defn implement [t return-type params body]
    (def {:name name
          :declared-return-type declared-return-type
          :declared-param-sigs declared-param-sigs
          :params current-params
          :body current-body} t)

    (assertf (empty? current-body) "%s: cannot implement a function multiple times" name)
    (assertf (not (empty? body)) "%s: cannot implement with empty body" name)
    (def implemented-param-sigs (map param/sig params))
    (assertf (contents= declared-param-sigs implemented-param-sigs)
      "%s: parameter mismatch, declared as %q implemented as %q"
      name
      declared-param-sigs
      implemented-param-sigs)
    (assertf (= declared-return-type return-type)
      "%s: return type mismatch, declared as %q implemented as %q"
      name
      declared-return-type
      return-type)
    (array/concat current-body body)
    (array/concat current-params params)
    t)

  (defn root-identifier [expr]
    (expr/match expr
      (literal _ _) nil
      (identifier variable) variable
      (call _ _) nil
      (length _) nil
      (dot expr _) (root-identifier expr)
      (in expr index) (root-identifier expr)
      (if _ _ _) nil
      (crement _ _) nil))

  # returns free variables and all referenced functions
  (defn- scan [name body params]
    (assertf (not (empty? body)) "%s: cannot find free variables of a function that has not been implemented yet" name)

    (var scope (tabseq [param :in params] (param/var param) true))
    (var functions-called @{})

    # a map from variables to an array of @[read? written?]
    (def free-vars @{})
    (defn free-entry [k] (get-or-put free-vars k @[false false]))
    (defn mark [variable rw]
      (case rw
        :read (put (free-entry variable) 0 true)
        :write (put (free-entry variable) 1 true)
        (error "BUG")))

    (defn see-expr [expr rw]
      (expr/match expr
        (literal _ _) nil
        (identifier variable)
          (unless (in scope variable)
            (mark variable rw))
        (call function args) (do
          (put functions-called function (table/proto-flatten scope))
          (def args-and-params (try
            (zip args (function/param-sigs function))
            ([_]
              (errorf "wrong number of arguments to function %s, expected %q, got %q"
                (/name function)
                (length (function/param-sigs function))
                (length args)
                ))))
          (each [arg param-sig] args-and-params
            (match (param-sig/access param-sig)
              :in (see-expr arg :read)
              :out (see-expr arg :write)
              :inout (do (see-expr arg :read) (see-expr arg :write))
              access (errorf "BUG: unknown access qualifier %q" access))))
        (dot expr _) (do
          # okay so technically if the field is the entire thing,
          # e.g. if you have `foo.xyz = vec3(1, 2, 3)`, then the
          # read here is unnecessary. But just... just set `foo`
          # instead.
          (see-expr expr :read)
          (see-expr expr rw))
        (length expr) (see-expr expr :read)
        (in expr index) (do (see-expr expr :read) (see-expr expr rw) (see-expr index :read))
        (if cond then else) (do (see-expr cond :read) (see-expr then :read) (see-expr else :read))
        (crement _ expr) (do (see-expr expr :read) (see-expr expr :write))
        ))

    (var visit nil)
    (defn in-block [f]
      (set scope (table/setproto @{} scope))
      (f)
      (set scope (table/getproto scope)))

    (defn block [statements]
      (in-block (fn []
        (each statement statements
          (visit statement)))))

    (set visit (fn visit [statement]
      (statement/match statement
        (declaration const? variable expr) (do
          (see-expr expr :read)
          (put scope variable true))
        (assign l-value r-value) (do
          (see-expr l-value :write)
          (see-expr r-value :read)
          # it's not actually in scope, but it's no longer
          # exactly free. if we read the value after this,
          # we don't need to mark it as read-free, and we've
          # already marked it write-free
          (put scope (root-identifier l-value) true))
        (update op l-value r-value) (do
          (see-expr l-value :read)
          (see-expr l-value :write)
          (see-expr r-value :read))
        (break) nil
        (continue) nil
        (discard) nil
        (return expr) (see-expr expr :read)
        (do body) (block body)
        (upscope body) (each statement body (visit statement))
        (with bindings body)
          (block [
            ;(seq [[variable expr] :in bindings]
              (statement/declaration false variable expr))
            ;body])
        (if cond then else) (do
          (see-expr cond :read)
          (visit then)
          (if else (visit else)))
        (case value cases) (do
          (see-expr value :read)
          (each case cases
            (pat/match case
              [body] (visit body)
              [expr body] (do (see-expr expr :read) (visit body)))))
        (while cond body) (do
          (see-expr cond :read)
          (block body))
        (do-while cond body) (do
          (see-expr cond :read)
          (block body))
        (for init cond update body) (in-block (fn []
          (visit init)
          (see-expr cond :read)
          (visit update)
          (each statement body
            (visit statement))))
        (expr expr) (see-expr expr nil))))
    (block body)

    [free-vars functions-called])

  (defn- memoized-scan [{:name name :params params :body body :scan-ref scan-ref}]
    (free-vars/match (ref/get scan-ref)
      (unscanned) (do
        (def [free-vars functions] (scan name body params))
        (ref/set scan-ref (free-vars/unresolved free-vars functions))
        [free-vars functions])
      (unresolved free-vars functions) [free-vars functions]
      (resolved free-vars) [free-vars {}]))

  (var- free-var-accesses nil)

  (defn- union-variable [into variable [read? written?]]
    (if (has-key? into variable)
      (let [[old-read? old-written?] (in into variable)]
        (put into variable [(or read? old-read?) (or written? old-written?)]))
      (put into variable [read? written?])))

  (defn- compute-free-var-accesses [t]
    (def result @{})
    (def [free-vars functions] (memoized-scan t))
    (eachp [variable access-types] free-vars
      (union-variable result variable access-types))
    (eachp [function bound-vars] functions
      (function/match function
        (builtin _ _ _) nil
        (custom impl) (do
          (def free-var-set (free-var-accesses impl))
          (loop [[free-var access-types] :pairs free-var-set
                 :unless (in bound-vars free-var)]
            (union-variable result free-var access-types)))))
    result)

  (set free-var-accesses (fn impl/free-var-accesses [t]
    (def {:free-var-access-ref free-var-access-ref} t)
    (def currently (ref/get free-var-access-ref))
    (when (= currently :computing) (break @{}))
    (when currently (break currently))
    (ref/set free-var-access-ref :computing)
    (def result (compute-free-var-accesses t))
    (ref/set free-var-access-ref result)
    result))

  (defn- sort-params [params]
    (sort params (fn [param1 param2]
      (cond
        (= (param/access param1) (param/access param2))
          (< (param/name param1) (param/name param2))
        (< (param/access param1) (param/access param2))
          true
        false))))

  (defn implicit-params [t]
    (def {:implicit-params-ref implicit-params-ref} t)
    (ref/get-or-put implicit-params-ref
      (sort-params (seq [[variable [read? write?]] :pairs (free-var-accesses t)]
        (param/new variable (param-sig/new (variable/type variable) (cond
          (and read? write?) :inout
          read? :in
          write? :out
          (error "BUG: free variable not actually used"))))))))
  )

(import ./builtins-prelude)
(defn try-coerce-expr [value]
  (if (expr? value)
    value
    (pat/match value
      |number? (expr/literal type/float value)
      |variable? (expr/identifier value)
      |tuple? (let [args (map try-coerce-expr value)]
        (if (some nil? args)
          nil
          (expr/call (builtins-prelude/resolve-vec-constructor nil nil "[]" (tmap expr/type args)) args)))
      |boolean? (expr/literal type/bool value)
      |int64? (expr/literal type/int value)
      |uint64? (expr/literal type/uint value)
      nil)))

(defn coerce-expr [value]
  (assert (try-coerce-expr value) (errorf "Can't coerce %q into an expression" value)))

(def- multifunction-proto @{:type 'function})
(defn multifunction? [t] (and (table? t) (= (table/getproto t) multifunction-proto)))
(defmodule multifunction
  # So this is kinda tricky. When we declare a jlsl multifunction, we
  # want to be able to call it like a regular function, and have it
  # return an expression node. But we also want to be able to look up
  # the corresponding multifunction without invoking it, so that we
  # can implement it. This registry is how we do that.
  (var- multifunction-registry (table/weak-keys 64))

  # multifunction -> function
  (defn resolve-overload [{:name name :overloads overloads :fallback fallback} arg-types]
    (assertf (tuple? arg-types) "arg-types must be a tuple, got %q" arg-types)
    (or (overloads arg-types)
      (if fallback (fallback arg-types))
      (errorf "%s: no overload for arguments %q" name (tuple/brackets ;(map type/to-glsl arg-types)))))

  (defn- resolve-multifunction [t]
    (if (multifunction? t)
      t
      (or (in multifunction-registry t)
        (errorf "Cannot coerce %q to a multifunction" t))))

  (defn register-wrapper [wrapper multifunction]
    (assert (multifunction? multifunction) "register-wrapper needs a multifunction")
    (put multifunction-registry wrapper multifunction)
    wrapper)

  # given a multifunction wrapper and a signature, returns a particular function
  # in the mulifunction
  # TODO: the only place we call this is in `main`. there's probably a better way...
  (defn resolve-function [t arg-types]
    (resolve-overload (resolve-multifunction t) arg-types))

  (defn resolve-impl [t arg-types]
    (function/match (resolve-overload (resolve-multifunction t) arg-types)
      (custom impl) impl
      (builtin name _ _) (errorf "cannot implement builtin %s" name)))

  (defn implement [t return-type params body]
    (function/custom (impl/implement (resolve-impl t (tmap param/type params))
      return-type params body)))

  (defn- wrapper-function [multifunction]
    (assert (multifunction? multifunction))
    (def wrapper (fn [& args]
      (def args (map coerce-expr args))
      (expr/call (resolve-overload multifunction (tmap expr/type args)) args)))
    (register-wrapper wrapper multifunction))

  (defn new [name overloads]
    (wrapper-function
      (table/setproto @{:name name :overloads overloads} multifunction-proto)))

  (defn single [name return-type param-sigs]
    (new name
      @{(tmap param-sig/type param-sigs)
        (function/custom
          (impl/new name return-type param-sigs))}))

  (defn extend [t return-type param-sigs]
    (def t (resolve-multifunction t))
    (def overloads (in t :overloads))
    (def types (tmap param-sig/type param-sigs))
    (def new-function (function/custom (impl/new (in t :name) return-type param-sigs)))
    (if (table? overloads)
      (do
        (assert (not (has-key? overloads types)) "duplicate overload!")
        (put overloads types new-function))
      (do
        (put t :fallback overloads)
        (put t :overloads @{types new-function}))))

  (defn param-sigs [t arg-types]
    (impl/param-sigs (resolve-overload t arg-types)))

  (defn name [{:name name}] name))

(defmodule function
  (defn name [t]
    (function/match t
      (builtin name _ _) name
      (custom impl) (impl/name impl)))

  (defn implicit-params [t]
    (function/match t
      (builtin _ _ _) []
      (custom impl) (impl/implicit-params impl))))

(set function/param-sigs (fn function/param-sigs [t]
  (function/match t
    (builtin _ _ param-sigs) param-sigs
    (custom impl) (impl/param-sigs impl))))
