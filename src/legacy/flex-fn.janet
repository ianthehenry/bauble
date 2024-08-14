(import ../glslisp/src/index :as glslisp)
(import ./light)

(import ../glslisp/src/type :as type :export true)
(def type/keyword :keyword)
(def type/3d :3d-sdf)
(def type/fn :fn)
(def type/axis :axis)
(def type/signed-axis :signed-axis)
(def type/light :light)

(def- unset (gensym))

(defmacro- swap [f arg1 arg2]
  ~(,f ,arg2 ,arg1))

(def- keyword-types {
  :x type/axis
  :y type/axis
  :z type/axis
  :+x type/signed-axis
  :+y type/signed-axis
  :+z type/signed-axis
  :-x type/signed-axis
  :-y type/signed-axis
  :-z type/signed-axis
  })

(defn- typeof [value]
  (case (type value)
    :keyword (get keyword-types value type/unknown)
    :struct type/3d # TODO: obviously this is wrong once I add 2D SDFs
    :function type/fn
    (cond
      (light/instance? value) type/light
      (glslisp/typecheck value))))

(defn typecheck [name expected-type value]
  (def actual-type (typeof value))
  (if (tuple? expected-type)
    (if (find |(= actual-type $) expected-type)
      value
      (errorf "%s type mismatch: %s %p should be one of %s"
        name actual-type value (string/join expected-type ", ")))
    (if (= expected-type actual-type)
      value
      (errorf "%s type mismatch: %s %p should be %s"
        name actual-type value expected-type))))

(defn- set? [value]
  (not= value unset))
(defn- unset? [value]
  (= value unset))

(defmacro set-param [param value &opt type]
  ~(if (,set? ,param)
    (errorf "%s specified multiple times" ',param)
    (set ,param ,(if (nil? type) value ~(,typecheck ',param ,type ,value)))))

# the error message here only makes sense if this is
# used for handling positional arguments
(defmacro set-first [params value]
  (let [$value (gensym)]
    (def checks (swap map params (fn [param]
      ~(when (,unset? ,param)
        (set-param ,param ,$value)
        (return :break)))))
    ~(let [,$value ,value]
      (prompt :break
        ,;checks
        (errorf "unexpected argument %p" ,$value)))))

(defn function/max-arity [f]
  ((disasm f) :max-arity))

(defn- handle-args [args spec]
  (var i 0)
  (var last-index (- (length args) 1))
  (while (<= i last-index)
    (def arg (args i))
    (var handled-as-keyword false)
    (when (= (type arg) :keyword)
      (when-let [dispatch (spec arg)]
        (set handled-as-keyword true)
        (if (= (function/max-arity dispatch) 0)
          (dispatch)
          (if (= i last-index)
            (errorf "%s needs a value" arg)
            (dispatch (args (++ i)))))))
    (def type (typeof arg))
    (unless handled-as-keyword
      (if-let [dispatch (spec type)]
        (dispatch arg)
        (errorf "unexpected %s argument %p" type arg)))
    (++ i)))

(defn- binding-default-value [binding]
  (if (and (tuple? binding) (= (tuple/type binding) :brackets))
    (binding 1)
    unset))

(defn- binding-name [binding]
  (if (tuple? binding)
    (binding 0)
    binding))

(defmacro def-flexible-fn [fn-name bindings spec arg-string doc-string & body]
  (unless (string? arg-string)
    (error "no argument string"))
  (unless (string? doc-string)
    (error "no doc string"))
  (def param-defs
    (swap map bindings (fn [binding]
      (def [name initial-value]
        (if (tuple? binding)
          (case (tuple/type binding)
            :parens binding
            :brackets [(binding 0) ~(quote ,unset)])
          [binding ~(quote ,unset)]))
      ~(var ,name ,initial-value))))
  (def check-required-params
    (swap map bindings (fn [binding]
      (def name (binding-name binding))
      (def default-value (binding-default-value binding))
      ~(if (,unset? ,name)
        ,(if (unset? default-value)
          ~(errorf "%s required" ',name)
          ~(set ,name ,default-value))))))
  (def $args (gensym))
  ~(def ,fn-name
    ,(string/format "(%s %s)\n\n%s" fn-name arg-string doc-string)
    (fn [& ,$args]
      (try (do
        ,;param-defs
        (,handle-args ,$args ,spec)
        ,;check-required-params
        ,;body)
      ([e]
        (if (string? e)
          (errorf "(%s) %s" ',fn-name e)
          (error e)))))))
