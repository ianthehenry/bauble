(def type/bool @"bool")
(def type/vec2 @"vec2")
(def type/vec3 @"vec3")
(def type/vec4 @"vec4")
(def type/float @"float")
(def type/keyword @"keyword")
(def type/3d @"3d-sdf")
(def type/axis @"axis")
(def type/signed-axis @"signed-axis")
(def- type/unknown @"unknown")
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
    :number type/float
    :boolean type/bool
    :keyword (get keyword-types value type/unknown)
    :struct type/3d # TODO: obviously this is wrong once I add 2D SDFs
    :tuple (case (length value)
      2 type/vec2
      3 type/vec3
      4 type/vec4
      type/unknown)
    type/unknown))

(defn typecheck [name expected-type value]
  (if (tuple? expected-type)
    (if (find |(= (typeof value) $) expected-type)
      value
      (errorf "%s type mismatch: %p should be one of %s"
        name value (string/join expected-type ", ")))
    (if (= expected-type (typeof value))
      value
      (errorf "%s type mismatch: %p should be %s"
        name value expected-type))))

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
        (errorf "unexpected argument %p" arg)))
    (++ i)))

(defn- binding-default-value [binding]
  (if (and (tuple? binding) (= (tuple/type binding) :brackets))
    (binding 1)
    unset))

(defn- binding-name [binding]
  (if (tuple? binding)
    (binding 0)
    binding))

(defmacro def-flexible-fn [fn-name bindings spec & body]
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
  ~(defn ,fn-name [& ,$args]
    (try (do
      ,;param-defs
      (,handle-args ,$args ,spec)
      ,;check-required-params
      ,;body)
    ([e]
      (if (string? e)
        (errorf "(%s) %s" ',fn-name e)
        (error e))))))
