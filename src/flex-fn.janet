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

(defn typecheck [expected-type value]
  (if (nil? expected-type) value
    (let [actual-type (typeof value)]
      (if (= expected-type actual-type)
        value
        (errorf "%s: expected %s, got %s" (dyn :fn-name) expected-type actual-type)))))

(defn- def-param [name type]
  ~(def ,name @{:type ,type :value ',unset :name ',name}))

(defn- set? [value]
  (not= value unset))

(defn set-param [param value]
  (if (set? (param :value))
    (errorf "%s: %s specified multiple times" (dyn :fn-name) (param :name))
    (set (param :value) (typecheck (param :type) value))))

# the error message here only makes sense if this is
# used for handling positional arguments
(defn set-first [params value]
  (prompt :break
    (each param params
      (when (not (set? (param :value)))
        (set-param param value)
        (return :break)))
    (errorf "%s: unexpected argument %p" (dyn :fn-name) value)))

(defn- get-param [param default-value]
  (let [value (param :value)]
    (if (set? value) value
      (if (set? default-value) default-value
        (errorf "%s: %s: missing required argument" (dyn :fn-name) (param :name))))))

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
            (errorf "%s: named argument %s without value" (dyn :fn-name) arg)
            (dispatch (args (++ i)))))))
    (def type (typeof arg))
    (unless handled-as-keyword
      (if-let [dispatch (spec type)]
        (dispatch arg)
        (errorf "%s: unexpected argument %p" (dyn :fn-name) arg)))
    (++ i)))

(defmacro- swap [f arg1 arg2]
  ~(,f ,arg2 ,arg1))

(defn get-strict [list index default-value]
  (if (< index (length list))
    (list index)
    default-value))

(defmacro def-flexible-fn [fn-name bindings spec & body]
  (def param-defs
    (swap map bindings (fn [binding]
      (def [name arg] binding)
      (case (tuple/type binding)
        :parens ~(var ,name ,arg)
        :brackets (def-param name arg)))))
  (def get-bindings-defs
    (swap mapcat bindings (fn [binding]
      (if (= (tuple/type binding) :parens) []
        (let [[name type] binding
              default-value (get-strict binding 2 ~(quote ,unset))]
          ~(,name (,get-param ,name ,default-value)))))))
  (def $args (gensym))
  ~(defn ,fn-name [& ,$args]
    (with-dyns [:fn-name ',fn-name]
      ,;param-defs
      (,handle-args ,$args ,spec)
      (let ,get-bindings-defs ,;body))))
