(use judge)
(use ./util)
(import pat)
(import ../ordered)
(import ./syntax)
(import ./shape)
(import ./environment/derive :prefix "environment/derive/")
(import ../jlsl)
(import ./expression-hoister)

(defn- chunk-string [str]
  (var unread true)
  (fn [buf _]
    (when unread
      (set unread false)
      (buffer/blit buf str))))

(defn- get-parse-error [parser where]
  (def buf @"")
  (with-dyns [*err* buf *err-color* false]
    (bad-parse parser where))
  [(string/slice buf 0 -2) nil])

(defn- get-compile-error [msg macro-fiber where &opt line col]
  (def buf @"")
  (with-dyns [*err* buf *err-color* false]
    (bad-compile msg nil where line col))
  [(string/slice buf 0 -2) macro-fiber])

(defn unhoist [env expr]
  (def hoisted-vars (in env expression-hoister/*hoisted-vars*))
  (def references (ordered/table/new))
  (def seen @{})
  (defn visit [node]
    (when (seen node) (break))
    (put seen node true)
    (if (jlsl/variable? node)
      (when-let [expr (in hoisted-vars node)]
        (visit expr)
        (unless (ordered/table/has-key? references node)
          (ordered/table/put references node expr)))
      (pat/match node
        ,(jlsl/@function/custom impl) (walk visit (impl :body))
        (walk visit node))))
  (walk visit expr)

  (def to-hoist (ordered/table/pairs references))
  (if (empty? to-hoist)
    expr
    (jlsl/with-expr to-hoist [] expr "hoist")))

# this returns the resulting environment
(defn evaluate [script]
  (def env (environment/derive/new))

  (def errors @[])
  (var implicit-subject nil)

  (run-context {
    :env env
    :chunks (chunk-string script)
    :source "script"
    :expander syntax/expand
    :on-parse-error (fn [& args]
      (put env :exit true)
      (array/push errors (get-parse-error ;args)))
    :on-compile-error (fn [& args]
      (put env :exit true)
      (array/push errors (get-compile-error ;args)))
    :on-status (fn [fiber value]
      (unless (= (fiber/status fiber) :dead)
        (array/push errors [value fiber]))
      (when (shape/shape? value)
        (set implicit-subject value)))
    })

  (unless (empty? errors)
    (def [error-message error-fiber] (first errors))
    (if error-fiber
      (propagate error-message error-fiber)
      (error error-message)))

  # we declare all of the user-settable variables
  # in the environment's prototype, while any variables
  # that the user declares will be in this one. we don't
  # need to unhoist user-declared values, because we'll never
  # try to read them.
  (def stdenv (table/getproto env))

  (when (nil? (get-var stdenv 'subject))
    (set-var stdenv 'subject implicit-subject))

  (loop [[name entry] :pairs stdenv
         :when (and (symbol? name) (table? entry) (has-key? entry :ref))
         :let [ref (entry :ref) value (get ref 0)]]
    (def unhoisted
      (cond
        (shape/shape? value) (shape/map value (partial unhoist env))
        (jlsl/expr? value) (unhoist env value)
        (jlsl/variable? value) (unhoist env (jlsl/expr/identifier value))
        value))
    (put ref 0 unhoisted))

  env)

(import ../jlsl)
(defn- make-presentable [entry]
  (def value (or (in entry :value) (in (in entry :ref) 0)))
  (if (shape/shape? value)
    (shape/map value jlsl/show)
    value))

(defn- run [script]
  (def env (evaluate script))
  (def user-defined
    (tabseq [[sym entry] :pairs env :when (symbol? sym) :when (table? entry) :unless (entry :private)]
      sym (make-presentable entry)))
  (put user-defined 'subject (make-presentable (in env 'subject)))
  user-defined)

(deftest "subject defaults to the final result"
  (test (run "(circle 10)")
    @{subject {:fields {:distance [sdf-circle 10]}
               :tag <1>
               :type [<2> vec [<3> float] 2]}}))

(deftest "if subject is set explicitly, the final result does not matter"
  (test (run `
    (set subject 1)
    (circle 10)
    `)
    @{subject 1}))

(deftest "view macro"
  (test (run `
    (circle 10 | view)
    (circle 20)
    `)
    @{subject {:fields {:distance [sdf-circle 10]}
               :tag <1>
               :type [<2> vec [<3> float] 2]}}))
