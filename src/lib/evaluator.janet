(use judge)
(use ./util)
(import ./syntax)
(import ./field-set)

# TODO: only doing this because of the stupid weak table bug
(import ../jlsl)

# we create a copy here so that we don't import private bindings
(def bauble-env (merge-module (make-env root-env) (require "./environment")))

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

(defn can-be-subject? [x] (field-set/is? x))

# this returns the resulting environment
(defn evaluate [script]
  # TODO: remove this once the bug is fixed
  (jlsl/multifunction/fix-the-weak-table-bug)
  (def env (make-env bauble-env))

  (loop [[sym entry] :pairs bauble-env
         :when (and (symbol? sym) (table? entry))
         :let [ref (in entry :ref)]
         :when (and (array? ref) (= (length ref) 1))]
    (put env sym @{:doc (in entry :doc) :ref @[(in ref 0)]}))

  (var last-value nil)
  (def errors @[])

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
      (if (can-be-subject? value)
        (set last-value value)))
    })

  (unless (empty? errors)
    (def [error-message error-fiber] (first errors))
    (if error-fiber
      (propagate error-message error-fiber)
      (error error-message)))

  (when (nil? (get-var env 'subject))
    (set-var env 'subject last-value))

  env)

# now we want to invoke a function that's like "given this subject, give me a JLSL program to compile"

(import ../jlsl)
(defn- make-presentable [value]
  (if (field-set/is? value)
    (tabseq [[k v] :pairs value] k (jlsl/show v))
    value))

(defn- run [script]
  (tabseq [[sym entry] :pairs (evaluate script) :when (symbol? sym)]
    sym (make-presentable (or (in entry :value) (in (in entry :ref) 0)))))

(deftest "subject defaults to the final result"
  (test (run "(circle 10)")
    @{subject @{:distance [- [length q] 10]
                :tag <1>
                :type :vec2}}))

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
    @{subject @{:distance [- [length q] 10]
                :tag <1>
                :type :vec2}}))
