(use judge)
(use ./util)
(import ./syntax)

(def bauble-env (require "./environment"))

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

# TODO: this should only return true if given a fieldset
(defn can-be-subject? [x] true)

# this returns the resulting environment
(defn evaluate [script]
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

(defn run [script]
  (tabseq [[sym entry] :pairs (evaluate script) :when (symbol? sym)]
    sym (or (in entry :value) (in (in entry :ref) 0))))

(deftest "subject defaults to the final result"
  (test (run "(1 + 2)")
    @{subject 3}))

(deftest "if subject is set explicitly, the final result does not matter"
  (test (run `
    (set subject 1)
    10
    `)
    @{subject 1}))

(deftest "view macro"
  (test (run `
    (10 | view)
    20
    `)
    @{subject 10}))
