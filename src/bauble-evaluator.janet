(def bauble-env (make-env root-env))
(each module ["./helpers" "./dsl" "./globals" "./glslisp/src/builtins"]
  (merge-module bauble-env (require module)))

(import ./infix-syntax)
(import ./dot-syntax)

(defn chunk-string [str]
  (var already-read false)
  (fn [buf _]
    # TODO: i'm ignoring the "number of
    # bytes" requested here, which is
    # probably fine but might be bad.
    # dofile hardcodes 4096
    (if already-read
      nil
      (do
        (set already-read true)
        (buffer/blit buf str)))))

(defn evaluate [user-script]
  (def env (make-env bauble-env))

  (var last-value nil)
  (def errors @[])
  (var error-fiber nil)

  (defn parse-error [&opt x y]
    (def buf @"")
    (with-dyns [*err* buf *err-color* false]
      (bad-parse x y))
    (put env :exit true)
    (array/push errors (string/slice buf 0 -2)))
  (defn compile-error [&opt msg fiber where line col]
    (def buf @"")
    (with-dyns [*err* buf *err-color* false]
      (bad-compile msg nil where line col))
    (array/push errors (string/slice buf 0 -2))
    (put env :exit true)
    (set error-fiber fiber))

  (run-context {
    :env env
    :chunks (chunk-string user-script)
    :source "script"
    :expander (comp infix-syntax/expand dot-syntax/expand)
    :on-parse-error parse-error
    :on-compile-error compile-error
    :on-status (fn [fiber value]
      (unless (= (fiber/status fiber) :dead)
        (array/push errors value)
        (set error-fiber fiber))
      (set last-value value))
    })
  # TODO: we can actually record and
  # report multiple errors, although
  # this just reports the first one
  (if (empty? errors)
    [last-value env]
    (if error-fiber
      (propagate (first errors) error-fiber)
      (error (first errors)))))
