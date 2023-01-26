(import ./globals)
(import ./light)
(def- bauble-env (make-env root-env))
(each module ["./helpers" "./dsl" "./globals" "./glslisp/src/builtins"]
  (merge-module bauble-env (require module)))

(import ./infix-syntax)
(import ./dot-syntax)

(defn- compilable? [value]
  (and (struct? value)
       (not (nil? (value :compile)))))

(defn- chunk-string [str]
  (var unread true)
  (fn [buf _]
    (when unread
      (set unread false)
      (buffer/blit buf str))))

(def- default-lights
  [(light/point/new ~(+ ,globals/P [1024 1024 512]) [1 1 1] 1 0.25)
   (light/ambient/new [1 1 1] 0.05)])

(defn evaluate [user-script]
  (def env (make-env bauble-env))
  (put env 'lights @{:value default-lights})

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
      (if (compilable? value)
        (set last-value value)))
    })
  # TODO: we can actually record and
  # report multiple errors, although
  # this just reports the first one
  (if (empty? errors)
    [last-value env]
    (if error-fiber
      (propagate (first errors) error-fiber)
      (error (first errors)))))
