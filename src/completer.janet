(import ./bauble-evaluator)

(defn- parse-usage [name usage]
  (def docname (string name))
  (def capture
    (peg/match ~(* "(" ,(string docname) (+ (* " " (<- (to (* ")" -1)))) (* (constant "") ")" -1))) usage))
  (if (or (nil? capture) (not= (length capture) 1))
    (errorf "failed to parse %q : %q" name usage))
  (in capture 0))

(defn- parse-docstring [name docstring]
  (def ix (string/find "\n\n" docstring))
  (unless ix
    (errorf "unable to parse docstring %q" docstring))
  [(parse-usage name (string/slice docstring 0 ix))
   (string/slice docstring (+ ix 2))])

(defn functiony? [x] (or (function? x) (cfunction? x)))

(defn get-definitions []
  (seq [[name binding] :pairs (table/proto-flatten bauble-evaluator/bauble-env)
         :when (symbol? name)
         :let [{:value value :doc docstring :macro macro} binding]
         # lots of built-ins, like net/write, are nil in webassembly
         :when (not (nil? value))
         :when (not (nil? docstring))
         ]
    (when (and (cfunction? value) macro)
      (errorf "cfunction macro?? name"))
    (if (functiony? value)
      [(string name) ;(parse-docstring name docstring) (if macro 2 1)]
      [(string name) "" docstring 0])))
