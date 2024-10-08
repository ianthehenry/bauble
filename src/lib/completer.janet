(use judge)
(import ./environment/derive)

(def- declaration-peg (peg/compile
  ~{:main (* "(" :identifier :usage ")" -1)
    :usage (* (? " ") '(to (* ")" -1)))
    :identifier (some (+ (range "09" "AZ" "az") (set "!$%&*+-./:<?=>@^_")))}))

(defn- parse-usage [usage]
  (def capture (peg/match declaration-peg usage))
  (if (or (nil? capture) (not= (length capture) 1))
    (errorf "failed to parse %q" usage))
  (in capture 0))

(test (parse-usage "(foo bar baz)") "bar baz")
(test (parse-usage "(foo bar &opt baz)") "bar &opt baz")
(test (parse-usage "(foo bar &opt baz)") "bar &opt baz")
(test (parse-usage "(tuple/slice arrtup [,start=0 [,end=(length arrtup)]])") "arrtup [,start=0 [,end=(length arrtup)]]")

(defn- parse-docstring [name docstring]
  (def ix (string/find "\n\n" docstring))
  (unless ix (errorf "unable to parse docstring %q" docstring))
  [(parse-usage (string/slice docstring 0 ix))
   (string/slice docstring (+ ix 2))])

(defn- functiony? [x] (or (function? x) (cfunction? x)))

# [name usage tag]
# where tag=0 for value, tag=1 for functions, tag=2 for macros
(defn get-definitions []
  (seq [[name binding] :pairs (table/proto-flatten (derive/new))
         :when (symbol? name)
         :let [{:value value :ref ref :doc docstring :macro macro} binding]
         :let [value (if ref (ref 0) value)]
         :when (not (nil? docstring))
         ]
    (when (and (cfunction? value) macro)
      (errorf "cfunction macro?? %s" name))
    (if (functiony? value)
      [(string name) ;(parse-docstring name docstring) (if macro 2 1)]
      [(string name) "" docstring 0])))
