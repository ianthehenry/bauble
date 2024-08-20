(use judge)
(use ./util)
(use ./types)
(use ./type)
(use ./expr)
(import ./builtins)
(use ./builtin-test-helpers)

# "flexins" are functions that *either* behave as builtins, if
# invoked with expression arguments, or behave as regular Janet
# functions, if invoked without any.
#
# however the janet versions are not overloaded, such that
# e.g. (sin [1 2 3]) does not work

(defn- simple-expr? [arg]
  (or (expr? arg) (variable? arg)))

(defn- definitely-not-expr? [arg]
  (or (string? arg) (symbol? arg)))

(defn- none [f arg]
  (not (some f arg)))

(defn- vector-expr? [arg]
  (and (indexed? arg)
    (some simple-expr? arg)
    (none definitely-not-expr? arg)))

(defn- expr-args? [args]
  (and
    (some |(or (simple-expr? $) (vector-expr? $)) args)
    (none definitely-not-expr? args)))

(test (expr-args? [1 2 3]) nil)
(test (expr-args? [[1 2 3]]) nil)
(test (expr-args? [[[1 2 3]]]) nil)
(test (expr-args? [[1 (expr/dot (variable/new "foo" type/float) 'x) 3]]) true)
(test (expr-args? [[1 (variable/new "foo" type/float) 3]]) true)
(test (expr-args? (variable/new "foo" type/float)) nil)

# the representation of an expression with a variable looks like a tuple
# with an expression, if you're very naive
(test (expr-args? (expr/dot (variable/new "foo" type/float) 'x)) false)

(defmacro- defflex [sym &opt alt]
  (default alt sym)
  ~(def ,sym (multifunction/register-wrapper (fn ,(symbol "flex/" sym) [& args]
    # TODO: this isn't really right. Consider
    # (+ 1 [1 2 p.x]). The tuple isn't an expression,
    # but it contains one. Similarly variables
    # aren't expressions, but they could be.
    ((if (,expr-args? args) ,(symbol "builtins/" sym) ,alt) ;args))
    ,(symbol "builtins/" sym))))

(defn- vector-length [vec] (math/sqrt (reduce (fn [acc v] (+ acc (* v v))) 0 vec)))
(defn- non-short-circuiting-or [& args]
  (if (empty? args) nil (reduce |(or $0 $1) false args)))
(defn- non-short-circuiting-and [& args]
  (reduce |(and $0 $1) true args))
# this is a weird inconsistency, but we'll follow along...
(test [(or) (non-short-circuiting-or)] [nil nil])
(test [(and) (non-short-circuiting-and)] [true true])

(test (non-short-circuiting-or false true true) true)
(test (non-short-circuiting-and false true true) false)

(defflex +)
(defflex -)
(defflex *)
(defflex /)
(defflex %)
(defflex <)
(defflex >)
(defflex >=)
(defflex <=)
(defflex =)
(defflex not=)
(defflex max)
(defflex min)
(defflex mod)
(defflex not)

(put (curenv) '@length (dyn 'length))
(put (curenv) '@and (dyn 'and))
(put (curenv) '@or (dyn 'or))
(defflex length vector-length)
(defflex or non-short-circuiting-or)
(defflex and non-short-circuiting-and)

(defn- non-flexible []
  (seq [[key entry] :pairs (require "./builtins")
        :when (and (symbol? key) (table? entry))
        :unless (in entry :private)
        :unless (table/rawget (curenv) key)]
    key))

(eval (seq [sym :in (non-flexible)
            :let [math-equivalent (symbol "math/" sym)]
            :when (has-key? (curenv) math-equivalent)]
  (call defflex sym math-equivalent)))

(defn- float [x] (expr/literal type/float x))

# this is a bit different, as these aren't actually function calls...
# but i think i'll count it?
(put (curenv) '@in (dyn 'in))
(defn in [& args]
  (if (expr-args? args)
    (do
      (assert (= (@length args) 2) "cannot specify a default value in a GL in expression")
      (def [structure key] args)
      (expr/in (coerce-expr structure) (coerce-expr key)))
    (@in ;args)))

(test (in (float 1) (float 2))
  [<1>
   in
   [<1>
    literal
    [<2> primitive [<3> float]]
    1]
   [<1>
    literal
    [<2> primitive [<3> float]]
    2]])

(deftest "builtins without flexin equivalents"
  (test (sort (seq [key :in (non-flexible)
                    :unless (string/has-prefix? "mat" key)
                    :unless (string/has-prefix? "vec" key)]
    (if (or (has-key? (curenv) key)) key [key])))
    @[[bool]
      [clamp]
      [cross]
      [distance]
      [dot]
      [double]
      [equal]
      [faceforward]
      [fma]
      [fract]
      [int]
      [inversesqrt]
      [mix]
      [normalize]
      [not-equal]
      [reflect]
      [refract]
      [round-even]
      [sign]
      [smoothstep]
      [step]
      [uint]
      [xor]]))

(test (sin 0) 0)
(typecheck (sin (float 0)) [:float -> :float])
(test (+ 1 2) 3)
(typecheck (+ (float 1) 2) [:float :float -> :float])

(test (length [3 4]) 5)
(test (@length [1 2 3]) 3)

(test-stdout (and false (print "hello")) `
  hello
`)
(test-stdout (do (print "ian fix this judge bug") (@and false (print "hello"))) `
  ian fix this judge bug
`)

(def- foo (variable/new "name" type/vec2))
(typecheck (+ (float 1) [1 foo 3]) [:float :vec4 -> :vec4])
