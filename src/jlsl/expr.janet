(use judge)
(use module)
(import pat)
(use ./util)
(use ./types)
(import ./builtins)

(var statement/of-ast nil)
(var statement/of-asts nil)

(defn- call [general-function args]
  (expr/call (multifunction/resolve-function general-function (tmap expr/type args)) args))

(defn iife [name return-type statements]
  (def trivial-expr-body
    (if (= (length statements) 1)
      (statement/match (first statements)
        (return expr) expr
        nil)))
  (or trivial-expr-body
    (call (multifunction/implement (multifunction/single name return-type []) return-type [] statements) [])))

(defn- check-do-statements [statements can-break? can-continue?]
  (each statement statements (statement/match statement
    (declaration _ _ _) nil
    (assign _ _) nil
    (update _ _ _) nil
    (break) (assert can-break? "cannot break in an expression context")
    (continue) (assert can-continue? "cannot continue in an expression context")
    (discard) nil
    (return _) (error "cannot return in an expression context")
    (do body) (check-do-statements body can-break? can-continue?)
    (with _ body) (check-do-statements body can-break? can-continue?)
    (if _ then else) (check-do-statements [then ;(if else [else] [])] can-break? can-continue?)
    (case _ cases) (do
      (each case cases (pat/match case
        [statement] (check-do-statements [statement] true can-continue?)
        [value statement] (check-do-statements [statement] true can-continue?))))
    (while _ body)
      (check-do-statements body true true)
    (do-while _ body)
      (check-do-statements body true true)
    (for _ _ _ body)
      (check-do-statements body true true)
    (expr _) nil)))

(defn- do-expr [statements last-expr name]
  (default name "do")
  (check-do-statements statements false false)
  (iife name (expr/type last-expr) [;statements (statement/return last-expr)]))

(defn- with-expr [bindings statements last-expr name]
  (default name "with")
  (check-do-statements statements false false)
  (let [return-type (expr/type last-expr)]
    (iife (string name "-outer") return-type [
      (statement/with bindings [
        (statement/return (iife (string name "-inner") return-type [;statements (statement/return last-expr)]))
        ])
      ])))

(defn- if-expr [cond then else]
  (assertf (= (expr/type then) (expr/type else))
    "type error: if expressions must match, got %q and %q" (type/to-glsl (expr/type then)) (type/to-glsl (expr/type else)))
  (expr/if cond then else))

(var expr/of-ast nil)

(defn bindings-of-ast [ast]
  [tuple ;(seq [[variable expr] :in (partition 2 ast)]
    [tuple variable (expr/of-ast expr)])])

(varfn expr/of-ast [ast]
  (pat/match ast
    |keyword? [expr/literal ['quote type/int] ast]
    |boolean? [expr/literal ['quote type/bool] ast]
    |number? [expr/literal ['quote type/float] ast]
    |symbol? [coerce-expr ast]
    |btuple? [call ~',builtins/vec (map expr/of-ast ast)]
    ['. expr field] [expr/dot (expr/of-ast expr) ['quote field]]
    ['in expr index] [expr/in (expr/of-ast expr) (expr/of-ast index)]
    ['do & body] (do
      (def [body name] (if (string? (first body))
        [(slice body 1) (first body)]
        [body nil]))
      (assert (> (length body) 0) "do expression cannot be empty")
      [do-expr (statement/of-asts (drop -1 body)) (expr/of-ast (last body)) name])
    ['with bindings & body] (do
      (def [name bindings body]
        (if (string? bindings)
          [bindings (first body) (slice body 1)]
          [nil bindings body]))
      (assert (> (length body) 0) "with expression cannot be empty")
      [with-expr (bindings-of-ast bindings) (statement/of-asts (drop -1 body)) (expr/of-ast (last body)) name])
    ['unquote expr] expr
    [(and op (or '++ '-- '_++ '_--)) expr] [expr/crement ['quote op] (expr/of-ast expr)]
    ['if cond then else] [if-expr (expr/of-ast cond) (expr/of-ast then) (expr/of-ast else)]
    [f & args] [call f (map expr/of-ast args)]
    ))

# takes a list of ASTs and returns code that you can evaluate
# to return a list of statements
(varfn statement/of-asts [asts]
  ~[,;(map statement/of-ast asts)])

# takes the AST of a statement and returns code that
# creates a first-class statement
(varfn statement/of-ast [ast]
  (pat/match ast
    # TODO: we have no way to declare a variable without any initial value
    [(and declaration (or 'def 'var)) name value]
      (with-syms [$expr $type $statement]
        (def const? (= declaration 'def))
        ~(upscope
          (def ,$expr ,(expr/of-ast value))
          (def ,$type (,expr/type ,$expr))
          (def ,name (,variable/new ,(string name) ,$type))
          (,statement/declaration ,const? ,name ,$expr)))
    [(or 'def 'var) &] (errorf "illegal form %q" ast)
    ['set dest value] [statement/assign (expr/of-ast dest) (expr/of-ast value)]
    ['return value] [statement/return (expr/of-ast value)]
    ['unquote expr] expr
    ['break] [statement/break]
    ['continue] [statement/continue]
    [(and op (or
      '+= '*= '/= '-= '%=
      'blshift= 'brshift=
      'bxor= 'band= 'bor=)) dest expr]
      [statement/update ~',op (expr/of-ast dest) (expr/of-ast expr)]
    ['do & body] [statement/do (statement/of-asts body)]
    ['with bindings & body] [statement/with (bindings-of-ast bindings) (statement/of-asts body)]
    ['if cond then & else] (do
      (assert (<= (length else) 1) "too many arguments to if")
      (def else (get else 0))
      [statement/if (expr/of-ast cond)
        (statement/of-ast then)
        (if else (statement/of-ast else))])
    ['case value & cases]
      [statement/case
        (expr/of-ast value)
        [tuple ;(seq [case :in (partition 2 cases)]
          (pat/match case
            [statement] [tuple (statement/of-ast statement)]
            [value statement] [tuple (expr/of-ast value) (statement/of-ast statement)]))]]
    ['while cond & body] [statement/while (expr/of-ast cond) (statement/of-asts body)]
    ['do-while cond & body] [statement/do-while (expr/of-ast cond) (statement/of-asts body)]
    ['for init check advance & body]
      (with-syms [$init]
        ~(let [,$init ,(statement/of-ast init)]
          ,[statement/for $init
            (expr/of-ast check)
            (statement/of-ast advance)
            (statement/of-asts body)]))
    other [statement/expr (expr/of-ast other)]
  ))
