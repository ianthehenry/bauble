(use judge)
(use ./util)
(use ./types)
(use ./expr)

(defmacro jlsl/defdyn [name type docstring]
  ~(def ,name ,docstring (,variable/new ,(string name) ,(type/of-ast type))))

(defmacro- jlsl/stub [return-type name param-sigs]
  ~(,multifunction/single ,name ,(type/of-ast return-type) [,;(map param-sig/of-ast param-sigs)]))

# declare returns a multifunction
(defmacro jlsl/declare [return-type name param-sigs &opt docstring]
  (def docstring (if docstring [docstring] []))
  ['def name ;docstring (call jlsl/stub return-type (string name) param-sigs)])
(defmacro jlsl/declare- [return-type name param-sigs &opt docstring]
  (def docstring (if docstring [docstring] []))
  ['def- name ;docstring (call jlsl/stub return-type (string name) param-sigs)])

(defmacro jlsl/declare-overload [return-type name param-sigs]
  [multifunction/extend name (type/of-ast return-type) [tuple ;(map param-sig/of-ast param-sigs)]])

# implement takes a multifunction and returns a single function
(defmacro jlsl/implement [return-type name params & body]
  (def $return-type (gensym))
  (def $params (gensym))
  (def $body (gensym))

  (def <params> (seq [[sig name] :in (partition 2 params)]
    (def <sig> (param-sig/of-ast sig))
    (with-syms [$sig]
      ~(upscope
        (def ,$sig ,<sig>)
        (def ,name (,variable/new ,(string name) (,param-sig/type ,$sig)))
        (,param/new ,name ,$sig)))))

  (def <body> (seq [statement-ast :in body]
    ~(,array/push ,$body ,(statement/of-ast statement-ast))))

  ~(do
    (def ,$return-type ,(type/of-ast return-type))
    (def ,$params [,;<params>])
    (def ,$body @[])
    ,;<body>
    (,multifunction/implement ,name ,$return-type ,$params ,$body)))

# fn returns a single function
(defmacro jlsl/fn [return-type name params & body]
  (call jlsl/implement return-type (call jlsl/stub return-type name (map 0 (partition 2 params))) params ;body))

(defn- get-docstring [name params body]
  (def preamble (string/format "(%s %s)\n\n" name (string/join (map 1 (partition 2 params)) " ")))
  (if (string? (first body))
    [(string preamble (first body)) (drop 1 body)]
    [preamble body]))

# defn defines a multifunction but returns a single function
(defmacro jlsl/defn [return-type name params & body]
  (def [docstring body] (get-docstring name params body))
  ['upscope
    (call jlsl/declare return-type name (map 0 (partition 2 params)) docstring)
    (call jlsl/implement return-type name params ;body)])
(defmacro jlsl/defn- [return-type name params & body]
  (def [docstring body] (get-docstring name params body))
  ['upscope
    (call jlsl/declare- return-type name (map 0 (partition 2 params)) docstring)
    (call jlsl/implement return-type name params ;body)])
(defmacro jlsl/overload [return-type name params & body]
  ['upscope
    (call jlsl/declare-overload return-type name (map 0 (partition 2 params)))
    (call jlsl/implement return-type name params ;body)])

(defmacro jlsl/defstruct [name & fields]
  (def field-names (map 1 (partition 2 fields)))
  (def <param-types> (map (comp type/of-ast 0) (partition 2 fields)))
  (def <type> (type/of-ast ~(struct ,name ,;fields)))
  (with-syms [$type $f $args]
    ~(upscope
      (def- ,$type ,<type>)
      (def- ,$f (,function/builtin ,(string name) ,$type (,map ,param-sig/in ,<param-types>)))
      (defn ,name [,;field-names]
        (,expr/call ,$f (,map ,coerce-expr [,;field-names])))
      (,type/register-constructor ,name ,$type))))

(use ./builtins)
(use ./flexins)

(test-macro (jlsl/defstruct Foo :float bar :float baz)
  (upscope
    (def- <1> (@type/struct "Foo" (@new (quote bar) (@type/primitive (quote (<2> float))) (quote baz) (@type/primitive (quote (<2> float))))))
    (def- <3> (@function/builtin "Foo" <1> (@map @in @[(@type/primitive (quote (<2> float))) (@type/primitive (quote (<2> float)))])))
    (defn Foo
      [bar baz]
      (@expr/call <3> (@map @coerce-expr [bar baz])))
    (@register-constructor Foo <1>)))

(test-macro (jlsl/declare :float incr [:float])
  (def incr (@single "incr" (@type/primitive (quote (<1> float))) [(@new (@type/primitive (quote (<1> float))) :in)])))

(test-macro (jlsl/declare :float incr [:float] "docstring")
  (def incr "docstring" (@single "incr" (@type/primitive (quote (<1> float))) [(@new (@type/primitive (quote (<1> float))) :in)])))

(test-macro (jlsl/implement :float incr [:float x] (return x))
  (do
    (def <1> (@type/primitive (quote (<2> float))))
    (def <3> [(upscope (def <4> (@new (@type/primitive (quote (<2> float))) :in)) (def x (@new "x" (@type <4>))) (@new x <4>))])
    (def <5> @[])
    (@array/push <5> (@statement/return (@coerce-expr x)))
    (@implement incr <1> <3> <5>)))
(test-macro (jlsl/defn :float incr [:float x] (return x))
  (upscope
    (def incr "(incr x)\n\n" (@single "incr" (@type/primitive (quote (<1> float))) [(@new (@type/primitive (quote (<1> float))) :in)]))
    (do
      (def <2> (@type/primitive (quote (<1> float))))
      (def <3> [(upscope (def <4> (@new (@type/primitive (quote (<1> float))) :in)) (def x (@new "x" (@type <4>))) (@new x <4>))])
      (def <5> @[])
      (@array/push <5> (@statement/return (@coerce-expr x)))
      (@implement incr <2> <3> <5>))))

(test-macro (jlsl/defn :void foo [:float x :float y]
  "docstring"
  (var x 1)
  (return [x 2 3]))
  (upscope
    (def foo "(foo x y)\n\ndocstring" (@single "foo" (quote (<1> void)) [(@new (@type/primitive (quote (<2> float))) :in) (@new (@type/primitive (quote (<2> float))) :in)]))
    (do
      (def <3> (quote (<1> void)))
      (def <4> [(upscope (def <5> (@new (@type/primitive (quote (<2> float))) :in)) (def x (@new "x" (@type <5>))) (@new x <5>)) (upscope (def <6> (@new (@type/primitive (quote (<2> float))) :in)) (def y (@new "y" (@type <6>))) (@new y <6>))])
      (def <7> @[])
      (@array/push <7> (upscope (def <8> (@expr/literal (quote (<1> primitive (<2> float))) 1)) (def <9> (@expr/type <8>)) (def x (@new "x" <9>)) (@statement/declaration false x <8>)))
      (@array/push <7> (@statement/return (@call (quote "<function 0x1>") @[(@coerce-expr x) (@expr/literal (quote (<1> primitive (<2> float))) 2) (@expr/literal (quote (<1> primitive (<2> float))) 3)])))
      (@implement foo <3> <4> <7>))))

(test (jlsl/defn :void foo [:float x :float y]
  (var z 1)
  (return (+ x y z)))
  [<1>
   custom
   {:body @[[<5>
             declaration
             false
             [<6>
              lexical
              <7>
              "z"
              [<3> primitive [<4> float]]]
             [<8>
              literal
              [<3> primitive [<4> float]]
              1]]
            [<5>
             return
             [<8>
              call
              [<1>
               builtin
               "+"
               [<3> primitive [<4> float]]
               @[[<2> [<3> primitive [<4> float]] :in]
                 [<2> [<3> primitive [<4> float]] :in]
                 [<2> [<3> primitive [<4> float]] :in]]]
              @[[<8>
                 identifier
                 [<6>
                  lexical
                  <9>
                  "x"
                  [<3> primitive [<4> float]]]]
                [<8>
                 identifier
                 [<6>
                  lexical
                  <10>
                  "y"
                  [<3> primitive [<4> float]]]]
                [<8>
                 identifier
                 [<6>
                  lexical
                  <7>
                  "z"
                  [<3> primitive [<4> float]]]]]]]]
    :declared-param-sigs [[<2> [<3> primitive [<4> float]] :in]
                          [<2> [<3> primitive [<4> float]] :in]]
    :declared-return-type [<3> void]
    :free-var-access-ref @[]
    :implicit-params-ref @[]
    :name "foo"
    :params @[[[<6>
                lexical
                <9>
                "x"
                [<3> primitive [<4> float]]]
               [<2> [<3> primitive [<4> float]] :in]]
              [[<6>
                lexical
                <10>
                "y"
                [<3> primitive [<4> float]]]
               [<2> [<3> primitive [<4> float]] :in]]]
    :scan-ref @[[<11> unscanned]]}])

(test (jlsl/defn :void foo [:float x :float y]
  (var z 0)
  (for (var i 0) (< i 10) (++ i)
    (+= z i))
  (return (+ x y z)))
  [<1>
   custom
   {:body @[[<5>
             declaration
             false
             [<6>
              lexical
              <7>
              "z"
              [<3> primitive [<4> float]]]
             [<8>
              literal
              [<3> primitive [<4> float]]
              0]]
            [<5>
             for
             [<5>
              declaration
              false
              [<6>
               lexical
               <9>
               "i"
               [<3> primitive [<4> float]]]
              [<8>
               literal
               [<3> primitive [<4> float]]
               0]]
             [<8>
              call
              [<1>
               builtin
               "<"
               [<3> primitive [<4> bool]]
               @[[<2> [<3> primitive [<4> float]] :in]
                 [<2> [<3> primitive [<4> float]] :in]]]
              @[[<8>
                 identifier
                 [<6>
                  lexical
                  <9>
                  "i"
                  [<3> primitive [<4> float]]]]
                [<8>
                 literal
                 [<3> primitive [<4> float]]
                 10]]]
             [<5>
              expr
              [<8>
               crement
               ++
               [<8>
                identifier
                [<6>
                 lexical
                 <9>
                 "i"
                 [<3> primitive [<4> float]]]]]]
             [[<5>
               update
               +=
               [<8>
                identifier
                [<6>
                 lexical
                 <7>
                 "z"
                 [<3> primitive [<4> float]]]]
               [<8>
                identifier
                [<6>
                 lexical
                 <9>
                 "i"
                 [<3> primitive [<4> float]]]]]]]
            [<5>
             return
             [<8>
              call
              [<1>
               builtin
               "+"
               [<3> primitive [<4> float]]
               @[[<2> [<3> primitive [<4> float]] :in]
                 [<2> [<3> primitive [<4> float]] :in]
                 [<2> [<3> primitive [<4> float]] :in]]]
              @[[<8>
                 identifier
                 [<6>
                  lexical
                  <10>
                  "x"
                  [<3> primitive [<4> float]]]]
                [<8>
                 identifier
                 [<6>
                  lexical
                  <11>
                  "y"
                  [<3> primitive [<4> float]]]]
                [<8>
                 identifier
                 [<6>
                  lexical
                  <7>
                  "z"
                  [<3> primitive [<4> float]]]]]]]]
    :declared-param-sigs [[<2> [<3> primitive [<4> float]] :in]
                          [<2> [<3> primitive [<4> float]] :in]]
    :declared-return-type [<3> void]
    :free-var-access-ref @[]
    :implicit-params-ref @[]
    :name "foo"
    :params @[[[<6>
                lexical
                <10>
                "x"
                [<3> primitive [<4> float]]]
               [<2> [<3> primitive [<4> float]] :in]]
              [[<6>
                lexical
                <11>
                "y"
                [<3> primitive [<4> float]]]
               [<2> [<3> primitive [<4> float]] :in]]]
    :scan-ref @[[<12> unscanned]]}])

(deftest "defn declares a janet function that returns an expr"
  (jlsl/defn :float foo []
    (return 1))
  (test foo "<function 0x1>")
  (test (foo)
    [<1>
     call
     [<2>
      custom
      {:body @[[<3>
                return
                [<1>
                 literal
                 [<4> primitive [<5> float]]
                 1]]]
       :declared-param-sigs []
       :declared-return-type [<4> primitive [<5> float]]
       :free-var-access-ref @[]
       :implicit-params-ref @[]
       :name "foo"
       :params @[]
       :scan-ref @[[<6> unscanned]]}]
     @[]])
  )
