(use judge)
(use module)
(import pat)
(use ./adt)
(use ./util)
(import ../ordered)

(defadt primitive-type
  (float)
  (double)
  (int)
  (uint)
  (bool))

(defadt type
  (void)
  (primitive type)
  (vec type count)
  (mat cols rows) # this is specifically a float matrix; we don't support dmat yet
  (array type length)
  (struct name fields)) # fields is an ordered/table

(defadt variable
  (dynamic id name type)
  (lexical id name type))

(defmodule variable
  (defn new [name type] (variable/lexical (gensym) name type))
  (defn dyn [name type] (variable/dynamic (gensym) name type))

  (defn name [t]
    (variable/match t
      (dynamic _ name _) name
      (lexical _ name _) name))

  (defn type [t]
    (variable/match t
      (dynamic _ _ type) type
      (lexical _ _ type) type)))

(defadt free-vars
  (unscanned)
  (unresolved free-variables function-references)
  (resolved free-variables))

(defadt function
  (builtin name return-type param-sigs)
  (custom impl))

(defadt expr
  (literal type value)
  (identifier variable)
  (call function args)
  (crement op value)
  (dot expr field)
  (in expr index)
  (length expr)
  (if cond then else))

(defadt statement
  (declaration const? variable expr)
  (assign l-value r-value)
  (update op l-value r-value)
  (break)
  (continue)
  (discard)
  (return expr)
  (do body)
  (upscope body)
  (with bindings body)
  (if cond then else)
  (case value cases)
  (while cond body)
  (do-while cond body)
  (for init cond update body)
  (expr expr))

(defmodule primitive-type
  (def short-names
    {:float (primitive-type/float)
     :double (primitive-type/double)
     :int (primitive-type/int)
     :uint (primitive-type/uint)
     :bool (primitive-type/bool)})

  (defn of-ast [ast]
    (if-let [t (in short-names ast)]
      ~',t
      (errorf "%q is not a primitive type" ast)))

  (defn to-glsl [t]
    (primitive-type/match t
      (float) :float
      (double) :double
      (int) :int
      (uint) :uint
      (bool) :bool))

  (defn vec-prefix [t]
    (primitive-type/match t
      (float) "vec"
      (double) "dvec"
      (int) "ivec"
      (uint) "uvec"
      (bool) "bvec")))

(defmodule type
  (def float (type/primitive (primitive-type/float)))
  (def int (type/primitive (primitive-type/int)))
  (def uint (type/primitive (primitive-type/uint)))
  (def double (type/primitive (primitive-type/double)))
  (def bool (type/primitive (primitive-type/bool)))
  (def vec2 (type/vec (primitive-type/float) 2))
  (def vec3 (type/vec (primitive-type/float) 3))
  (def vec4 (type/vec (primitive-type/float) 4))

  (def short-names
    {:void (type/void)
     :vec2 (type/vec (primitive-type/float) 2)
     :vec3 (type/vec (primitive-type/float) 3)
     :vec4 (type/vec (primitive-type/float) 4)
     :dvec2 (type/vec (primitive-type/double) 2)
     :dvec3 (type/vec (primitive-type/double) 3)
     :dvec4 (type/vec (primitive-type/double) 4)
     :ivec2 (type/vec (primitive-type/int) 2)
     :ivec3 (type/vec (primitive-type/int) 3)
     :ivec4 (type/vec (primitive-type/int) 4)
     :uvec2 (type/vec (primitive-type/uint) 2)
     :uvec3 (type/vec (primitive-type/uint) 3)
     :uvec4 (type/vec (primitive-type/uint) 4)
     :bvec2 (type/vec (primitive-type/bool) 2)
     :bvec3 (type/vec (primitive-type/bool) 3)
     :bvec4 (type/vec (primitive-type/bool) 4)
     :mat2 (type/mat 2 2)
     :mat3 (type/mat 3 3)
     :mat4 (type/mat 4 4)
     :mat2x2 (type/mat 2 2)
     :mat2x3 (type/mat 2 3)
     :mat2x4 (type/mat 2 4)
     :mat3x2 (type/mat 3 2)
     :mat3x3 (type/mat 3 3)
     :mat3x4 (type/mat 3 4)
     :mat4x2 (type/mat 4 2)
     :mat4x3 (type/mat 4 3)
     :mat4x4 (type/mat 4 4)})

  (def type-registry (table/weak-keys 64))
  (defn coerce [x] (in type-registry x x))
  (defn register-constructor [constructor type] (put type-registry constructor type))

  (defn struct? [t]
    (and (type? t) (type/match t
      (struct _ _) true
      false)))

  (defn of-ast [ast]
    (if-let [t (in short-names ast)]
      ~',t
      (pat/match ast
        ['struct name & fields]
          [type/struct (string name)
            [ordered/table/new
              ;(catseq [[type name] :in (partition 2 fields)]
                [~',name (of-ast type)])]]
        (and |ptuple? [type count]) [type/array (of-ast type) count]
        |keyword? [type/primitive (primitive-type/of-ast ast)]
        [coerce ast])))

  (test (of-ast :float)
    [@type/primitive [quote [<1> float]]])
  (test (eval (of-ast :float))
    [<1> primitive [<2> float]])
  (test (eval (of-ast '(:float 3)))
    [<1>
     array
     [<1> primitive [<2> float]]
     3])

  (defn to-glsl [t]
    (type/match t
      (void) :void
      (primitive t) (primitive-type/to-glsl t)
      (struct name _) (symbol name)
      (mat col row) (if (= col row) (keyword "mat" col) (keyword "mat" col "x" row))
      (array type length) [(to-glsl type) length]
      (vec type count) (keyword (primitive-type/vec-prefix type) count)))

  (defn components [t]
    (type/match t
      (void) (error "vector cannot contain void")
      (primitive _) 1
      (vec _ count) count
      (mat cols rows) (* cols rows)
      (array _ _) (error "you can't construct vectors from arrays")
      (struct _ _) (error "vectors cannot contain compound entries")))

  (defn base-type [t]
    (type/match t
      (void) nil
      (primitive t) t
      (vec t _) t
      (mat _ _) (primitive-type/float)
      (array _ _) nil
      (struct _ _) nil))

  (defn- is-vector-field? [field]
    (or (string/check-set "xyzw" field)
        (string/check-set "rgba" field)
        (string/check-set "stpq" field)))

  (defn field-type [t field]
    (type/match t
      (void) (errorf "cannot access field %q of void" field)
      (primitive t) (errorf "cannot access field %q of primitive type" field)
      (mat _ _) (errorf "cannot access field %q of a matrix" field)
      (array _ _) (errorf "cannot access field %q of an array" field)
      (vec t count)
        (if (is-vector-field? field)
          (let [len (length field)]
            (cond
              (= len 1) (type/primitive t)
              (and (>= len 2) (<= len 4)) (type/vec t len)
              (errorf "cannot create a vector with %d components" len)))
          (errorf "unknown vector field %q" field))
      (struct name fields)
        (or (ordered/table/in fields field)
          (errorf "%s: unknown field %q" name field))))

  (defn element-type [t]
    (type/match t
      (void) (error "cannot index into void")
      (primitive t) (error "cannot index into primitive type")
      (mat _ rows) (type/vec (primitive-type/float) rows)
      (array type _) type
      (vec t _) (type/primitive t)
      (struct _ _) (error "cannot index into struct")))
  )

(def param-sig-type-id (gensym))
(defn param-sig? [t]
  (and (tuple? t) (= (length t) 3) (= (in t 0) param-sig-type-id)))
(defmodule param-sig
  (defn new [type access] [param-sig-type-id type access])
  (defn type [t] (in t 1))
  (defn access [t] (in t 2))
  (defn in [type] (new type :in))

  (defn to-glsl [t]
    (def type (type/to-glsl (type t)))
    (match (access t)
      :in type
      :out (tuple/brackets 'out type)
      :inout (tuple/brackets 'inout type)
      (error "BUG: unknown access type")))

  (defn of-ast [ast]
    (if (btuple? ast)
      (match ast
        ['in type] [new (type/of-ast type) :in]
        ['out type] [new (type/of-ast type) :out]
        ['inout type] [new (type/of-ast type) :inout]
        (errorf "unknown parameter signature %q" ast))
      [new (type/of-ast ast) :in]))
  )
