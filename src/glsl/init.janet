(use judge)
(use module)
(import pat)
(use ./util)

(defmodule- printer
  (defn new [&opt capacity]
    @{:indent 0
      :indented false
      :buf (buffer/new (or capacity 65535))})

  (defn indent [t]
    (repeat (t :indent)
      (buffer/push-string (t :buf) "  "))
    (put t :indented true))

  (defn newline [t]
    (buffer/push-byte (t :buf) (chr "\n"))
    (put t :indented false))

  (defn prin [t & things]
    (unless (t :indented) (indent t))
    (each thing things
      (buffer/push-string (t :buf) thing)))

  (defn print [t & things]
    (prin t ;things)
    (newline t))

  (defn to-string [t]
    (string (t :buf)))

  (defn indent-body* [t f]
    (++ (t :indent))
    (f)
    (-- (t :indent)))

  (defmacro indent-body [t & body]
    ~(,indent-body* ,t (fn [] ,;body)))

  (defmacro bracket-body [t start end & body]
    ~(do
      (,print ,t ,start)
      (,indent-body* ,t (fn [] ,;body))
      (,prin ,t ,end))))

(deftest "printer works"
  (def p (printer/new))
  (printer/print p "hello")
  (printer/print p "start " "{")
  (printer/indent-body p
    (printer/print p "one")
    (printer/print p "two"))
  (printer/print p "}")
  (test-stdout (print (printer/to-string p)) `
    hello
    start {
      one
      two
    }
    
  `))

(deftest "bracketing works"
  (def p (printer/new))
  (printer/print p "hello")
  (printer/prin p "start")
  (printer/bracket-body p " {" "} "
    (printer/print p "one")
    (printer/print p "two"))
  (printer/print p "hello")
  (test-stdout (print (printer/to-string p)) `
    hello
    start {
      one
      two
    } hello
    
  `))

# okay this is terrible but whatever
(defn identifier [name]
  (->> name
    (peg/replace-all ~(* (column) (set "+*$/") (+ (/ (look 0 -1) true) (constant false))) (fn [char col eos]
      (def bos (= col 1))
      (if (and bos eos)
        char
        (string (if-not bos "_") ('{"+" "plus" "*" "star" "$" "dollar" "/" "slash"} char) (if-not eos "_")))))
    (string/replace-all "-" "_")
    ))

(test (identifier "foo-bar") "foo_bar")
(test (identifier "foo+") "foo_plus")
(test (identifier "+foo") "plus_foo")
(test (identifier "foo*+") "foo_star__plus")
(test (identifier "capsule/2d") "capsule_slash_2d")
(test (identifier "$i") "dollar_i")
(test (identifier "+") "+")

(var render-statement nil)
(var render-expression nil)

(defn- to-float [num]
  (def str (string num))
  (var looks-integral true)
  (each c str
    (when (= c (chr ".")) (set looks-integral false) (break))
    (when (= c (chr "e")) (set looks-integral false) (break)))
  (if looks-integral
    (string str ".0")
    str))

(varfn render-expression [p expression &named needs-parens?]
  (default needs-parens? false)
  (pat/match expression
    |number? (printer/prin p (to-float expression))
    |symbol? (printer/prin p (identifier expression))
    |int64? (printer/prin p (string expression))
    |uint64? (printer/prin p (string expression "u"))
    |boolean? (printer/prin p (string expression))
    ['. expr key] (do
      (render-expression p expr :needs-parens? true)
      (printer/prin p "." (identifier key)))
    ['- expr]
      (wrap-when needs-parens?
        (printer/prin p "(")
        (if (number? expr)
          # special case to avoid rendering --1
          (render-expression p (- expr) :needs-parens? true)
          (do
            (printer/prin p "-")
            (render-expression p expr :needs-parens? true)))
        (printer/prin p ")"))
    ['/ expr] (render-expression p ~(/ 1 ,expr) :needs-parens? needs-parens?)
    [(and op (or '+ '/ '- '*
      '< '> '<= '>=
      '= 'not=
      'and 'or
      '% 'xor
      'blshift 'brshift 'band 'bor 'bxor)) & args] (do
      (def op (case op
        'blshift "<<"
        'brshift ">>"
        'band "&"
        'bor "|"
        'bxor "^"
        'and "&&"
        'or "||"
        'xor "^^"
        'not= "!="
        '= "=="
        op))
      (wrap-when needs-parens?
        (printer/prin p "(")
        (each-last arg args
          (render-expression p arg :needs-parens? true)
          (printer/prin p " " op " "))
        (printer/prin p ")")))
    [(and op (or '++ '--)) expr] (do
      (printer/prin p op)
      (render-expression p expr :needs-parens? true))
    [(and op (or '_++ '_--)) expr] (do
      (render-expression p expr :needs-parens? true)
      (printer/prin p (string/slice op 1)))
    [(and op (or 'not 'bnot)) expr] (do
      (def op (case op
        'not "!"
        'bnot "~"
        (error "BUG")))
      (printer/prin p op)
      (render-expression p expr :needs-parens? true))
    ['if cond then else] (do
      (wrap-when needs-parens?
        (printer/prin p "(")
        (render-expression p cond :needs-parens? true)
        (printer/prin p " ? ")
        (render-expression p then :needs-parens? true)
        (printer/prin p " : ")
        (render-expression p else :needs-parens? true)
        (printer/prin p ")")))
    ['in expr key] (do
      (render-expression p expr)
      (printer/prin p "[")
      (render-expression p key)
      (printer/prin p "]"))
    ['.length expr] (do
      (render-expression p expr :needs-parens? true)
      (printer/prin p ".length()"))
    [f & args] (do
      (printer/prin p (identifier f) "(")
      (each-last arg args
        (render-expression p arg)
        (printer/prin p ", "))
      (printer/prin p ")"))
    ))

(defn format-type [type]
  (if (and (ptuple? type) (= (length type) 2))
    (string/format "%s[%d]" (format-type (in type 0)) (in type 1))
    type))

(varfn render-statement [p statement &named newline-after-block? one-line? semicolon?]
  (default newline-after-block? true)
  (default semicolon? true)
  (default one-line? false)
  (var semicolon? semicolon?)
  (pat/match statement
    ['do & statements] (do
      (printer/bracket-body p "{" "}"
        (each statement statements
          (render-statement p statement)))
      (if newline-after-block?
        (printer/newline p)
        (printer/prin p " "))
      (set semicolon? false))
    ['var type name] (do
      (printer/prin p (format-type type) " " (identifier name)))
    ['var type name value] (do
      (printer/prin p (format-type type) " " (identifier name) " = ")
      (render-expression p value))
    ['set dest value] (do
      (render-expression p dest)
      (printer/prin p " = ")
      (render-expression p value))
    ['return value] (do
      (printer/prin p "return ")
      (render-expression p value))
    ['break] (printer/prin p "break")
    ['if cond then & else] (do
      (assert (<= (length else) 1) "too many arguments to if")
      (printer/prin p "if (")
      (render-expression p cond)
      (printer/prin p ") ")
      (render-statement p then :newline-after-block? (empty? else))
      (unless (empty? else)
        (printer/prin p "else ")
        (render-statement p (first else)))
      (set semicolon? false))
    ['for init check advance & body] (do
      (printer/prin p "for (")
      (render-statement p init :one-line? true)
      (printer/prin p " ")
      (render-expression p check)
      (printer/prin p "; ")
      (render-statement p advance :semicolon? false)
      (printer/prin p ") ")
      (render-statement p ~(do ,;body))
      (set semicolon? false))
    ['case value & cases] (do
      (printer/prin p "switch (")
      (render-expression p value)
      (printer/print p ") {")
      (each case (partition 2 cases)
        (def body (match (length case)
          1 (do (printer/prin p "default: ") (first case))
          2 (let [[value body] case]
            (printer/prin p "case ")
            (render-expression p value)
            (printer/prin p ": ")
            body)
          (error "impossible")))
        (render-statement p body))
      (printer/print p "}")
      (set semicolon? false))
    [(and op (or
      '+= '*= '/= '-= '%=
      'blshift= 'brshift=
      'bxor= 'band= 'bor=)) dest expr] (do
      (def op (case op
        'bxor= "^="
        'band= "&="
        'bor= "|="
        'brshift= ">>="
        'blshift= "<<="
        op))
      (render-expression p dest)
      (printer/prin p " " op " ")
      (render-expression p expr))
    ['continue] (printer/prin p "continue")
    ['discard] (printer/prin p "discard")
    ['def type name value] (do
      (printer/prin p "const " (format-type type) " " (identifier name) " = ")
      (render-expression p value))
    ['upscope & statements] (do
      (each statement statements
        (render-statement p statement))
      (set semicolon? false))
    ['while cond & body] (do
      (printer/prin p "while (")
      (render-expression p cond)
      (printer/prin p ") ")
      (render-statement p ~(do ,;body))
      (set semicolon? false))
    ['do-while cond & body] (do
      (printer/prin p "do ")
      (render-statement p ~(do ,;body) :newline-after-block? false)
      (printer/prin p "while (")
      (render-expression p cond)
      (printer/prin p ")"))
    (render-expression p statement))
  (when semicolon?
    ((if one-line? printer/prin printer/print) p ";")))

(defn- format-params [params]
  (->
    (seq [[sig arg] :in (partition 2 params)]
      (string/join [;(if (and (btuple? sig) (= (length sig) 2))
        [(in sig 0) (format-type (sig 1))] [(format-type sig)]) (identifier arg)] " "))
    (string/join ", ")))

(test (format-params '[:foo name :bar other]) "foo name, bar other")
(test (format-params '[(:foo 10) name :bar other]) "foo[10] name, bar other")
(test (format-params '[((:foo 10) 10) name :bar other]) "foo[10][10] name, bar other")
(test (format-params '[[out (:foo 10)] name :bar other]) "out foo[10] name, bar other")

(defn render-toplevel [p toplevel]
  (pat/match toplevel
    ['struct name & fields] (do
      (printer/prin p "struct " (identifier name))
      (printer/bracket-body p " {" "};"
        (each field (partition 2 fields)
          (render-statement p ~(var ,;field))))
      (printer/newline p))
    ['defn type name params & body] (do
      (printer/prin p type " " (identifier name) "(" (format-params params) ")")
      (if (empty? body)
        (printer/print p ";")
        (do
          (printer/bracket-body p " {" "}"
            (each statement body
              (render-statement p statement)))
          (printer/newline p))))
    ['def type name value] (do
      (printer/prin p "const " type " " (identifier name) " = ")
      (render-expression p value)
      (printer/print p ";"))
    [(and (or 'uniform 'out 'in) decl) type name] (printer/print p decl " " type " " (identifier name) ";")
    ['precision & rest] (printer/print p "precision " ;(intercalate rest " ") ";")))

(defn render-program [program &opt version]
  (def p (printer/new))
  (when version
    (printer/print p "#version " version))
  (var last-head nil)
  (each toplevel program
    (var new-head (first toplevel))
    # group forward declarations together
    (when (and (= new-head 'defn) (= (length toplevel) 4))
      (set new-head :forward-declaration))

    (when (and last-head
        (or (= new-head 'defn)
        (not= last-head new-head)))
      (printer/newline p))

    (render-toplevel p toplevel)

    (set last-head new-head))
  (printer/to-string p))

(defn uniforms [program]
  (def uniforms @{})
  (each toplevel program
    (match toplevel
      ['uniform type name] (put uniforms (identifier name) type)))
  uniforms)

(defmacro*- test-program [program & args]
  ~(test-stdout (print (,render-program ',program)) ,;args))

(defmacro*- test-program* [program & args]
  ~(test-stdout (print (,render-program ,program)) ,;args))

(defmacro*- test-statements [statements & args]
  ~(test-program [(defn :void main [] ,;statements)] ,;args))

(deftest "post- and pre-crements"
  (test-statements [
    (++ i)
    (_++ i)
    (-- i)
    (_-- i)
    ] `
    void main() {
      ++i;
      i++;
      --i;
      i--;
    }
    
  `))

(deftest "out parameters"
  (test-program [
    (defn :void hello [[out :float] foo]
      (set foo 10))
    ] `
    void hello(out float foo) {
      foo = 10.0;
    }
    
  `))

(deftest "dot syntax"
  (test-statements [
    (. foo xyy)
    (. (+ (vec2 1 2) (vec2 3 4)) xyy)
    ] `
    void main() {
      foo.xyy;
      (vec2(1.0, 2.0) + vec2(3.0, 4.0)).xyy;
    }
    
  `))

(deftest "ternary conditional expressions"
  (test-statements [
    (def :float x (if (> y 10) 10 20))
    (def :float x (if (> y 10) (+ 5 5) (+ 10 10)))
    (def :float x (if (> y 10) (if (> y 20) 1 2) 3))
    (def :float x (if (> y 10) 1 (if (> y 20) 2 3)))
    (def :float x (if (if (> y 10) false true) (if (> y 20) 1 2) 3))
    ] `
    void main() {
      const float x = (y > 10.0) ? 10.0 : 20.0;
      const float x = (y > 10.0) ? (5.0 + 5.0) : (10.0 + 10.0);
      const float x = (y > 10.0) ? ((y > 20.0) ? 1.0 : 2.0) : 3.0;
      const float x = (y > 10.0) ? 1.0 : ((y > 20.0) ? 2.0 : 3.0);
      const float x = ((y > 10.0) ? false : true) ? ((y > 20.0) ? 1.0 : 2.0) : 3.0;
    }
    
  `))

(deftest "array access"
  (test-statements [
    (def :float x (in lights 0:u))
    (def :float x (in lights (+ i 1:s)))
    ] `
    void main() {
      const float x = lights[0u];
      const float x = lights[i + 1];
    }
    
  `))

(deftest "length method"
  (test-statements [
    (def :int x (.length lights))
    ] `
    void main() {
      const int x = lights.length();
    }
    
  `))

(deftest "field access"
  (test-statements [
    (def :float x (. foo xyz))
    (def :float x (. (f) xyz))
    (def :float x (. (. (in lights 0:u) incidence) brightness))
    ] `
    void main() {
      const float x = foo.xyz;
      const float x = f().xyz;
      const float x = lights[0u].incidence.brightness;
    }
    
  `))

(deftest "unary operators"
  (test-statements [
    (def :float x (- x))
    (def :float x (- 1))
    (def :float x (- -1))
    (def :float x (- (- 1)))

    (def :float x (- 0))
    (def :float x (- -0))

    (def :float x (not x))
    (def :float x (not 1))
    (def :float x (not -1))
    (def :float x (not (not 1)))
    ] `
    void main() {
      const float x = -x;
      const float x = -1.0;
      const float x = 1.0;
      const float x = -(-1.0);
      const float x = 0.0;
      const float x = 0.0;
      const float x = !x;
      const float x = !1.0;
      const float x = !-1.0;
      const float x = !!1.0;
    }
    
  `))

(deftest "loops"
  (test-statements [
    (while (> i 10) (-- i))
    (do-while (> i 10) (-- i))
    (for (var :int i 0:s) (< i 10:s) (++ i) (f))
    ] `
    void main() {
      while (i > 10.0) {
        --i;
      }
      do {
        --i;
      } while (i > 10.0);
      for (int i = 0; i < 10; ++i) {
        f();
      }
    }
    
  `))

(deftest "struct declaration"
  (test-program [
    (struct LightIncidence
      :vec3 direction
      :vec3 color)
    ] `
    struct LightIncidence {
      vec3 direction;
      vec3 color;
    };
    
  `))

(deftest "if formats correctly with and without else"
  (test-statements [
    (if x (set y 10))
    (set x 10)
    (if x (do (set y 10)))
    (set x 10)
    (if x (do (set y 10)) (set y 20))
    (set x 10)
    (if x (do (set y 10)) (do (set y 20)))
    (set x 10)
    ] `
    void main() {
      if (x) y = 10.0;
      x = 10.0;
      if (x) {
        y = 10.0;
      }
      x = 10.0;
      if (x) {
        y = 10.0;
      } else y = 20.0;
      x = 10.0;
      if (x) {
        y = 10.0;
      } else {
        y = 20.0;
      }
      x = 10.0;
    }
    
  `))

(deftest "switch indentation"
  (test-statements [
    (case x
      10 (do (f) (continue) (g))
      20 (h)
      30 (do (f) (g) (break))
      (do (h)))
    ] `
    void main() {
      switch (x) {
      case 10.0: {
        f();
        continue;
        g();
      }
      case 20.0: h();
      case 30.0: {
        f();
        g();
        break;
      }
      default: {
        h();
      }
      }
    }
    
  `))

(deftest "switch fallthrough"
  (test-statements [
    (case x
      0:s (do)
      1:s (upscope)
      2:s (break)
      (do (h)))
    ] `
    void main() {
      switch (x) {
      case 0: {
      }
      case 1: case 2: break;
      default: {
        h();
      }
      }
    }
    
  `))

(deftest "assignment to arbitrary expression"
  (test-statements [
    (set (. foo x) 10)
    (set (in foo 0:u) 10)
    (+= (in foo 0:u) 10)
    (*= (f foo) 10)
    ] `
    void main() {
      foo.x = 10.0;
      foo[0u] = 10.0;
      foo[0u] += 10.0;
      f(foo) *= 10.0;
    }
    
  `))

(deftest "do vs upscope"
  (test-statements [
    (do (var :float x 10))
    (upscope (var :float x 10))
    ] `
    void main() {
      {
        float x = 10.0;
      }
      float x = 10.0;
    }
    
  `))

(deftest "scientific notation"
  (test-statements [
    (return 0.0001)
    (return 0.00001)
    (return 0.0000123)
    (return 0.000001)
    (return 1e15)
    (return 1e16)
    ] `
    void main() {
      return 0.0001;
      return 1e-05;
      return 1.23e-05;
      return 1e-06;
      return 1000000000000000.0;
      return 1e+16;
    }
    
  `))

(deftest "if formatting"
  (test-statements [
    (if (< x 10) (return 1))
    (if (< x 10) (return 1) (return 2))
    (if (< x 10) (do (return 1)) (return 2))
    (if (< x 10) (do (return 1)) (do (return 2)))
    (if (< x 10) (return 1) (do (return 2)))
    ] `
    void main() {
      if (x < 10.0) return 1.0;
      if (x < 10.0) return 1.0;
      else return 2.0;
      if (x < 10.0) {
        return 1.0;
      } else return 2.0;
      if (x < 10.0) {
        return 1.0;
      } else {
        return 2.0;
      }
      if (x < 10.0) return 1.0;
      else {
        return 2.0;
      }
    }
    
  `))

(deftest "forward function declarations"
  (test-program [
    (defn :float foo [:float x])
    (defn :float bar [:float x])
    (defn :float foo [:float x]
      (return (+ (bar x) 1)))
    (defn :float bar [:float x]
      (return (+ (foo x) 1)))
    ] `
    float foo(float x);
    float bar(float x);
    
    float foo(float x) {
      return bar(x) + 1.0;
    }
    
    float bar(float x) {
      return foo(x) + 1.0;
    }
    
  `))

(def example-program ~[
  (precision highp float)
  (uniform :vec3 camera-origin)
  (uniform :vec3 camera-rotation)
  (uniform float t)
  (uniform vec4 viewport)
  (out vec4 frag_color)

  (def :int MAX_STEPS 64:s)
  (def :float MINIMUM_HIT_DISTANCE 0.1)
  (def :float NORMAL_OFFSET 0.005)
  (def :float MAXIMUM_TRACE_DISTANCE (* 64 1024))

  (def :float PI 3.14159265359)

  (defn :mat3 rotate-x [:float angle]
    (def :float s (sin angle))
    (def :float c (cos angle))
    (return (mat3
      1 0 0
      0 c (- s)
      0 s c)))

  (defn :float s3d-ellipsoid [:vec3 p :vec3 size]
    (def :float k0 (length (/ p size)))
    (def :float k1 (length (/ p (* size size))))
    (return (/ (* k0 (- k0 1)) k1)))

  (defn :void main []
    (def :float gamma 2.2)

    (def :vec2 local_coord (- gl_FragCoord.xy viewport.xy))
    (def :vec2 resolution viewport.zw)
    (def :vec3 dir (*
      (rotate_x camera_rotation.x)
      (rotate_y camera_rotation.y)
      (rotate_z camera_rotation.z)
      (perspective 27.0 resolution local_coord)))
    (def :vec3 hit (march camera-origin dir))
    (var :vec3 color)
    (def :float depth (distance camera-origin hit))
    (var :float alpha 1)
    (if (>= depth MAXIMUM_TRACE_DISTANCE) (do
      (def :vec3 light (pow (/ (vec3 69 72 79) (vec3 255)) (vec3 gamma)))
      (def :vec3 dark (pow (/ (vec3 40 42 46) (vec3 255)) (vec3 gamma)))
      (set color (vec3 (mix dark light (/ (+ local_coord.x local_coord.y) (+ resolution.x resolution.y))))))
      (do
        (set color (nearest-color hit))))

    (if (or (>= depth MAXIMUM_TRACE_DISTANCE) (< (+ hit.x hit.z) 10.0)) (do
      (def :float r (length (- (/ local-coord resolution) (vec2 0.5))))
      (if (> r 0.45) (do
        (set color (vec3 0.6 0.8 0.6))
        (set alpha 1)))
      (if (> r 0.47) (do
        (set alpha 0)))))
    (set frag-color (vec4 (pow color (vec3 (/ gamma))) alpha)))
  ])

(test (uniforms example-program)
  @{"camera_origin" :vec3
    "camera_rotation" :vec3
    "t" float
    "viewport" vec4})

(test-program* example-program `
  precision highp float;
  
  uniform vec3 camera_origin;
  uniform vec3 camera_rotation;
  uniform float t;
  uniform vec4 viewport;
  
  out vec4 frag_color;
  
  const int MAX_STEPS = 64;
  const float MINIMUM_HIT_DISTANCE = 0.1;
  const float NORMAL_OFFSET = 0.005;
  const float MAXIMUM_TRACE_DISTANCE = 64.0 * 1024.0;
  const float PI = 3.14159265359;
  
  mat3 rotate_x(float angle) {
    const float s = sin(angle);
    const float c = cos(angle);
    return mat3(1.0, 0.0, 0.0, 0.0, c, -s, 0.0, s, c);
  }
  
  float s3d_ellipsoid(vec3 p, vec3 size) {
    const float k0 = length(p / size);
    const float k1 = length(p / (size * size));
    return (k0 * (k0 - 1.0)) / k1;
  }
  
  void main() {
    const float gamma = 2.2;
    const vec2 local_coord = gl_FragCoord.xy - viewport.xy;
    const vec2 resolution = viewport.zw;
    const vec3 dir = rotate_x(camera_rotation.x) * rotate_y(camera_rotation.y) * rotate_z(camera_rotation.z) * perspective(27.0, resolution, local_coord);
    const vec3 hit = march(camera_origin, dir);
    vec3 color;
    const float depth = distance(camera_origin, hit);
    float alpha = 1.0;
    if (depth >= MAXIMUM_TRACE_DISTANCE) {
      const vec3 light = pow(vec3(69.0, 72.0, 79.0) / vec3(255.0), vec3(gamma));
      const vec3 dark = pow(vec3(40.0, 42.0, 46.0) / vec3(255.0), vec3(gamma));
      color = vec3(mix(dark, light, (local_coord.x + local_coord.y) / (resolution.x + resolution.y)));
    } else {
      color = nearest_color(hit);
    }
    if ((depth >= MAXIMUM_TRACE_DISTANCE) || ((hit.x + hit.z) < 10.0)) {
      const float r = length((local_coord / resolution) - vec2(0.5));
      if (r > 0.45) {
        color = vec3(0.6, 0.8, 0.6);
        alpha = 1.0;
      }
      if (r > 0.47) {
        alpha = 0.0;
      }
    }
    frag_color = vec4(pow(color, vec3(1.0 / gamma)), alpha);
  }
  
`)
