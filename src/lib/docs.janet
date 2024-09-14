# this is a program that, when executed, will print a markdown representation
# of a docs page. just... it's fine; don't worry about it. i know it's a rube
# goldberg machine

(use judge)
(use ./util)
(import ./environment/derive)

(def <name |(< (string/ascii-lower ($0 :name)) (string/ascii-lower ($1 :name))))

(defn category-order [category]
  (case category
    :shapes 0
      :3d 0
      :2d 1
      :combinators 2
      :functions 3
      :shape 4
    :transforms 1
    :dynvars 2
    :variables 2.5
    :surfacing 3
    :repeat 4
    :noise 5
    #:colors 0
    #:camera 0
    #:rotation 0
    #:helpers 0
    #:unknown 0
    :gl 99
    :misc 200
    100
    #(errorf "unknown category %q" category)
    ))

(def <priority (fn [a b]
  (def pa (category-order a))
  (def pb (category-order b))
  (cond
    (< pa pb) true
    (> pa pb) false
    (< a b))))

(defn category-name [category]
  (case category
    :shape "Lower-level shape stuff"
    :shapes "Shapes"
    :2d "2D shapes"
    :3d "3D shapes"
    :colors "Colors"
    :camera "Camera"
    :combinators "Shape combinators"
    :gl "GLSL helpers"
    :repeat "Repetition"
    :noise "Noise"
    :transforms "Transformations"
    :variables "Bauble variables"
    :dynvars "Dynamic variables"
    :surfacing "Shading"
    :rotation "Rotation"
    :functions "Shape functions"
    :misc "Uncategorized"
    :unknown "TODO"
    (errorf "unnamed category %q" category)
    ))

(defn categorize [name filename]
  (or
    ('{map-distance [:shapes :functions]
       map-color [:shapes :functions]
       shape/2d [:shapes :functions]
       shape/3d [:shapes :functions]} name)
    (cond
      (= filename "lib/shape.janet") [:shapes :shape]
      (nil? filename) [:misc]
      (string/has-prefix? "lib/environment/" filename)
        (let [basename (slice filename (length "lib/environment/") (- (length filename) (length ".janet")))]
          (case basename
            "boolean" [:shapes :combinators]
            "dimensions" [:shapes :combinators]
            "operator-overloads" [:misc]
            "forward-declarations" [:misc]
            "rotate" [:transforms :rotate]
            "prelude" [:misc]
            "general" [:misc]
            "shapes-2d" [:shapes :2d]
            "shapes-3d" [:shapes :3d]
            [(keyword basename)]))
      [:misc])))

# [name usage tag]
# where tag=0 for value, tag=1 for functions, tag=2 for macros
(defn gather-definitions []
  (seq [[name binding] :pairs (proto-flatten-to-root (derive/new))
        :when (symbol? name)
        :let [{:value value :ref ref :doc docstring :macro macro :source-map source-map} binding]
        :let [value (if ref (ref 0) value)]
        :when (not (nil? docstring))]

    (def [signature docstring]
      (if (function? value)
        (let [[signature & paragraphs] (string/split "\n\n" docstring)]
          [signature (string/join paragraphs "\n\n")])
        [nil docstring]))

    {:name name
     :value value
     :signature signature
     :docstring docstring
     :source-map source-map
     :category (categorize name (get-in source-map [0]))
     }))

#(def commit-hash (do
#  (def proc (os/spawn ["git" "rev-parse" "HEAD"] :p {:out :pipe}))
#  (def output @"")
#  (while (ev/read (in proc :out) 128 output))
#  (os/proc-wait proc)
#  # we don't need the full hash
#  ))

(defn print-definitions [depth definitions commit-hash]
  (each {:name name
         :signature signature
         :docstring docstring
         :source-map source-map}
      (sorted definitions <name)
    (def source-link (if-let [[file line col] source-map]
      (string/format `https://github.com/ianthehenry/bauble/blob/%s/src/%s#L%d`
        commit-hash file line)))
    (printf ```<h%d class="help-entry" id="%s">`%s`%s</h2>``` (+ depth 2) name (or signature name)
      (if source-link (string/format ` <a href="%s" class="source-link">source</a>` source-link) ""))
    (print)

    (print docstring)
    (print)))

(defn index-by [list key]
  (def nested @{})
  (eachp [path values] (group-by |(in $ key) list)
    (put-in nested path values))
  nested)

(defn walk-indexed [t f-begin f-each f-end &opt path]
  (default path [])
  (each key (sorted (keys t) <priority)
    (def path [;path key])
    (f-begin path)
    (def value (in t key))
    (if (table? value)
      (walk-indexed value f-begin f-each f-end path)
      (f-each path value))
    (f-end path)))

(defn main [_ commit-hash &]
  (def commit-hash (string/slice commit-hash 0 16))
  (def definitions (index-by (gather-definitions) :category))

  (print "<ul>")
  (walk-indexed definitions
    (fn [path] (printf "<li>%s<ul>" (category-name (last path))))
    (fn [_ definitions]
      (each {:name name :signature signature} (sorted definitions <name)
        (printf ``` <a href="#%s">`%s`</a>``` name name))
      )
    (fn [_] (print "</ul></li>")))
  (print "</ul>")

  (var indentation 0)
  (walk-indexed definitions
    (fn [path] (print (string/repeat "#" (length path)) " " (category-name (last path))) (print))
    (fn [path definitions] (print-definitions (length path) definitions commit-hash))
    (fn [_] (print))
    ))

(deftest "missing sourcemaps"
  (test (seq [definition :in (gather-definitions) :unless (has-key? definition :source-map)]
    (definition :name))
    @[@in @length]))

(deftest "categories"
  (var indent 0)
  (test-stdout (walk-indexed (index-by (gather-definitions) :category)
    (fn [path] (prin (string/repeat "  " indent)) (print (category-name (last path))) (++ indent))
    (fn [_ values] (prin (string/repeat "  " indent)) (printf "%d values" (length values)))
    (fn [_] (-- indent))) `
    Shapes
      3D shapes
        8 values
      2D shapes
        22 values
      Shape combinators
        7 values
      Shape functions
        4 values
      Lower-level shape stuff
        14 values
    Transformations
      10 values
    Dynamic variables
      15 values
    Bauble variables
      7 values
    Shading
      23 values
    Repetition
      3 values
    Noise
      8 values
    GLSL helpers
      9 values
    Camera
      2 values
    Rotation
      7 values
    Uncategorized
      35 values
  `))

