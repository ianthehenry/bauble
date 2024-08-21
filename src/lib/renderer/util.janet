(import ../syntax)

(defmacro sugar [expr] (syntax/expand expr))

(defmacro program/new [& body]
  ~(jlsl/program/new ,;(syntax/expand body)))
