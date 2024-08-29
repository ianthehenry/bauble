(import ../syntax)

(import ../util :prefix "" :export true)

(defmacro sugar [expr] (syntax/expand expr))

(defmacro program/new [& body]
  ~(jlsl/program/new ,;(syntax/expand body)))
