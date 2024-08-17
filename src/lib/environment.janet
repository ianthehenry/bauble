(defmacro view
  "A shorthand for `(set subject _)` that fits nicely into pipe notation, e.g. `(sphere 50 | view)`."
  [subject]
  ~(set subject ,subject))

(var subject
  "A variable that determines what Bauble will render.\n\nYou can set this variable explicitly to change your focus, or use the `view` macro to change your focus. If you don't set a subject, Bauble will render the last expression in your script that it knows how to render."
  nil)

(use ../jlsl)
(use ./dynvars)
(import ../jlsl/prelude :prefix "" :export true)

(jlsl/defn :float circle [:float r]
  (- (length q) r))

(defn circle [r]
  {:distance (- (length q) r)})

(defn color [fields color-expression]
  (struct/with-proto fields :color (coerce-expr color-expression)))
