(import jaylib)

(def pixel-center [0.5 0.5])

(defn make-fbo [size texture-filter]
  (def fbo (jaylib/load-render-texture ;size))
  (jaylib/set-texture-filter (jaylib/get-render-texture-texture2d fbo) texture-filter)
  fbo)

(defmacro do-texture [texture & forms]
  ~(do
    (,jaylib/begin-texture-mode ,texture)
    ,;forms
    (,jaylib/end-texture-mode)))

(defmacro do-shader [shader & forms]
  ~(do
    (,jaylib/begin-shader-mode ,shader)
    ,;forms
    (,jaylib/end-shader-mode)))

(defmacro do-2d [camera & forms]
  ~(do
    (,jaylib/begin-mode-2d ,camera)
    ,;forms
    (,jaylib/end-mode-2d)))

(defmacro do-3d [camera & forms]
  ~(do
    (,jaylib/begin-mode-3d ,camera)
    ,;forms
    (,jaylib/end-mode-3d)))

(defmacro do-drawing [& forms]
  ~(do
    (,jaylib/begin-drawing)
    ,;forms
    (,jaylib/end-drawing)))

(defn save-screenshot [frame-buffer dest-file]
  (def image (jaylib/load-image-from-texture (jaylib/get-render-texture-texture2d frame-buffer)))
  (jaylib/image-flip-vertical image)
  (jaylib/export-image image dest-file))
