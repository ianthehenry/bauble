(import jaylib)
(use module)
(import ./ray)
(import ../glsl)

(defdyn *verbose*)

(defn init-jaylib []
  (jaylib/set-trace-log-level :warning)
  (jaylib/set-config-flags :window-hidden)
  (jaylib/init-window 0 0 "Bauble"))

(defmodule shader
  (defn new [glsl-program]
    (def shader-source (glsl/render-program glsl-program "330"))
    (when (dyn *verbose*)
      (eprint "```glsl")
      (eprint shader-source)
      (eprint "```"))

    (def shader (jaylib/load-shader-from-memory nil shader-source))
    (def uniforms (glsl/uniforms glsl-program))
    (def uniform-locations (tabseq [name :keys uniforms]
      name (jaylib/get-shader-location shader name)))
    (def unset-uniforms (tabseq [name :keys uniforms] name true))
    (var last-resolution nil)
    (var fbo nil)

    (defn set-uniform-values [& new-values]
      (each [name value] (partition 2 new-values)
        (def name (glsl/identifier name))
        (match (in uniforms name)
          nil (errorf "unknown uniform %s" name)
          type (do
            (put unset-uniforms name nil)
            (jaylib/set-shader-value shader (in uniform-locations name) value type)))))

    (defn render [resolution & uniform-values]
      (when (not= resolution last-resolution)
        (set-uniform-values 'viewport [0 0 ;resolution])
        (when fbo (jaylib/unload-render-texture fbo))
        (set fbo (jaylib/load-render-texture ;resolution)))

      (set-uniform-values ;uniform-values)
      (unless (empty? unset-uniforms)
        (errorf "must set values for uniforms %s" (string/join (keys unset-uniforms) ", ")))

      (ray/do-texture fbo
        (ray/do-shader shader
          (jaylib/draw-rectangle-v [0 0] resolution :red)))
      (def image (jaylib/load-image-from-texture (jaylib/get-render-texture-texture2d fbo)))
      (jaylib/image-flip-vertical image)
      image)
    {:render render})

  (defn render [{:render render} & args]
    (render ;args)))

(defn- calry [f & args]
  (if f (f ;args)))

(defn render-image [shader-source &named
  dimensions
  animated?
  free-camera?
  resolution
  origin
  orbit
  zoom
  t
  slices
  on-render-start
  on-slice-start
  on-slice-end
  on-render-end]
  (default slices [1 1])
  (default zoom 1)
  (default origin (if (= dimensions 2) [0 0] [0 0 0]))
  (default orbit [0 0])
  (default t 0)
  (def frame-buffer (ray/make-fbo resolution :point))
  (def shader (jaylib/load-shader-from-memory nil shader-source))

  (defn set-uniform [name type value]
    (jaylib/set-shader-value shader (jaylib/get-shader-location shader name) value type))

  (when free-camera?
    (set-uniform "free_camera_zoom" :float zoom)
    (case dimensions
      2 (do
        (set-uniform "free_camera_origin" :vec2 origin))
      3 (do
        (set-uniform "free_camera_origin" :vec3 origin)
        (set-uniform "free_camera_orbit" :vec2 orbit))
      0 (do)
      (error "invalid dimension")))
  (when animated? (set-uniform "t" :float t))
  (set-uniform "viewport" :vec4 [0 0 ;resolution])

  (def slice-size (map |(math/floor (/ $0 $1)) resolution slices))
  (def residution (map - resolution (map |(* $0 $1) slice-size slices)))
  (calry on-render-start slice-size residution)
  (ray/do-shader shader (ray/do-texture frame-buffer
    (loop [y :range [0 (slices 1)]
           x :range [0 (slices 0)]]
      (def last-x? (= x (dec (slices 0))))
      (def last-y? (= y (dec (slices 1))))
      # the final slice in each direction may be larger to account
      # for any floored pixels
      (def residution
        [(if last-x? (residution 0) 0)
         (if last-y? (residution 1) 0)])
      (def this-size (map + residution slice-size))
      (calry on-slice-start x y)
      (jaylib/draw-rectangle-v (map * [x y] slice-size) this-size :red)
      (jaylib/draw-render-batch-active)
      (jaylib/finish)
      (unless (and last-x? last-y?)
        (os/sleep 0.01))
      (calry on-slice-end x y))))
  (calry on-render-end)
  (def image (jaylib/load-image-from-texture (jaylib/get-render-texture-texture2d frame-buffer)))
  (jaylib/image-flip-vertical image)
  image)
