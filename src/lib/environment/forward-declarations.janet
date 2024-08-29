(use ./import)
(defn make-forward-declarations [env]
  # TODO: wait this docstring is bad. i think there's a bug where
  # it can't parse things
  (eval ~(as-macro ,jlsl/jlsl/declare :float nearest-distance []
    ```(nearest-distance)

    This is the forward declaration of the function that will become the eventual
    distance field for the shape we're rendering. This is used in the main raymarcher,
    as well as the shadow calculations. You can refer to this function to sample the
    current distance field at the current value of `p` or `q`, for example to create
    a custom ambient occlusion value.
    ```) env))
