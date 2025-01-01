(use ./import)
(import ./expression-hoister)

(thunk ~(as-macro ,jlsl/jlsl/declare :float nearest-distance []
  ```(nearest-distance)

  This is the forward declaration of the function that will become the eventual
  distance field for the shape we're rendering. This is used in the main raymarcher,
  as well as the shadow calculations. You can refer to this function to sample the
  current distance field at the current value of `p` or `q`, for example to create
  a custom ambient occlusion value.
  ```))

(thunk ~(setdyn ,expression-hoister/*hoisted-vars* (table/weak-keys 8)))
