(use ../../jlsl)

# TODO: docstrings for these

(jlsl/defstruct Ray
  :vec3 origin
  :vec3 dir)

(jlsl/defstruct Light
  :vec3 color
  :vec3 direction
  :float brightness)
