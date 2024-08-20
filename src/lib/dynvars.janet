(use ../jlsl)

# TODO: documentation

# available in distance or color fields:

(jlsl/defdyn t :float)

(jlsl/defdyn p :vec3)
(jlsl/defdyn P :vec3)
(jlsl/defdyn q :vec2)
(jlsl/defdyn Q :vec2)

# only available in color fields:

(jlsl/defdyn normal :vec3)
(jlsl/defdyn gradient :vec2)
(jlsl/defdyn d :float)
