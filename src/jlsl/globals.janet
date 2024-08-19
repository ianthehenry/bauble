(use ./type)

# fragment shader implicit inputs:
(def gl-frag-coord (variable/new "gl_FragCoord" type/vec4))
(def gl-front-facing (variable/new "gl_FrontFacing" type/bool))
(def gl-point-coord (variable/new "gl_PointCoord" type/vec2))

# fragment shader implicit outputs:
(def gl-frag-depth (variable/new "gl_FragDepth" type/float))
