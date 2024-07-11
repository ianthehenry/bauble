# `jlsl`

An s-expression syntax for GLSL. Still todo:

- [x] `switch` statements
- [x] `while` loops
- [x] do-while loops
- [x] `for` loops
- [x] `+=` and friends
- [x] `++` and `_++`, `--` and `_--`
- [ ] conditional expressions
- [ ] array indexing
- [ ] struct declarations
- [ ] `.` access on arbitrary expressions
- [ ] `out` and `inout` qualifiers on function parameters

Because integers and floats are syntactically distinct in (some versions of) GLSL, jlsl always renders numbers as floats. So the Janet s-expression `64` will become the GLSL string `64.0`. If you want to include integer literals, you have to write them as keywords: the Janet s-expression `:64` will render as the GLSL integer literal `64`.

Generally operators use their Janet names, e.g. `(and x y)` becomes `x && y`. This holds true for bitwise operators, e.g. `(bxor= x y)` becomes `x ^= y` and `blshift x y` becomes `(<< x y)`.
