# `jlsl`

An s-expression syntax for GLSL.

Think of this library as a better version of string interpolation. It doesn't understand any of GLSL's semantics; there is no typechecking, and it's perfectly possible to generate GLSL code that won't actually compile.

# notes

- `kebab-case` identifiers will be converted to `snake_case`
- integer literals like `64` are written `:64`. The Janet number `64` will become the float literal `64.0`.
- operators use their Janet names, e.g. `(and x y)` becomes `x && y`, `(not= x y)` becomes `x != y`, `(bxor= x y)` becomes `x ^= y`, `(blshift x y)` becomes `x << y`, etc.

# known deficiencies

These features are not supported (yet?):

- `out` and `inout` qualifiers on function parameters
- interface blocks
- forward declarations of functions

# unknown deficiencies

- lol
