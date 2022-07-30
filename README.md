# bauble

Bauble is a toy for composing signed distance functions in a high-level language ([Janet](https://janet-lang.org/)), compiling them to [GLSL](https://www.khronos.org/opengl/wiki/OpenGL_Shading_Language), and rendering them via WebGL.

It is not deployed anywhere, but there is [a short early demo here](https://twitter.com/ianthehenry/status/1551422839307190272).

# Dependencies

Requires [`emscripten`](https://emscripten.org/). Janet is vendored, so it does not actually require that you have Janet installed right now.

# Building

Run `./build` to compile Janet to wasm. For something like a production build you'll want to change that to an optimized build.

Serve files from the `public/` directory however is most convenient for you. I like:

```
$ cd public
$ python3 -m http.server
```
