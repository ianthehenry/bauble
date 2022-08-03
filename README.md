# bauble

Bauble is a toy for composing signed distance functions in a high-level language ([Janet](https://janet-lang.org/)), compiling them to [GLSL](https://www.khronos.org/opengl/wiki/OpenGL_Shading_Language), and rendering them via WebGL.

It is not deployed anywhere, but there is [a short early demo here](https://twitter.com/ianthehenry/status/1551422839307190272).

# Dependencies

Requires [`emscripten`](https://emscripten.org/). Janet is vendored, so it does not actually require that you have Janet installed right now.

Also requires [`yarn`](https://yarnpkg.com/), to install JavaScript dependencies.

This is horrible, but because [`codemirror-lang-janet`](https://github.com/ianthehenry/codemirror-lang-janet) is not actually installed anywhere yet, you'll have to edit `package.json` to remove it as a dependency before you run `yarn`. Run `yarn` to install everything else, then manually install `codemirror-lang-janet` by cloning the repo and running [`yalc publish`](https://github.com/wclr/yalc) from the `codemirror-lang-janet` repository. Then run `yalc link codemirror-lang-janet` from this repository, and you'll be in business.

# Building

Run `./build` to compile Janet to wasm; run `./build --prod` for an optimized, minified build that will take much longer. Run `./build --js` to only rebuild the UI or `./build --wasm` to only rebuild the Janet/C components.

# Running

Serve files from the `public/` directory however is most convenient for you. For example:

```
$ cd public
$ python3 -m http.server
```
