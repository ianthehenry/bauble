# [bauble](https://bauble.studio)

Bauble is a toy for composing signed distance functions in a high-level language ([Janet](https://janet-lang.org/)), compiling them to [GLSL](https://www.khronos.org/opengl/wiki/OpenGL_Shading_Language), and rendering them via WebGL.

You can see a live demo running at <https://bauble.studio>, and I sometimes tweet videos of it:

- [modeling a face](https://twitter.com/ianthehenry/status/1551422839307190272)
- [morphing shapes](https://twitter.com/ianthehenry/status/1554729639183937536)
- [cutaway effect](https://twitter.com/ianthehenry/status/1554730689374720000)
- [infinite tiling](https://twitter.com/ianthehenry/status/1555788963905880064)

# Dependencies

Requires [`emscripten`](https://emscripten.org/). Janet is vendored, so it does not actually require that you have Janet installed right now.

Also requires [`yarn`](https://yarnpkg.com/), to install JavaScript dependencies.

`bauble` is being developed alongside [`codemirror-lang-janet`](https://github.com/ianthehenry/codemirror-lang-janet). If you want to make changes to the grammar, clone that repo and run [`yalc publish`](https://github.com/wclr/yalc) from the root of it. Then run `yalc link codemirror-lang-janet` in this repository, and you'll be able to see your changes locally.

# Building

Run `./build` to compile Janet to wasm; run `./build --prod` for an optimized, minified build that will take much longer. Run `./build --js` to only rebuild the UI or `./build --wasm` to only rebuild the Janet/C components.

# Running

```
$ node_modules/.bin/alive-server public
```
