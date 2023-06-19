# [Bauble](https://bauble.studio)

Bauble is a toy for composing signed distance functions in a high-level language ([Janet](https://janet-lang.org/)), compiling them to [GLSL](https://www.khronos.org/opengl/wiki/OpenGL_Shading_Language), and rendering them via WebGL.

Bauble is still in its early days, but it's progressed to the point that you can do some pretty neat stuff with it. Try it out at <https://bauble.studio/>, or watch this video introduction where I model an infinite number of hot air balloons:

[![Livecoding a hot air balloon (1/3)](https://img.youtube.com/vi/0-OtdjiR7dc/maxresdefault.jpg)](https://www.youtube.com/watch?v=0-OtdjiR7dc&list=PLjT5GDnW_UMBS6ih0kG7jWB0n1SnotnEu)

For more examples, I sometimes tweet videos of Bauble's development:

- [bounding surfaces](https://twitter.com/ianthehenry/status/1567709580792315904)
- [instanced repetition](https://twitter.com/ianthehenry/status/1566583962989842432)
- [animation](https://twitter.com/ianthehenry/status/1566081717592502274)
- [procedural distortion](https://twitter.com/ianthehenry/status/1565575515016085504)
- [non-rigid deformations](https://twitter.com/ianthehenry/status/1559778903324954624)
- [announcement](https://twitter.com/ianthehenry/status/1559049547099254785)
- [surfaces](https://twitter.com/ianthehenry/status/1557881955156275200)
- [shapes](https://twitter.com/ianthehenry/status/1554729639183937536)
- [first ever demo](https://twitter.com/ianthehenry/status/1551422839307190272)

# Dependencies

Bauble requires at least Janet 1.29.1. It may work with newer versions of Janet, assuming that the image format is compatible, but it's better to [update the version of Janet that Bauble includes](build/janet/janet-version) to match your local version.

- [`emscripten`](https://emscripten.org/)
- [`redo`](https://github.com/apenwarr/redo)
- [`yarn`](https://yarnpkg.com/)

Afterwards, install JavaScript dependencies with:

```
$ yarn
$ (cd ui && yarn)
```

`bauble` is being developed alongside [`codemirror-lang-janet`](https://github.com/ianthehenry/codemirror-lang-janet). If you want to make changes to the grammar, clone that repo and run [`yalc publish`](https://github.com/wclr/yalc) from the root of it. Then run `yalc link codemirror-lang-janet` in this repository, and you'll be able to see your changes locally.

# Development

To build Bauble, all you have to do is run:

```
$ redo
```

To create a minified, optimized build, use:

```
$ BUILD_MODE=prod redo
```

Lint the JS with:

```
(cd ui/; yarn eslint .)
```

You can serve a local Bauble like this:

```
$ node_modules/.bin/alive-server public
```

# Known bugs

- [ ] Bauble will cast and calculate lights even for shaders that don't need lights, making it slower than it needs to be in the default RGB-normal shading view.
