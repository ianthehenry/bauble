import nodeResolve from '@rollup/plugin-node-resolve';
import typescript from '@rollup/plugin-typescript';
import babel from "@rollup/plugin-babel";

export default ({
  input: "main.tsx",
  output: {
    file: "../out/all.js",
    format: "iife",
  },
  plugins: [
    nodeResolve(),
    typescript(),
    babel({
      extensions: ["tsx"],
      babelHelpers: "bundled",
      presets: ["solid"],
    }),
  ],
});
