import nodeResolve from '@rollup/plugin-node-resolve';
import typescript from '@rollup/plugin-typescript';
import {babel} from "@rollup/plugin-babel";

export default ({
  strictDeprecations: true,
  input: "main.tsx",
  output: {
    file: "../build/all.js",
    format: "iife",
  },
  plugins: [
    nodeResolve(),
    typescript({include: '**/*.(ts|tsx|js)'}),
    babel({
      extensions: ["tsx"],
      babelHelpers: "bundled",
      presets: ["solid"],
    }),
  ],
});
