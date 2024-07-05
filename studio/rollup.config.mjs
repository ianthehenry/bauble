import nodeResolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
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
    commonjs(),
    nodeResolve(),
    typescript({include: '**/*.(ts|tsx|js)'}),
    babel({
      extensions: ["tsx"],
      babelHelpers: "bundled",
      presets: ["solid"],
    }),
  ],
});
