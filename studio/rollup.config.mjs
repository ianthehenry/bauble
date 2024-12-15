import nodeResolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import typescript from '@rollup/plugin-typescript';
import {babel} from "@rollup/plugin-babel";

const plugins = () => [
 commonjs(),
 nodeResolve(),
 typescript({include: '**/*.(ts|tsx|js)'}),
 babel({
   extensions: ["tsx"],
   babelHelpers: "bundled",
   presets: ["solid"],
 }),
];

export default [{
  strictDeprecations: true,
  input: "main.tsx",
  output: {
    file: "../build/rollup-artifacts/all.js",
    format: "iife",
  },
  plugins: plugins(),
}, {
  strictDeprecations: true,
  input: "embauble.ts",
  output: [{
    file: "../build/rollup-artifacts/embauble.iife.js",
    format: "iife",
    name: "Bauble",
  }, {
    file: "../build/rollup-artifacts/embauble.cjs.js",
    format: "cjs",
  }, {
    file: "../build/rollup-artifacts/embauble.esm.js",
    format: "es",
  }],
  plugins: plugins(),
}];
