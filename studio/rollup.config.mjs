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
  input: "player.ts",
  output: [{
    file: "../build/rollup-artifacts/player.iife.js",
    format: "iife",
    name: "Bauble",
  }, {
    file: "../build/rollup-artifacts/player.cjs.js",
    format: "cjs",
  }, {
    file: "../build/rollup-artifacts/player.esm.js",
    format: "es",
  }],
  plugins: plugins(),
}, {
  strictDeprecations: true,
  input: "compiler.ts",
  output: [{
    file: "../build/rollup-artifacts/compiler.iife.js",
    format: "iife",
    name: "BaubleCompiler",
  }, {
    file: "../build/rollup-artifacts/compiler.cjs.js",
    format: "cjs",
  }, {
    file: "../build/rollup-artifacts/compiler.esm.js",
    format: "es",
  }],
  plugins: plugins(),
}];
