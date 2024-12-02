#!/usr/bin/env bash

redo-ifchange mode wasm.js

mode=$(cat mode)

actual_outpath_jfc=$PWD/$3
mkdir -p rollup-artifacts

cd ../studio

redo-ifchange *.ts *.tsx tsconfig.json rollup.config.mjs yarn.lock types/*/*.ts

declare rollup_args
if [[ $mode == "prod" ]]; then
  rollup_args="--no-indent"
else
  rollup_args="--no-treeshake --no-indent"
fi

./node_modules/.bin/rollup -c $rollup_args

shasum ../build/rollup-artifacts/* > "$actual_outpath_jfc"
