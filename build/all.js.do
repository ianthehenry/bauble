#!/usr/bin/env bash

redo-ifchange mode wasm.js
mode=$(cat mode)

actual_outpath_jfc=$PWD/$3

cd ../studio

redo-ifchange *.ts *.tsx tsconfig.json rollup.config.mjs yarn.lock types/*/*.ts

rollup_args="--no-treeshake --no-indent"
if [[ $mode == "prod" ]]; then
  rollup_args="-p @rollup/plugin-terser"
fi

./node_modules/.bin/rollup -c $rollup_args -o $actual_outpath_jfc

# So SolidJS performs a top-level effect at load time that reads from
# the window. In a worker, this throws. So we don't perform the effect
# in a window.
sed -E "s/^delegateEvents/if (typeof window !== 'undefined') delegateEvents/" -i $actual_outpath_jfc

# Similar, but this time it's emscripten that wants to read the current script path for some reason.
sed -E "s|document\.baseURI|(typeof document == 'undefined' ? 'https://example.org/' : document.baseURI)|" -i $actual_outpath_jfc
