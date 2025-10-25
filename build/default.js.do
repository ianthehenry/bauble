#!/usr/bin/env bash

redo-ifchange mode rollup
mode=$(cat mode)

actual_outpath_jfc=$PWD/$3

# the fake "rollup" target created this
ln -f rollup-artifacts/$1 $3

# So SolidJS performs a top-level effect at load time that reads from
# the window. In a worker, this throws. So we don't perform the effect
# in a window.
sed -E -e "s/^delegateEvents/if (typeof window !== 'undefined') delegateEvents/" -i= "$3"

# Similar, but this time it's emscripten that wants to read the current script path for some reason.
sed -E -e "s|document\.baseURI|(typeof document == 'undefined' ? 'https://example.org/' : document.baseURI)|" -i= "$3"

cd ../studio

if [[ $mode == "prod" ]]; then
  ./node_modules/.bin/terser "$actual_outpath_jfc" -o "$actual_outpath_jfc"
fi
