#!/usr/bin/env bash

redo-ifchange \
  main.css{,.checksum} \
  all.js{,.checksum} \
  player.{iife,cjs,esm}.js{,.checksum} \
  compiler.{iife,cjs,esm}.js{,.checksum} \
  docs.jimage

css=$(< main.css.checksum)
js=$(< all.js.checksum)
player_iife=$(< player.iife.js.checksum)
player_cjs=$(< player.cjs.js.checksum)
player_esm=$(< player.esm.js.checksum)
compiler_iife=$(< compiler.iife.js.checksum)
compiler_cjs=$(< compiler.cjs.js.checksum)
compiler_esm=$(< compiler.esm.js.checksum)

cd ..

rm -rf public
mkdir -p public

assets=$(echo studio/assets/*)
redo-ifchange $assets

ln -f $assets public/
ln -f build/main.css public/$css
ln -f build/all.js public/$js

mkdir -p public/dist

ln -f build/player.iife.js public/dist/$player_iife
ln -f build/player.cjs.js public/dist/$player_cjs
ln -f build/player.esm.js public/dist/$player_esm

ln -f build/compiler.iife.js public/dist/$compiler_iife
ln -f build/compiler.cjs.js public/dist/$compiler_cjs
ln -f build/compiler.esm.js public/dist/$compiler_esm

find studio/html -type f -exec redo-ifchange {} \+

mkdir -p public/{about,help,embed}
studio/html/home "/$css" "/$js" > public/index.html
studio/html/about/index "/$css" "/$js" > public/about/index.html
studio/html/embed/index "/$css" "/dist/$player_iife /dist/$compiler_iife" > public/embed/index.html
studio/html/help/index "/$css" "/$js" \
  "/dist/$player_iife" \
  "/dist/$player_cjs" \
  "/dist/$player_esm" \
  "/dist/$compiler_iife" \
  "/dist/$compiler_cjs" \
  "/dist/$compiler_esm" \
  > public/help/index.html

tree public --noreport | tr ' ' ' '
