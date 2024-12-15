#!/usr/bin/env bash

redo-ifchange main.css{,.checksum} all.js{,.checksum} embauble.{iife,cjs,esm}.js{,.checksum} docs.jimage

css=$(< main.css.checksum)
js=$(< all.js.checksum)
embauble_iife=$(< embauble.iife.js.checksum)
embauble_cjs=$(< embauble.cjs.js.checksum)
embauble_esm=$(< embauble.esm.js.checksum)

cd ..

rm -rf public
mkdir -p public

assets=$(echo studio/assets/*)
redo-ifchange $assets

ln -f $assets public/
ln -f build/main.css public/$css
ln -f build/all.js public/$js

mkdir -p public/embauble
ln -f build/embauble.iife.js public/embauble/$embauble_iife
ln -f build/embauble.cjs.js public/embauble/$embauble_cjs
ln -f build/embauble.esm.js public/embauble/$embauble_esm

find studio/html -type f -exec redo-ifchange {} \+

mkdir -p public/{about,help,embed}
studio/html/home "/$css" "/$js" > public/index.html
studio/html/about/index "/$css" "/$js" > public/about/index.html
studio/html/embed/index "/$css" "/embauble/$embauble_iife" > public/embed/index.html
studio/html/help/index "/$css" "/$js" \
  "/embauble/$embauble_iife" \
  "/embauble/$embauble_cjs" \
  "/embauble/$embauble_esm" \
  > public/help/index.html

tree public --noreport | tr ' ' ' '
