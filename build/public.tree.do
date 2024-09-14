#!/usr/bin/env bash

redo-ifchange main.css{,.checksum} all.js{,.checksum} docs.jimage

css=$(< main.css.checksum)
js=$(< all.js.checksum)

cd ..

rm -rf public
mkdir -p public

ln -f studio/assets/icons.svg public/icons.svg
ln -f build/main.css public/$css
ln -f build/all.js public/$js

mkdir -p public/{about,help}
studio/html/home "/$css" "/$js" > public/index.html
studio/html/about/index "/$css" "/$js" > public/about/index.html
studio/html/help/index "/$css" "/$js" > public/help/index.html

tree public --noreport | tr ' ' ' '
