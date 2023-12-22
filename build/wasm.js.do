#!/usr/bin/env bash

redo-ifchange mode
mode=$(cat mode)

actual_outpath_jfc=$PWD/$3
cd ..

redo-ifchange build/janet/janet.{c,h} src/driver.cpp build/bauble.jimage src/intro.janet

extra_flags="-O0"
if [[ $mode == "prod" ]]; then
  extra_flags="-O3 --closure 1"
fi

emcc \
  $extra_flags \
  -o $actual_outpath_jfc \
  -I build/janet \
  build/janet/janet.c \
  src/driver.cpp \
  --embed-file build/bauble.jimage@bauble.jimage \
  --embed-file src/intro.janet@intro.janet \
  -lembind \
  -s "EXPORTED_FUNCTIONS=['_main']" \
  -s "EXPORTED_RUNTIME_METHODS=['FS', 'UTF8ToString']" \
  -s ALLOW_MEMORY_GROWTH=1 \
  -s AGGRESSIVE_VARIABLE_ELIMINATION=1 \
  -s MODULARIZE \
  -s EXPORT_ES6 \
  -s SINGLE_FILE
