redo-ifchange mode
mode=$(cat mode)

actual_outpath_jfc=$PWD/$3
cd ..

redo-ifchange janet/janet.{c,h} src/driver.cpp

janet_files=$(echo src/*.janet src/glslisp/src/*.janet)

redo-ifchange $janet_files

file_args=""
for file in $janet_files; do
  file_args="$file_args --embed-file $file@${file#src/}"
done

extra_flags="-O0"
if [[ $mode == "prod" ]]; then
  extra_flags="-O3 --closure 1"
fi

emcc \
  $extra_flags \
  -o $actual_outpath_jfc \
  -I janet \
  janet/janet.c \
  src/driver.cpp \
  $file_args \
  -lembind \
  -s "EXPORTED_FUNCTIONS=['_main']" \
  -s "EXPORTED_RUNTIME_METHODS=['FS', 'UTF8ToString']" \
  -s ALLOW_MEMORY_GROWTH=1 \
  -s AGGRESSIVE_VARIABLE_ELIMINATION=1 \
  -s MODULARIZE \
  -s EXPORT_ES6 \
  -s SINGLE_FILE
