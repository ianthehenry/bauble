#!/usr/bin/env bash

actual_outpath_jfc=$PWD/$3
cd ..

redo-ifchange $(find src -path '*/jpm_tree' -prune -o -name '*.janet')

janet -c src/init.janet $actual_outpath_jfc
