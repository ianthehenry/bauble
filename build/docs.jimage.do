#!/usr/bin/env bash

actual_outpath_jfc=$PWD/$3
redo-ifchange commit-hash
cd ../src

redo-ifchange $(find . -path '*/jpm_tree' -prune -o -name '*.janet')

jpm -l janet -c lib/docs.janet $actual_outpath_jfc
