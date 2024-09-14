#!/usr/bin/env bash

actual_outpath_jfc=$PWD/$3
cd ../src

redo-ifchange $(find . -path '*/jpm_tree' -prune -o -name '*.janet' | grep -vF 'docs.janet')

jpm -l janet -c lib/init.janet $actual_outpath_jfc
