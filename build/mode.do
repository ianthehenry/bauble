#!/usr/bin/env bash

echo ${BUILD_MODE:-dev} >$3
redo-always
redo-stamp <$3
