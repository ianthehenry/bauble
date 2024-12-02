#!/usr/bin/env bash

deps="editor.css page.css vars.css mixed.css"
redo-ifchange $deps

cat $deps
