#!/bin/bash
gxc -gsc-flag -:t8,f8,-8 -static \
  $(find . -name '*.sld' | "$(dirname "$0")/../schemepunk/scripts/find-dependencies.scm" "$1")
gxc -static -exe -o "$2" "$1"
