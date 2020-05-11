#!/bin/bash
for lib in $(find . -name '*.sld' | "$(dirname "$0")/find-dependencies.scm" "$1"); do
  compile-larceny -I "$(pwd)" "$lib"
done
compile-larceny -o "$2" -I "$(pwd)" "$1"
