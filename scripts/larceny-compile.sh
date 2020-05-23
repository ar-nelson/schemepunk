#!/bin/bash
set -eou pipefail
for lib in $(find . -name '*.sld' | "$(dirname "$0")/find-dependencies.scm" "$1"); do
  expect <<EOF
    set timeout -1
    spawn compile-larceny -I "$(pwd)" "$lib"
    expect {
      "Entering debugger" { exit 1 }
    }
EOF
done
compile-larceny -o "$2" -I "$(pwd)" "$1"
