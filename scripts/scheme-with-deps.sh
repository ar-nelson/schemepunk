#!/bin/bash
FILE=$1
shift
gosh -r7 $(find . -name '*.sld' | "$(dirname "$0")/find-dependencies.scm" "$FILE" | awk '{printf "-l%s ", $0}') "$@"
