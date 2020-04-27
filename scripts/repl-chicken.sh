#!/bin/bash
rlwrap csi -D debug -R r7rs -R utf8 $(find . -name '*.sld' | "$(dirname "$0")/find-dependencies.scm" $(dirname "$0")/repl.scm) "$(dirname "$0")/repl.scm"
