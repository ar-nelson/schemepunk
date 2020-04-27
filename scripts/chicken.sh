#!/bin/bash
csi -D debug -R r7rs -R utf8 -qbn $(find . -name '*.sld' | "$(dirname "$0")/find-dependencies.scm" "$@") "$@"
