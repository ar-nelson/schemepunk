#!/bin/bash
csi -R r7rs -R utf8 -qb $(find . -name '*.sld' | "$(dirname "$0")/find-dependencies.scm" "$@") "$@"
