#!/bin/bash
csi -R r7rs -qb $(find . -name '*.sld' | "$(dirname "$0")/find-dependencies.scm" "$@") "$@"
