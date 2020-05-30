#!/bin/bash
set -eou pipefail
gxc -v -gsc-flag -static \
  $("$(dirname "$0")/../schemepunk/scripts/find-dependencies.scm" "$1")
gxc -v -static -exe -o "$2" "$1"
