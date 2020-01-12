#!/bin/bash

to_f () {
  awk '{printf "-f %s ", $0}'
}

to_load () {
  tr " " "\n" | awk '{printf "(load \"%s\") ", $0}'
}

kawa --full-tailcalls $(find . -name '*.sld' | "$(dirname "$0")/find-dependencies.scm" "$@" | to_f) -e "$(echo "$@" | to_load)"
