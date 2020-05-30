#!/bin/bash

to_f () {
  tr " " "\n" | awk '{printf "-f %s ", $0}'
}

to_load () {
  tr " " "\n" | awk '{printf "(load \"%s\") ", $0}'
}

kawa --full-tailcalls $("$(dirname "$0")/find-dependencies.scm" "$@" | to_f) -e "$(echo "$@" | to_load)"
