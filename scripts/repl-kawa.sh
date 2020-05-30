#!/bin/bash

with_f () {
  awk '{printf "-f %s ", $0}'
}

kawa --full-tailcalls $("$(dirname "$0")/find-dependencies.scm" "$(dirname "$0")/repl.scm" | with_f) -f "$(dirname "$0")/repl.scm" -s
