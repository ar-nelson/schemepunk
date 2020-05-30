#!/bin/bash
gxi -:t8,f8,-8 --lang r7rs $("$(dirname "$0")/find-dependencies.scm" $(dirname "$0")/repl.scm) "$(dirname "$0")/repl.scm" -
