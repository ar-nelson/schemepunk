#!/bin/bash

# Gerbil and Gambit require a command-line argument for Unicode support.
# -:t8,f8,-8 sets Gambit terminal IO, file IO, and stdio (respectively) to UTF-8.

gxi -:t8,f8,-8 --lang r7rs $("$(dirname "$0")/find-dependencies.scm" "$@") "$@"
