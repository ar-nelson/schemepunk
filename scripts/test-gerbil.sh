#!/bin/bash
"$(dirname "$0")/gerbil.sh" $(echo "$@" | sort) "$(dirname "$0")/end.scm"
