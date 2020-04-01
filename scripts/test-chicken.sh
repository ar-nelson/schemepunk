#!/bin/bash
"$(dirname "$0")/chicken.sh" $(echo "$@" | sort) "$(dirname "$0")/end.scm"
