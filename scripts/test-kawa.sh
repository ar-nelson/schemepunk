#!/bin/bash
"$(dirname "$0")/kawa.sh" $(echo "$@" | sort) "$(dirname "$0")/end.scm"
