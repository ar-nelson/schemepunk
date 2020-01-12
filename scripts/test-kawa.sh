#!/bin/bash
"$(dirname "$0")/kawa.sh" $(find . -name '*.test.scm' | sort) "$(dirname "$0")/end.scm"
