#!/bin/bash
"$(dirname "$0")/gerbil.sh" $(find . -name *.test.scm | sort) "$(dirname "$0")/end.scm"
