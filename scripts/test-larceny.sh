#!/bin/bash
TEST_FILES="$(find . -name *.test.scm | sort | awk '{printf "(load \"%s\") ", $0}')"
echo "(import (scheme load)) $TEST_FILES (end-test-runner)" | larceny -r7 -I .
