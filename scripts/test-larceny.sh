#!/bin/bash
TEST_FILES="$(echo "$@" | sort | awk '{printf "(load \"%s\") ", $0}')"
echo "(import (scheme load)) $TEST_FILES (end-test-runner)" | larceny -r7 -I .
