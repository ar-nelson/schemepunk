#!/bin/bash
TEST_FILES="$(echo "$@" | sort | awk '{printf "(load \"%s\") ", $0}')"
gosh -r7 -I. -b -e "(begin (import (scheme load) (schemepunk test)) $TEST_FILES (end-test-runner))"
