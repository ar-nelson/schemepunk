#!/bin/bash
TEST_FILES="$(echo "$@" | sort | awk '{printf "(load \"%s\") ", $0}')"
chibi-scheme -D debug -m "(schemepunk test)" -e "(begin $TEST_FILES (end-test-runner))"
