#!/bin/bash
TEST_FILES="$(find . -name *.test.scm | sort | awk '{printf "(load \"%s\") ", $0}')"
chibi-scheme -m "(schemepunk test)" -e "(begin $TEST_FILES (end-test-runner))"
