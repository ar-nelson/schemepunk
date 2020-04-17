#!/bin/bash
TEMP_SCRIPT="$(mktemp)"
echo "(import (scheme load) (schemepunk test))" >> $TEMP_SCRIPT
echo "$@" | sort | awk '{printf "(load \"%s\") ", $0}' >> $TEMP_SCRIPT
echo "(end-test-runner)" >> $TEMP_SCRIPT
sagittarius -r7 -L. "$TEMP_SCRIPT"
rm "$TEMP_SCRIPT"
