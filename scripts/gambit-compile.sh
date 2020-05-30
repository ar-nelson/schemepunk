#!/bin/bash
set -eou pipefail
gsc -warnings -debug -module-ref 'srfi/8' -o srfi8.c -c '/usr/local/Gambit/lib/srfi/8/8.sld'
gsc -warnings -debug -module-ref 'srfi/69' -o srfi69.c -c '/usr/local/Gambit/lib/srfi/69/69.sld'
gsc -warnings -debug -module-ref 'srfi/132' -o srfi132.c -c '/usr/local/Gambit/lib/srfi/132/132.sld'
c_files="srfi8.c srfi69.c srfi132.c"
for lib in $("$(dirname "$0")/find-dependencies.scm" "$1"); do
  module="${lib#./}"
  module="${module%.*}"
  echo "$module"
  gsc -:t8,f8,-8,r7rs -warnings -debug -module-ref "$module" -c . "$lib"
  c_files="$c_files $module.c"
done
gsc -:t8,f8,-8,r7rs -warnings -debug -exe -o "$2" -nopreload . $c_files "$1"
