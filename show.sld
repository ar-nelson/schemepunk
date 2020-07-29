(define-library (schemepunk show)
  (export show each each-in-list
          displayed written written-shared written-simply
          escaped maybe-escaped
          numeric numeric/comma numeric/si numeric/fitted
          nl fl space-to tab-to nothing
          joined joined/prefix joined/suffix
          joined/last joined/dot joined/range
          padded padded/right padded/both
          trimmed trimmed/right trimmed/both
          trimmed/lazy fitted fitted/right fitted/both
          fn with with! forked call-with-output
          make-state-variable port row col width output writer
          string-width substring/width pad-char ellipsis
          radix precision decimal-sep decimal-align
          sign-rule comma-rule comma-sep word-separator?
          ambiguous-is-wide?)
  (export pretty pretty-shared pretty-simply pretty-color
          pretty-json pretty-json-color)
  (export columnar tabular
          boxed boxed/double boxed/ascii boxed/custom
          wrapped wrapped/list wrapped/char justified
          from-file line-numbers)
  (export terminal-aware
          string-terminal-width string-terminal-width/wide
          substring-terminal-width substring-terminal-width/wide
          upcased downcased)
  (export as-red as-blue as-green as-cyan
          as-yellow as-magenta as-white as-black
          as-light-red as-light-blue as-light-green as-light-cyan
          as-light-yellow as-light-magenta as-light-white as-light-black as-gray
          as-bold as-italic as-underline
          as-color as-true-color
          on-red on-blue on-green on-cyan
          on-yellow on-magenta on-white on-black
          on-color on-true-color)
  (import (schemepunk show base)
          (schemepunk show pretty)
          (schemepunk show columnar)
          (schemepunk show unicode)
          (schemepunk show color)))
