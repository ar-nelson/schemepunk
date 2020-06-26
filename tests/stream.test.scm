(import (scheme base)
        (schemepunk syntax)
        (schemepunk stream)
        (schemepunk test))

(test-group "Streams"
  ; Basic sanity test that SRFI 41 exists
  (test-equal '(1 2 3) (stream->list (stream 1 2 3))))

