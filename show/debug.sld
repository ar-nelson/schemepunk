(define-library (schemepunk show debug)
  (export write-debug write-debug-json)
  (import (scheme base)
          (schemepunk show base)
          (schemepunk show pretty))
  (begin
    (define (write-debug datum)
      (show #t (pretty-color datum) fl))

    (define (write-debug-json json)
      (show #t (pretty-json-color json) fl))))
