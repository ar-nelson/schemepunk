(import (scheme base) (srfi 125) (srfi 128))
(define ht
  (make-hash-table (make-vector-comparator (make-default-comparator)
                                           vector?
                                           vector-length
                                           vector-ref)))
(hash-table-set! ht #(1 2) 'foo)
