(define-library (schemepunk vector)
  (export
    vector-unfold vector-unfold-right
    vector-reverse-copy
    vector-concatenate vector-append-subvectors

    vector-empty?
    vector=

    vector-fold vector-fold-right
    vector-map!
    vector-count
    vector-cumulate

    vector-index vector-index-right
    vector-skip vector-skip-right
    vector-binary-search
    vector-any vector-every
    vector-partition

    vector-swap!
    vector-reverse!
    vector-reverse-copy!
    vector-unfold! vector-unfold-right!

    reverse-vector->list
    reverse-list->vector

    vector-filter)

  (import (scheme base))

  (cond-expand
    (chicken (import (srfi 133)))
    ((library (scheme vector)) (import (scheme vector)))
    ((library (srfi 133)) (import (srfi 133)))
    ((library (std srfi 133)) (import (std srfi 133)))
    (else
      (import (scheme base))
      (include "polyfills/vector.scm")))

  (begin
    (define (vector-filter pred? vec)
      (define-values (partitioned i) (vector-partition pred? vec))
      (cond
        ((= i (vector-length vec)) vec)
        (else (vector-copy partitioned 0 i))))))
