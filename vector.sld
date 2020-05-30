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

  (import (scheme base)
          (schemepunk syntax))

  (cond-expand
    (chicken (import (srfi 133)))
    (chibi
      (import (rename (srfi 133) (vector-every %vector-every)))
      (begin
        ; Chibi bug: vector-every fails on empty vector
        (define (vector-every pred? . vecs)
          (define len (apply min (map vector-length vecs)))
          (or (zero? len) (apply %vector-every (cons pred? vecs))))))
    ((library (scheme vector)) (import (scheme vector)))
    ((library (srfi 133)) (import (srfi 133)))
    ((library (std srfi 133)) (import (std srfi 133)))
    (else
      (import (scheme base))
      (include "polyfills/vector.scm")))

  (begin
    (define (vector-filter pred? vec)
      (let1-values (partitioned i) (vector-partition pred? vec)
        (cond
          ((= i (vector-length vec)) vec)
          (else (vector-copy partitioned 0 i)))))))
