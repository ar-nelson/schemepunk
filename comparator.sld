;; Comparators (SRFI 128), polyfilled for Kawa, which doesn't implement it.
;;
;; Polyfills taken from the SRFI 128 implementation at
;; https://srfi.schemers.org/srfi-128/srfi-128.html
;;
;; Also exports `hash-lambda`, which defines a hash function that can handle
;; either one or two arguments. R7RS implementations don't agree on the arity of
;; hash functions for SRFI 128, so this is necessary for compatibility.
(define-library (schemepunk comparator)
  (export comparator? comparator-ordered? comparator-hashable?
          make-comparator
          make-pair-comparator make-list-comparator make-vector-comparator
          make-eq-comparator make-eqv-comparator make-equal-comparator
          boolean-hash char-hash char-ci-hash
          string-hash string-ci-hash symbol-hash number-hash
          make-default-comparator default-hash comparator-register-default!
          comparator-type-test-predicate comparator-equality-predicate
          comparator-ordering-predicate comparator-hash-function
          comparator-test-type comparator-check-type comparator-hash
          hash-bound hash-salt
          =? <? >? <=? >=?
          comparator-if<=>
          hash-lambda string-comparator)

  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme inexact)
          (scheme complex))

  (cond-expand
    ((or chicken chibi larceny) (import (srfi 128)))
    ((library (scheme comparator))
      (import (except (scheme comparator) make-pair-comparator
                                          make-list-comparator
                                          make-vector-comparator))
      (include "polyfills/128.universals.scm"))
    ((library (srfi 128))
      (import (except (srfi 128) make-pair-comparator
                                 make-list-comparator
                                 make-vector-comparator))
      (include "polyfills/128.universals.scm"))
    (else
      (cond-expand
        ((library (srfi 126))
          (import (only (srfi 126) equal-hash string-hash string-ci-hash)))
        ((library (srfi 69))
          (import (rename (only (srfi 69) hash-by-identity)
                          (hash-by-identity equal-hash))
                  (only (srfi 69) string-hash string-ci-hash))))

      (include "polyfills/128.body1.scm")
      (include "polyfills/128.universals.scm")
      (include "polyfills/128.body2.scm")))

  (begin
    (define-syntax hash-lambda
      (syntax-rules ()
        ((hash-lambda (x) . body)
           (case-lambda
             ((x) . body)
             ((x y) . body)))
        ((hash-lambda (x y) . body)
           (case-lambda
             ((x) (let ((y 0)) . body))
             ((x y) . body)))))

    (define string-comparator
      (make-comparator string? string=? string<? string-hash))))
