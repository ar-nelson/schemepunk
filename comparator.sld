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
          string-comparator string-ci-comparator symbol-comparator
          number-comparator char-comparator
          eq-comparator eqv-comparator equal-comparator
          hash-lambda)

  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme inexact)
          (scheme complex)
          (schemepunk syntax))

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

  (cond-expand
    ((and (not chicken) (library (srfi 114)))
      (import (only (srfi 114) string-comparator string-ci-comparator
                               symbol-comparator number-comparator char-comparator
                               eq-comparator eqv-comparator equal-comparator)))
    (else
      (begin
        (cond-expand
          ((or chicken (library (srfi 128)) (library (scheme comparator)))
            (define (symbol<? x y)
              (string<? (symbol->string x) (symbol->string y))))
          (else))

        (define string-comparator
          (make-comparator string? string=? string<? string-hash))

        (define string-ci-comparator
          (make-comparator string? string-ci=? string-ci<? string-ci-hash))

        (define symbol-comparator
          (make-comparator symbol? symbol=? symbol<? symbol-hash))

        (define number-comparator
          (make-comparator number? = < number-hash))

        (define char-comparator
          (make-comparator char? char=? char<? char-hash))

        (define eq-comparator (make-eq-comparator))
        (define eqv-comparator (make-eqv-comparator))
        (define equal-comparator (make-equal-comparator)))))

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
             ((x y) . body)))))))
