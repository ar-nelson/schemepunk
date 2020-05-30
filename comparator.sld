;; Comparators (SRFI 128), polyfilled for Kawa and bugfixed for other Schemes.
;;
;; Polyfills taken from the SRFI 128 implementation at
;; https://srfi.schemers.org/srfi-128/srfi-128.html
;;
;; Defines a bunch of extra comparators as well.
;; Some are taken from SRFI 114, others are original.
;;
;; Also exports `hash-lambda`, a `lambda` that takes and ignores a 2nd argument.
;; R7RS implementations don't agree on the arity of SRFI 128 hash functions,
;; so this is necessary for compatibility.

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
          make-sum-comparator
          string-comparator string-ci-comparator symbol-comparator
          number-comparator char-comparator
          eq-comparator eqv-comparator equal-comparator
          hash-lambda identity-hash identity<?)

  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme inexact)
          (scheme complex)
          (schemepunk syntax)
          (schemepunk list))

  (cond-expand
    (gambit (export %salt%))
    (else))

  (cond-expand
    ((or chicken chibi larceny)
      (import (srfi 128)
              (rename (only (srfi 69) hash-by-identity)
                      (hash-by-identity identity-hash))))
    ((or (library (scheme comparator)) (library (srfi 128)))
      (cond-expand
        ((library (scheme comparator))
          (import (except (scheme comparator) make-pair-comparator
                                              make-list-comparator
                                              make-vector-comparator)))
        (else
          (import (except (srfi 128) make-pair-comparator
                                     make-list-comparator
                                     make-vector-comparator))))
      (cond-expand
        ((library (srfi 69))
          (import (rename (only (srfi 69) hash-by-identity)
                          (hash-by-identity identity-hash))))
        ((library (srfi 126))
          (import (rename (only (srfi 126) equal-hash)
                          (equal-hash identity-hash))))
        ((library (std srfi 125))
          (import (rename (only (std srfi 125) hash-by-identity)
                          (hash-by-identity identity-hash)))))

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
      (include "polyfills/128.body2.scm")

      (begin (define identity-hash equal-hash))))

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
             ((x y) . body)))))

    (define (make-sum-comparator . comparators)
      (define tests (map comparator-type-test-predicate comparators))
      (make-comparator
        (λ x (any (cut <> x) tests))
        (λ(x y)
          (let ((cmp-x (find (cut comparator-test-type <> x) comparators))
                (cmp-y (find (cut comparator-test-type <> y) comparators)))
            (and cmp-x (eq? cmp-x cmp-y) (=? cmp-x x y))))
        (λ(x y)
          (let ((ix (list-index (cut <> x) tests))
                (iy (list-index (cut <> y) tests)))
            (cond
              ((< ix iy) #t)
              ((> ix iy) #f)
              (else (<? (list-ref comparators ix) x y)))))
        (hash-lambda (x)
          (let* ((i (list-index (cut <> x) tests))
                 (cmp (list-ref comparators i))
                 (hash (comparator-hash cmp x)))
            (if (zero? i)
              hash
              (modulo (+ i (* hash 33)) (hash-bound)))))))

    (define (identity<? x y)
      (< (identity-hash x) (identity-hash y)))))
