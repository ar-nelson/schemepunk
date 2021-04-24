;; Comparators (SRFI 128), polyfilled for Kawa and bugfixed for other Schemes.
;;
;; Polyfills taken from the SRFI 128 implementation at
;; https://srfi.schemers.org/srfi-128/srfi-128.html
;;
;; Defines a bunch of extra comparators as well.
;; Some are taken from SRFI 162 or 114, others are original.
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
          comparator-if<=>)
  (export comparator-max comparator-min
          comparator-max-in-list comparator-min-in-list
          default-comparator boolean-comparator real-comparator
          fixnum-comparator char-comparator char-ci-comparator
          string-comparator string-ci-comparator
          pair-comparator list-comparator vector-comparator
          eq-comparator eqv-comparator equal-comparator)
  (export make-sum-comparator symbol-comparator number-comparator
          hash-lambda identity-hash identity<?)

  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme inexact)
          (scheme complex)
          (scheme lazy)
          (schemepunk syntax)
          (schemepunk list))

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
             ((x y) . body))))))

  (cond-expand
    ((and (not chicken) (library (srfi 143)))
      (import (srfi 143))
      (begin
        (define fixnum-comparator
          (make-comparator fixnum? fx=? fx<? (hash-lambda (x) x)))))
    (gerbil
      (import (std srfi 143))
      (begin
        (define fixnum-comparator
          (make-comparator fixnum? fx=? fx<? (hash-lambda (x) x)))))
    (else
      (begin
        (define fixnum-comparator
          (make-comparator
            (λ x (and (number? x) (exact? x) (integer? x)))
            =
            <
            (hash-lambda (x) x))))))

  (cond-expand
    ((and (not chicken) (not gauche) (library (srfi 162)))
      (import (srfi 162)))
    ((and (not chicken) (library (srfi 114)))
      (import (only (srfi 114)
                comparator-max comparator-min
                default-comparator boolean-comparator real-comparator
                char-comparator char-ci-comparator
                string-comparator string-ci-comparator
                pair-comparator list-comparator vector-comparator
                eq-comparator eqv-comparator equal-comparator
                symbol-comparator number-comparator))
      (begin
        (define (comparator-max-in-list comp xs)
          (apply comparator-max comp xs))

        (define (comparator-min-in-list comp xs)
          (apply comparator-min comp xs))))
    (else
      (begin
        (cond-expand
          ((or chicken (library (srfi 128)) (library (scheme comparator)))
            ; If the polyfill was not loaded, define symbol<? here
            (define (symbol<? x y)
              (string<? (symbol->string x) (symbol->string y))))
          (else))

        (define (comparator-max-in-list comp list)
          (let ((< (comparator-ordering-predicate comp)))
            (let loop ((max (car list)) (list (cdr list)))
              (if (null? list)
                max
                (if (< max (car list))
                  (loop (car list) (cdr list))
                  (loop max (cdr list)))))))

        (define (comparator-min-in-list comp list)
          (let ((< (comparator-ordering-predicate comp)))
            (let loop ((min (car list)) (list (cdr list)))
              (if (null? list)
                min
                (if (< min (car list))
                  (loop min (cdr list))
                  (loop (car list) (cdr list)))))))

        (define (comparator-max comp . args)
          (comparator-max-in-list comp args))

        (define (comparator-min comp . args)
          (comparator-min-in-list comp args))

        (define-syntax lazy-comparator
          (syntax-rules ()
            ((_ cmp)
              (let1 wrapped (delay cmp)
                (make-comparator
                  (cut comparator-test-type (force wrapped) <>)
                  (cut =? (force wrapped) <> <>)
                  (cut <? (force wrapped) <> <>)
                  (cut comparator-hash (force wrapped) <>))))))

        (define default-comparator
          (lazy-comparator (make-default-comparator)))

        (define boolean-comparator
          (make-comparator boolean? boolean=? (λ(x y) (and (not x) y)) (hash-lambda (x) (if x 1 0))))

        (define real-comparator
          (make-comparator real? = < number-hash))

        (define char-comparator
          (make-comparator char? char=? char<? char-hash))

        (define char-ci-comparator
          (make-comparator char? char-ci=? char-ci<? char-ci-hash))

        (define string-comparator
          (make-comparator string? string=? string<? string-hash))

        (define string-ci-comparator
          (make-comparator string? string-ci=? string-ci<? string-ci-hash))

        (define symbol-comparator
          (make-comparator symbol? symbol=? symbol<? symbol-hash))

        (define number-comparator
          (make-comparator number? = < number-hash))

        (define pair-comparator
          (lazy-comparator (make-pair-comparator default-comparator default-comparator)))

        (define list-comparator
          (lazy-comparator (make-list-comparator default-comparator list? null? car cdr)))

        (define vector-comparator
          (lazy-comparator (make-vector-comparator default-comparator vector? vector-length vector-ref)))

        (define eq-comparator (lazy-comparator (make-eq-comparator)))
        (define eqv-comparator (lazy-comparator (make-eqv-comparator)))
        (define equal-comparator (lazy-comparator (make-equal-comparator))))))

  (begin
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
