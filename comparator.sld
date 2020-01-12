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
          hash-lambda)

  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme inexact)
          (scheme complex))

  (cond-expand
    ((library (std srfi 128))
       ; Gerbil's make-vector-comparator is broken[1], so we patch it with our
       ; own implementation here.
       ;
       ; [1]: https://github.com/vyzo/gerbil/issues/425
       (import (rename (std srfi 128)
                       (make-vector-comparator %make-vector-comparator%)))

       (begin
         (define (make-vector-comparator element-comparator type-test length ref)
           (define original-comparator
             (%make-vector-comparator% element-comparator type-test length ref))
           (make-comparator
             (comparator-type-test-predicate original-comparator)
             (make-vector=? element-comparator type-test length ref)
             (comparator-ordering-predicate original-comparator)
             (comparator-hash-function original-comparator)))

         (define (make-vector=? element-comparator type-test length ref)
            (lambda (a b)
              (and
                (= (length a) (length b))
                (let ((elem=? (comparator-equality-predicate element-comparator))
                      (len (length b)))
                  (let loop ((n 0))
                    (cond
                      ((= n len) #t)
                      ((elem=? (ref a n) (ref b n)) (loop (+ n 1)))
                      (else #f)))))))))
    ((library (scheme comparator)) (import (scheme comparator)))
    ((library (srfi 128)) (import (srfi 128)))
    (else
      (cond-expand
        ((library (srfi 126))
           (import (only (srfi 126) equal-hash string-hash string-ci-hash)))
        ((library (srfi 69))
           (import (rename (only (srfi 69) hash-by-identity)
                           (hash-by-identity equal-hash))
                   (only (srfi 69) string-hash string-ci-hash))))

        (include "polyfills/128.body1.scm")
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
             ((x y) . body)))))))
