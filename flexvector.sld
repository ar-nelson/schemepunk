;; A _flexvector_ is a dynamic array[1], like a Java ArrayList.
;;
;; Flexvectors support all of the basic vector operations, but additionally
;; support appending and deleting items in such a way that the length of vector
;; changes. When the vector grows too large for its initial size, it is copied
;; to a new vector with a larger capacity.
;;
;; [1]: https://en.wikipedia.org/wiki/Dynamic_array
(define-library (schemepunk flexvector)
  (export ; Constructors
          make-flexvector flexvector
          flexvector-unfold flexvector-unfold-right
          flexvector-copy flexvector-reverse-copy
          flexvector-append flexvector-concatenate flexvector-append-subvectors

          ; Predicates
          flexvector? flexvector-empty? flexvector=?

          ; Selectors
          flexvector-ref flexvector-front flexvector-back flexvector-length

          ; Mutators
          flexvector-add! flexvector-add-front! flexvector-add-back!
          flexvector-remove! flexvector-remove-front! flexvector-remove-back!
          flexvector-add-all! flexvector-remove-range! flexvector-clear!
          flexvector-set! flexvector-swap!
          flexvector-fill! flexvector-reverse!
          flexvector-copy! flexvector-reverse-copy!
          flexvector-append!

          ; Iteration
          flexvector-fold flexvector-fold-right
          flexvector-map flexvector-map! flexvector-map/index flexvector-map/index!
          flexvector-append-map flexvector-append-map/index
          flexvector-filter flexvector-filter! flexvector-filter/index flexvector-filter/index!
          flexvector-for-each flexvector-for-each/index
          flexvector-count flexvector-cumulate

          ; Searching
          flexvector-index flexvector-index-right
          flexvector-skip flexvector-skip-right
          flexvector-binary-search
          flexvector-any flexvector-every flexvector-partition

          ; Conversion
          flexvector->vector flexvector->list flexvector->string
          vector->flexvector list->flexvector string->flexvector
          reverse-flexvector->list reverse-list->flexvector
          generator->flexvector flexvector->generator)

  (cond-expand
    ((and (not chicken) (library (srfi 214)))
      (import (srfi 214)))
    (else
      (import (scheme base)
              (scheme case-lambda)
              (schemepunk syntax)
              (schemepunk list))

      (include "polyfills/flexvectors-body1.scm")
      (include "polyfills/flexvectors-body2.scm"))))
