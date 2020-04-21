(define-library (schemepunk mapping)
  (export
    mapping mapping-unfold
    mapping/ordered mapping-unfold/ordered
    mapping? mapping-contains? mapping-empty? mapping-disjoint?
    mapping-ref mapping-ref/default mapping-key-comparator
    mapping-adjoin mapping-adjoin!
    mapping-set mapping-set!
    mapping-replace mapping-replace!
    mapping-delete mapping-delete! mapping-delete-all mapping-delete-all!
    mapping-intern mapping-intern!
    mapping-update mapping-update! mapping-update/default mapping-update!/default
    mapping-pop mapping-pop!
    mapping-search mapping-search!
    mapping-size mapping-find mapping-count mapping-any? mapping-every?
    mapping-keys mapping-values mapping-entries
    mapping-map mapping-map->list mapping-for-each mapping-fold
    mapping-filter mapping-filter!
    mapping-remove mapping-remove!
    mapping-partition mapping-partition!
    mapping-copy mapping->alist alist->mapping alist->mapping!
    alist->mapping/ordered alist->mapping/ordered!
    mapping=? mapping<? mapping>? mapping<=? mapping>=?
    mapping-union mapping-intersection mapping-difference mapping-xor
    mapping-union! mapping-intersection! mapping-difference! mapping-xor!
    make-mapping-comparator
    mapping-comparator
    mapping-min-key mapping-max-key
    mapping-min-value mapping-max-value
    mapping-key-predecessor mapping-key-successor
    mapping-range= mapping-range< mapping-range> mapping-range<= mapping-range>=
    mapping-range=! mapping-range<! mapping-range>! mapping-range<=! mapping-range>=!
    mapping-split
    mapping-catenate mapping-catenate!
    mapping-map/monotone mapping-map/monotone!
    mapping-fold/reverse
    comparator?)

  (import (scheme base)
          (scheme case-lambda)
          (schemepunk syntax)
          (schemepunk list))

  (cond-expand
    (larceny
      (import (only (schemepunk comparator) comparator?)
              (rename (srfi 146) (mapping %mapping%)
                                 (mapping=? %mapping=?%)))

      ; Larceny is missing a decent chunk of SRFI 146 methods.
      ; Also, its (mapping) is right-biased, but the SRFI requires left-biased.
      (begin
        (define (mapping comparator . args)
          (apply mapping-adjoin (cons (%mapping% comparator) args)))
        (define mapping/ordered %mapping%)
        (define mapping-unfold/ordered mapping-unfold)
        (define (mapping-adjoin mapping . args)
          (assume (mapping? mapping))
          (let loop ((args args) (mapping mapping))
            (if (null? args)
              mapping
              (receive (mapping value)
                  (mapping-intern mapping (car args) (λ () (cadr args)))
                (loop (cddr args) mapping)))))
        (define mapping-adjoin! mapping-adjoin)
        (define alist->mapping/ordered alist->mapping)
        (define alist->mapping/ordered! alist->mapping!)
        (define mapping-pop
          (case-lambda
            ((mapping)
              (mapping-pop mapping (λ ()
                (error "mapping-pop: mapping has no association"))))
            ((mapping failure)
              (assume (mapping? mapping))
              (assume (procedure? failure))
              ((call/cc
                (λ return-thunk
                  (receive (key value)
                    (mapping-find (λ (_ _) #t) mapping (λ () (return-thunk failure)))
                    (λ () (values (mapping-delete mapping key) key value)))))))))
        (define mapping-pop! mapping-pop)
        (define (mapping=? value-comparator x . xs)
          (define key-comparator (mapping-key-comparator x))
          (and (every (λ-> mapping-key-comparator (eq? key-comparator)) xs)
               (apply %mapping=?% `(,value-comparator ,x ,@xs))))))
    (sagittarius
      (import (rename (srfi 146) (mapping-intern %mapping-intern%)
                                 (mapping-intern! %mapping-intern!%)))

      ; Sagittarius's 146 is almost compliant, but its mapping-intern always
      ; returns a new mapping even when there are no changes to make.
      (begin
        (define (mapping-intern mapping key failure)
          (assume (mapping? mapping))
          (assume (procedure? failure))
          (call/cc
           (λ return
             (mapping-search mapping
               key
               (λ (insert ignore)
                 (receive (value) (failure) (insert value value)))
               (λ (old-key old-value update remove)
                 (return mapping old-value))))))
        (define mapping-intern! mapping-intern)))
    ((and (not chicken) (library (srfi 146)))
      (import (rename (srfi 146) (mapping=? %mapping=?%)))

      ; Several Schemes raise an error when comparing mappings with different
      ; comparators, even though the SRFI explicitly says this is not an error.
      ;
      ; This bug is common enough that it makes sense to patch it by default.
      (begin
        (define (mapping=? value-comparator x . xs)
          (define key-comparator (mapping-key-comparator x))
          (and (every (λ-> mapping-key-comparator (eq? key-comparator)) xs)
               (apply %mapping=?% `(,value-comparator ,x ,@xs)))))

      ; Gauche 0.9.6's tree map implementation has a typo in its comparator,
      ; so we monkey-patch it with dynamic methods.
      (cond-expand
        (gauche
          (import (only (gauche base) <tree-map> define-method object-equal?)
                  (only (gauche treeutil) tree-map-compare-as-sequences))
          (begin
            (define-method object-equal? ((a <tree-map>) (b <tree-map>))
              (zero? (tree-map-compare-as-sequences a b)))))
        (else)))

    ; Gerbil's 146 implementation is hopelessly broken; it can't even perform
    ; basic operations like mapping-set. Uncomment once this is fixed.
    ;
    ; ((and gerbil (library (std srfi 146)))
    ;   (import (std srfi 146)))

    (else
      (import (schemepunk comparator)
              (schemepunk generator))

      (include "polyfills/nieper-rbtree.scm")
      (include "polyfills/146.scm"))))
