;; R7RS-Large hash tables (SRFI 125), with a polyfill implementation for Kawa,
;; which only has SRFI 69.
;;
;; The polyfill implementation is copied from Chibi Scheme.
;;
;; string-hash and string-ci-hash are omitted because Kawa can't handle
;; overlapping imports.
(define-library (schemepunk hash-table)
  (export
    ;; Constructors:
    make-hash-table hash-table hash-table-unfold alist->hash-table
    ;; Predicates:
    hash-table? hash-table-contains? hash-table-exists?
    hash-table-empty? hash-table=? hash-table-mutable?
    ;; Accessors:
    hash-table-ref hash-table-ref/default
    ;; Mutators:
    hash-table-set! hash-table-delete! hash-table-intern!
    hash-table-update! hash-table-update!/default hash-table-pop!
    hash-table-clear!
    ;; The whole hash table:
    hash-table-size hash-table-keys hash-table-values
    hash-table-entries hash-table-find hash-table-count
    ;; Mapping and folding:
    hash-table-map hash-table-for-each hash-table-walk
    hash-table-map! hash-table-map->list hash-table-fold hash-table-prune!
    ;; Copying and conversion:
    hash-table-copy hash-table-empty-copy hash-table->alist
    ;; Hash tables as sets:
    hash-table-union! hash-table-merge!
    hash-table-intersection! hash-table-difference! hash-table-xor!
    ;; Hash functions and reflectivity:
    hash hash-by-identity
    hash-table-equivalence-function hash-table-hash-function)

  (cond-expand
    ((library (scheme hash-table)) (import (scheme hash-table)))
    ((library (srfi 125)) (import (srfi 125)))
    ((library (std srfi 125)) (import (std srfi 125)))
    ((library (srfi 69))
       (import (scheme base)
               (except (schemepunk comparator) string-hash string-ci-hash)
               (rename (srfi 69)
                       (make-hash-table %make-hash-table)
                       (alist->hash-table %alist->hash-table)
                       (hash-table-copy %hash-table-copy)
                       (hash-table-set! %hash-table-set!)
                       (hash-table-delete! %hash-table-delete!)
                       (hash-table-fold %hash-table-fold)))
       (include "polyfills/hash.scm"))))
