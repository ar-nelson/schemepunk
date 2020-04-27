(import (scheme base)
        (scheme case-lambda)
        (scheme time)
        (schemepunk syntax)
        (schemepunk list)
        (schemepunk random)
        (schemepunk hash-table)
        (schemepunk comparator)
        (schemepunk generator)
        (schemepunk btree))

(cond-expand
  (chibi
    (include "tests/old-mapping/nieper-rbtree.scm")
    (include "tests/old-mapping/146.scm"))
  (else
    (include "./old-mapping/nieper-rbtree.scm")
    (include "./old-mapping/146.scm")))

(define comparator (make-default-comparator))
(define table-10k (make-hash-table comparator))
(define table-100 (make-hash-table comparator))

(dotimes 10000
  (hash-table-set! table-10k (random-integer 1000000) (random-integer 1000000)))

(dotimes 100
  (hash-table-set! table-100 (random-integer 10000) (random-integer 10000)))

(define alist-10k (hash-table->alist table-10k))
(define alist-100 (hash-table->alist table-100))

(format #t "Mapping - insert 10000 elements~%")
(define start (current-jiffy))
(define mapping-10k
  (hash-table-fold
    (λ (k v m) (mapping-set m k v))
    (mapping comparator)
    table-10k))
(format #t "~a~%~%" (- (current-jiffy) start))

(format #t "Btree size 6 - insert 10000 elements~%")
(set! start (current-jiffy))
(define tree6
  (hash-table-fold
    (λ (k v tree) (btree-set tree k v))
    (btree comparator 6)
    table-10k))
(format #t "~a~%~%" (- (current-jiffy) start))

(format #t "Btree size 3 - insert 10000 elements~%")
(set! start (current-jiffy))
(define tree3
  (hash-table-fold
    (λ (k v tree) (btree-set tree k v))
    (btree comparator 3)
    table-10k))
(format #t "~a~%~%" (- (current-jiffy) start))

(format #t "Mutable btree size 6 - insert 10000 elements~%")
(set! start (current-jiffy))
(define mtree6
  (hash-table-fold
    (λ (k v tree) (btree-set! tree k v))
    (btree comparator 6)
    table-10k))
(format #t "~a~%~%" (- (current-jiffy) start))

(format #t "Mutable btree size 3 - insert 10000 elements~%")
(set! start (current-jiffy))
(define mtree3
  (hash-table-fold
    (λ (k v tree) (btree-set! tree k v))
    (btree comparator 3)
    table-10k))
(format #t "~a~%~%" (- (current-jiffy) start))

(format #t "Mapping - insert 100 elements 100 times~%")
(set! start (current-jiffy))
(dotimes 100
  (hash-table-fold
    (λ (k v m) (mapping-set m k v))
    (mapping comparator)
    table-100))
(format #t "~a~%~%" (- (current-jiffy) start))

(format #t "Btree size 6 - insert 100 elements 100 times~%")
(set! start (current-jiffy))
(dotimes 100
  (hash-table-fold
    (λ (k v m) (btree-set m k v))
    (btree comparator 6)
    table-100))
(format #t "~a~%~%" (- (current-jiffy) start))

(format #t "Btree size 3 - insert 100 elements 100 times~%")
(set! start (current-jiffy))
(dotimes 100
  (hash-table-fold
    (λ (k v m) (btree-set m k v))
    (btree comparator 3)
    table-100))
(format #t "~a~%~%" (- (current-jiffy) start))

(format #t "Mapping - lookup 10000 elements~%")
(set! start (current-jiffy))
(hash-table-for-each
  (λ (k v) (assume (= v (mapping-ref mapping-10k k))
                   (list v (mapping-ref mapping-10k k))))
  table-10k)
(format #t "~a~%~%" (- (current-jiffy) start))

(format #t "Btree size 6 - lookup 10000 elements~%")
(set! start (current-jiffy))
(hash-table-for-each
  (λ (k v) (assume (= v (btree-ref tree6 k))
                   (list v (btree-ref tree6 k))))
  table-10k)
(format #t "~a~%~%" (- (current-jiffy) start))

(format #t "Btree size 3 - lookup 10000 elements~%")
(set! start (current-jiffy))
(hash-table-for-each
  (λ (k v) (assume (= v (btree-ref tree3 k))
                   (list v (btree-ref tree3 k))))
  table-10k)
(format #t "~a~%~%" (- (current-jiffy) start))

(define table-deleted (make-hash-table comparator))
(dotimes 4000
  (let1-values (k v) (hash-table-pop! table-10k)
    (hash-table-set! table-deleted k v)))

(format #t "Mapping - delete 4000 elements~%")
(set! start (current-jiffy))
(define mapping-10k-deleted
  (hash-table-fold
    (λ (k v m) (mapping-delete m k))
    mapping-10k
    table-deleted))
(format #t "~a~%~%" (- (current-jiffy) start))

(hash-table-for-each
  (λ (k v) (assume (not (mapping-contains? mapping-10k-deleted k))))
  table-deleted)
(hash-table-for-each
  (λ (k v) (assume (mapping-contains? mapping-10k-deleted k)))
  table-10k)

(format #t "Btree size 6 - delete 4000 elements~%")
(set! start (current-jiffy))
(define tree6-deleted
  (hash-table-fold
    (λ (k v t) (btree-delete t k))
    tree6
    table-deleted))
(format #t "~a~%~%" (- (current-jiffy) start))

(hash-table-for-each
  (λ (k v) (assume (not (btree-ref tree6-deleted k (λ () #f)))))
  table-deleted)
(hash-table-for-each
  (λ (k v) (assume (btree-ref tree6-deleted k (λ () #f))))
  table-10k)

(format #t "Btree size 3 - delete 4000 elements~%")
(set! start (current-jiffy))
(define tree3-deleted
  (hash-table-fold
    (λ (k v t) (btree-delete t k))
    tree3
    table-deleted))
(format #t "~a~%~%" (- (current-jiffy) start))

(hash-table-for-each
  (λ (k v) (assume (not (btree-ref tree3-deleted k (λ () #f)))))
  table-deleted)
(hash-table-for-each
  (λ (k v) (assume (btree-ref tree3-deleted k (λ () #f))))
  table-10k)
