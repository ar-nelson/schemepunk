;; Multimaps (maps from one key to multiple values),
;; implemented in terms of R7RS-Large hash tables and sets.
(define-library (schemepunk multimap)
  (export make-multimap
          multimap?
          multimap->hash-table
          multimap-copy
          multimap-ref
          multimap-add!
          multimap-delete-key!
          multimap-delete-value!
          multimap-union!
          multimap-contains?
          multimap-keys
          multimap-value-sets
          multimap-values
          multimap-key-count
          multimap-value-count)

  (import (scheme base)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk set)
          (schemepunk hash-table))

  (begin
    (define-record-type Multimap
      (make-multimap-record table key-comparator value-comparator)
      multimap?
      (table multimap->hash-table)
      (key-comparator multimap-key-comparator)
      (value-comparator multimap-value-comparator))

    (define (make-multimap key-comparator value-comparator)
      (make-multimap-record (make-hash-table key-comparator) key-comparator value-comparator))

    (define (multimap-copy mmap)
      (make-multimap-record
        (hash-table-map set-copy
                        (multimap-key-comparator mmap)
                        (multimap->hash-table mmap))
        (multimap-key-comparator mmap)
        (multimap-value-comparator mmap)))

    (define (multimap-ref mmap key)
      (hash-table-ref (multimap->hash-table mmap)
                      key
                      (λ() (set (multimap-value-comparator mmap)))))

    (define (multimap-add! mmap key value)
      (hash-table-update! (multimap->hash-table mmap)
                          key
                          (λ-> (set-adjoin! value))
                          (λ() (set (multimap-value-comparator mmap)))))

    (define (multimap-delete-key! mmap key)
      (hash-table-delete! (multimap->hash-table mmap) key))

    (define (multimap-delete-value! mmap key value)
      (set-delete! (multimap-ref mmap key) value))

    (define (multimap-union! lhs rhs)
      (hash-table-for-each
        (λ(k vs) (set-for-each (λ->> (multimap-add! lhs k)) vs))
        (multimap->hash-table rhs))
      lhs)

    (define (multimap-contains? mmap key value)
      (set-contains? (multimap-ref mmap key) value))

    (define (multimap-keys mmap)
      (hash-table-keys (multimap->hash-table mmap)))

    (define (multimap-value-sets mmap)
      (hash-table-values (multimap->hash-table mmap)))

    (define (multimap-values mmap)
      (fold (λ(x y) (set-union! y x))
            (set (multimap-value-comparator mmap))
            (multimap-value-sets mmap)))

    (define (multimap-key-count mmap)
      (hash-table-size (multimap->hash-table mmap)))

    (define (multimap-value-count mmap)
      (->> (multimap-value-sets mmap)
           (map set-size)
           (fold + 0)))))
