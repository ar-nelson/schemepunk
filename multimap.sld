;; Multimaps (maps from one key to multiple values),
;; implemented in terms of mappings (SRFI 146) and sets (SRFI 113)
(define-library (schemepunk multimap)
  (export multimap multimap?
          multimap->mapping multimap-key-comparator multimap-value-comparator
          multimap-copy
          multimap-ref
          multimap-adjoin multimap-adjoin!
          multimap-delete-key multimap-delete-key!
          multimap-delete-value multimap-delete-value!
          multimap-union multimap-union!
          multimap-contains?
          multimap-keys
          multimap-value-sets multimap-values
          multimap-key-count multimap-value-count)

  (import (scheme base)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk comparator)
          (schemepunk set)
          (schemepunk mapping))

  (begin
    (define-record-type Multimap
      (make-multimap mapping value-comparator)
      multimap?
      (mapping multimap->mapping set-multimap-mapping!)
      (value-comparator multimap-value-comparator))

    (define (multimap key-comparator value-comparator)
      (assume (comparator? key-comparator))
      (assume (comparator? value-comparator))
      (make-multimap (mapping key-comparator) value-comparator))

    (define (multimap-key-comparator mmap)
      (mapping-key-comparator (multimap->mapping mmap)))

    (define (multimap-copy mmap)
      (assume (multimap? mmap))
      (make-multimap
        (mapping-map/monotone!
          (λ(k v) (values k (set-copy v)))
          (multimap-key-comparator mmap)
          (mapping-copy (multimap->mapping mmap)))
        (multimap-value-comparator mmap)))

    (define (multimap-ref mmap key)
      (assume (multimap? mmap))
      (mapping-ref
        (multimap->mapping mmap)
        key
        (λ() (set (multimap-value-comparator mmap)))))

    (define (multimap-adjoin mmap key value)
      (assume (multimap? mmap))
      (make-multimap
        (mapping-update (multimap->mapping mmap) key
          (cut set-adjoin <> value)
          (λ() (set (multimap-value-comparator mmap))))
        (multimap-value-comparator mmap)))

    (define (multimap-adjoin! mmap key value)
      (assume (multimap? mmap))
      (set-multimap-mapping! mmap
        (mapping-update! (multimap->mapping mmap) key
          (cut set-adjoin! <> value)
          (λ() (set (multimap-value-comparator mmap)))))
      mmap)

    (define (multimap-delete-key mmap key)
      (assume (multimap? mmap))
      (make-multimap
        (mapping-delete (multimap->mapping mmap) key)
        (multimap-value-comparator mmap)))

    (define (multimap-delete-key! mmap key)
      (assume (multimap? mmap))
      (set-multimap-mapping! mmap
        (mapping-delete! (multimap->mapping mmap) key))
      mmap)

    (define (multimap-delete-value mmap key value)
      (assume (multimap? mmap))
      (let1 m (multimap->mapping mmap)
        (mapping-ref m key
          (λ() mmap)
          (λ vs (make-multimap
                  (mapping-set m key (set-delete vs value))
                  (multimap-value-comparator mmap))))))

    (define (multimap-delete-value! mmap key value)
      (assume (multimap? mmap))
      (let1 m (multimap->mapping mmap)
        (mapping-ref m key
          (λ() mmap)
          (λ vs (set-multimap-mapping! mmap
                  (mapping-set! m key (set-delete! vs value)))
                mmap))))

    (define (multimap-union lhs rhs)
      (assume (multimap? lhs))
      (assume (multimap? rhs))
      (make-multimap
        (mapping-fold
          (λ(k vs m)
            (mapping-update m k
              (cut set-union <> vs)
              (λ() (set (multimap-value-comparator lhs)))))
          (multimap->mapping lhs)
          (multimap->mapping rhs))
        (multimap-value-comparator lhs)))

    (define (multimap-union! lhs rhs)
      (assume (multimap? lhs))
      (assume (multimap? rhs))
      (set-multimap-mapping! lhs
        (mapping-fold
          (λ(k vs m)
            (mapping-update! m k
              (cut set-union! <> vs)
              (λ() (set (multimap-value-comparator lhs)))))
          (multimap->mapping lhs)
          (multimap->mapping rhs)))
      lhs)

    (define (multimap-contains? mmap key value)
      (assume (multimap? mmap))
      (set-contains? (multimap-ref mmap key) value))

    (define (multimap-keys mmap)
      (assume (multimap? mmap))
      (mapping-keys (multimap->mapping mmap)))

    (define (multimap-value-sets mmap)
      (assume (multimap? mmap))
      (mapping-values (multimap->mapping mmap)))

    (define (multimap-values mmap)
      (assume (multimap? mmap))
      (fold (λ(x y) (set-union! y x))
            (set (multimap-value-comparator mmap))
            (multimap-value-sets mmap)))

    (define (multimap-key-count mmap)
      (assume (multimap? mmap))
      (mapping-size (multimap->mapping mmap)))

    (define (multimap-value-count mmap)
      (assume (multimap? mmap))
      (->> (multimap-value-sets mmap)
           (map set-size)
           (fold + 0)))))
