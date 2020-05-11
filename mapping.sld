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
    mapping-min-entry mapping-max-entry
    mapping-key-predecessor mapping-key-successor
    mapping-range= mapping-range< mapping-range> mapping-range<= mapping-range>=
    mapping-range=! mapping-range<! mapping-range>! mapping-range<=! mapping-range>=!
    mapping-split mapping-split!
    mapping-catenate mapping-catenate!
    mapping-map/monotone mapping-map/monotone!
    mapping-fold/reverse
    comparator?)

  (import (scheme base)
          (scheme case-lambda)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk comparator)
          (schemepunk debug indent)
          (schemepunk debug indent scheme))

  (cond-expand
    (gauche
      ; Gauche is the only R7RS whose 146 is faster than (schemepunk btree),
      ; though it cheats by using a HAMT, making (srfi 146) = (srfi 146 hash).
      (import (rename (srfi 146) (mapping=? %mapping=?)
                                 (make-mapping-comparator %make-mapping-comparator)
                                 (mapping-comparator %mapping-comparator))
              (only (gauche base) <tree-map> define-method object-equal?)
              (only (gauche treeutil) tree-map-compare-as-sequences))

      (begin
        ; Gauche raises an error comparing mappings with different comparators,
        ; even though the SRFI explicitly says this is not an error.
        (define (mapping=? value-comparator x . xs)
          (define key-comparator (mapping-key-comparator x))
          (and (every (λ-> mapping-key-comparator (eq? key-comparator)) xs)
               (apply %mapping=? `(,value-comparator ,x ,@xs))))

        ; Gauche 0.9.6's tree map implementation has a typo in its comparator,
        ; so we monkey-patch it with dynamic methods.
        (define-method object-equal? ((a <tree-map>) (b <tree-map>))
          (zero? (tree-map-compare-as-sequences a b)))

        ; Gauche tree maps are not hashable, and so can't be put in sets.
        ; This is a problem for Datalog, so we add a hashing function.
        (define (mapping-hash value-comparator m)
          (define key-comparator (mapping-key-comparator m))
          (mapping-fold (λ(k v h)
                          (-> (modulo (* h 33) (hash-bound))
                              (+ (comparator-hash key-comparator k))
                              (* 33)
                              (modulo (hash-bound))
                              (+ (comparator-hash value-comparator v))))
                        (hash-salt)
                        m))

        (define (make-mapping-comparator value-cmpr)
          (make-comparator
            mapping?
            (cut mapping=? value-cmpr <> <>)
            (cut tree-map-compare-as-sequences <> <> value-cmpr)
            (cut mapping-hash value-cmpr <>)))

        (define mapping-comparator (make-mapping-comparator (make-default-comparator)))))
    (else
      (import (schemepunk btree))

      (begin
        (define *not-found* (list 'not-found))

        (define (empty-mapping comparator)
          (assume (comparator? comparator))
          ; According to benchmarks, higher-degree B-trees are faster
          ; in every Scheme except Kawa.
          (btree comparator (cond-expand (kawa 3) (else 7))))

        (define (mapping comparator . args)
          (fold-right-in-pairs (λ(k v m) (btree-set! m k v))
                               (empty-mapping comparator)
                               args))

        ;; Constructors

        (define (mapping-unfold stop? mapper successor seed comparator)
          (assume (procedure? stop?))
          (assume (procedure? mapper))
          (assume (procedure? successor))
          (let loop ((mapping (empty-mapping comparator))
                     (seed seed))
            (if (stop? seed)
                mapping
                (let1-values (k v) (mapper seed)
                  (loop (btree-set! mapping k v)
                        (successor seed))))))

        ;; Predicates

        (define mapping? btree?)

        (define (mapping-empty? mapping)
          (assume (mapping? mapping))
          (btree-empty? mapping))

        (define (mapping-contains? mapping key)
          (assume (mapping? mapping))
          (not (eq? *not-found* (btree-ref mapping key (λ () *not-found*)))))

        (define (mapping-disjoint? mapping1 mapping2)
          (assume (mapping? mapping1))
          (assume (mapping? mapping2))
          (call/cc
            (λ return
              (btree-fold (λ((k . _) _)
                             (when (mapping-contains? mapping2 k)
                               (return #f))
                             #f)
                          #f
                          mapping1)
              #t)))

        ;; Accessors

        (define mapping-ref
          (case-lambda
            ((mapping key)
              (assume (mapping? mapping))
              (btree-ref mapping key (λ ()
                (error "mapping-ref: key not in mapping" key))))
            ((mapping key failure)
              (assume (mapping? mapping))
              (assume (procedure? failure))
              (btree-ref mapping key failure))
            ((mapping key failure success)
              (assume (mapping? mapping))
              (assume (procedure? failure))
              (assume (procedure? success))
              (let1 result (btree-ref mapping key (λ () *not-found*))
                (if (eq? result *not-found*) (failure) (success result))))))

        (define (mapping-ref/default mapping key default)
          (mapping-ref mapping key (λ () default)))

        (define mapping-key-comparator btree-key-comparator)

        ;; Updaters

        (define (mapping-set mapping . args)
          (assume (mapping? mapping))
          (fold-in-pairs (λ(k v m) (btree-set m k v)) mapping args))

        (define (mapping-set! mapping . args)
          (assume (mapping? mapping))
          (fold-in-pairs (λ(k v m) (btree-set! m k v)) mapping args))

        (define (mapping-adjoin mapping . args)
          (assume (mapping? mapping))
          (fold-in-pairs
            (λ(k v m) (if (mapping-contains? m k) m (btree-set m k v)))
            mapping
            args))

        (define (mapping-adjoin! mapping . args)
          (assume (mapping? mapping))
          (fold-in-pairs
            (λ(k v m) (if (mapping-contains? m k) m (btree-set! m k v)))
            mapping
            args))

        (define (mapping-replace mapping k v)
          (assume (mapping? mapping))
          (if (mapping-contains? mapping k) (btree-set mapping k v) mapping))

        (define (mapping-replace! mapping k v)
          (assume (mapping? mapping))
          (if (mapping-contains? mapping k) (btree-set! mapping k v) mapping))

        (define (mapping-delete mapping . keys)
          (mapping-delete-all mapping keys))

        (define (mapping-delete! mapping . keys)
          (mapping-delete-all! mapping keys))

        (define (mapping-delete-all mapping keys)
          (assume (mapping? mapping))
          (assume (list? keys))
          (fold (λ(k m) (btree-delete m k)) mapping keys))

        (define (mapping-delete-all! mapping keys)
          (assume (mapping? mapping))
          (assume (list? keys))
          (fold (λ(k m) (btree-delete! m k)) mapping keys))

        (define (mapping-intern mapping key failure)
          (assume (procedure? failure))
          (mapping-ref mapping key
            (λ () (let1 value (failure)
                    (values (btree-set mapping key value) value)))
            (λ value (values mapping value))))

        (define (mapping-intern! mapping key failure)
          (assume (procedure? failure))
          (mapping-ref mapping key
            (λ () (let1 value (failure)
                    (values (btree-set! mapping key value) value)))
            (λ value (values mapping value))))

        (define mapping-update
          (case-lambda
            ((mapping key updater)
              (assume (procedure? updater))
              (mapping-ref mapping key
                (λ () (error "mapping-update: key not in mapping" key))
                (λ->> updater (btree-set mapping key))))
            ((mapping key updater failure)
              (assume (procedure? updater))
              (assume (procedure? failure))
              (mapping-ref mapping key
                (λ () (btree-set mapping key (updater (failure))))
                (λ->> updater (btree-set mapping key))))
            ((mapping key updater failure success)
              (assume (procedure? updater))
              (assume (procedure? failure))
              (assume (procedure? success))
              (mapping-ref mapping key
                (λ () (btree-set mapping key (updater (failure))))
                (λ->> success updater (btree-set mapping key))))))

        (define mapping-update!
          (case-lambda
            ((mapping key updater)
              (assume (procedure? updater))
              (mapping-ref mapping key
                (λ () (error "mapping-update!: key not in mapping" key))
                (λ->> updater (btree-set! mapping key))))
            ((mapping key updater failure)
              (assume (procedure? updater))
              (assume (procedure? failure))
              (mapping-ref mapping key
                (λ () (btree-set! mapping key (updater (failure))))
                (λ->> updater (btree-set! mapping key))))
            ((mapping key updater failure success)
              (assume (procedure? updater))
              (assume (procedure? failure))
              (assume (procedure? success))
              (mapping-ref mapping key
                (λ () (btree-set! mapping key (updater (failure))))
                (λ->> success updater (btree-set! mapping key))))))

        (define (mapping-update/default mapping key updater default)
          (mapping-update mapping key updater (λ () default)))

        (define (mapping-update!/default mapping key updater default)
          (mapping-update! mapping key updater (λ () default)))

        (define mapping-pop
          (case-lambda
            ((mapping)
              (mapping-pop mapping (λ () (error "mapping-pop: mapping is empty"))))
            ((mapping failure)
              (assume (mapping? mapping))
              (assume (procedure? failure))
              (let1-values (popped new-mapping) (btree-pop mapping)
                (if popped
                  (values new-mapping (car popped) (cdr popped))
                  (failure))))))

        (define mapping-pop!
          (case-lambda
            ((mapping)
              (mapping-pop! mapping (λ () (error "mapping-pop!: mapping is empty"))))
            ((mapping failure)
              (assume (mapping? mapping))
              (assume (procedure? failure))
              (match (btree-pop! mapping)
                ((k . v) (values mapping k v))
                (else (failure))))))

        (define (mapping-search mapping key failure success)
          (define (insert value obj)
            (values (btree-set mapping key value) obj))
          (define (ignore obj)
            (values mapping obj))
          (define (update new-key new-value obj)
            (-> (if (=? (mapping-key-comparator mapping) key new-key)
                  mapping
                  (btree-delete mapping key))
                (btree-set new-key new-value)
                (values obj)))
          (define (remove obj)
            (values (btree-delete mapping key) obj))
          (assume (procedure? failure))
          (assume (procedure? success))
          (mapping-ref mapping key
            (λ () (failure insert ignore))
            (λ value (success key value update remove))))

        (define (mapping-search! mapping key failure success)
          (define (insert value obj)
            (values (btree-set! mapping key value) obj))
          (define (ignore obj)
            (values mapping obj))
          (define (update new-key new-value obj)
            (-> (if (=? (mapping-key-comparator mapping) key new-key)
                  mapping
                  (btree-delete! mapping key))
                (btree-set! new-key new-value)
                (values obj)))
          (define (remove obj)
            (values (btree-delete! mapping key) obj))
          (assume (procedure? failure))
          (assume (procedure? success))
          (mapping-ref mapping key
            (λ () (failure insert ignore))
            (λ value (success key value update remove))))

        ;; The whole mapping

        (define (mapping-size mapping)
          (assume (mapping? mapping))
          (btree-fold (λ (_ n) (+ n 1)) 0 mapping))

        (define (mapping-find predicate? mapping failure)
          (assume (mapping? mapping))
          (assume (procedure? predicate?))
          (assume (procedure? failure))
          (call/cc
            (λ return
              (btree-fold (λ((k . v) _)
                            (when (predicate? k v) (return k v))
                            #f)
                          #f
                          mapping)
              (failure))))

        (define (mapping-count predicate? mapping)
          (assume (mapping? mapping))
          (assume (procedure? predicate?))
          (btree-fold (λ((k . v) n) (if (predicate? k v) (+ n 1) n))
                      0
                      mapping))

        (define (mapping-any? predicate? mapping)
          (assume (mapping? mapping))
          (assume (procedure? predicate?))
          (call/cc
            (λ return
              (btree-fold (λ((k . v) _) (when (predicate? k v) (return #t)) #f)
                          #f
                          mapping))))

        (define (mapping-every? predicate? mapping)
          (assume (mapping? mapping))
          (assume (procedure? predicate?))
          (call/cc
            (λ return
              (btree-fold (λ((k . v) _) (unless (predicate? k v) (return #f)) #t)
                          #t
                          mapping))))

        (define (mapping-keys mapping)
          (assume (mapping? mapping))
          (btree-fold-right (λ((k . _) ks) (cons k ks)) '() mapping))

        (define (mapping-values mapping)
          (assume (mapping? mapping))
          (btree-fold-right (λ((_ . v) vs) (cons v vs)) '() mapping))

        (define (mapping-entries mapping)
          (assume (mapping? mapping))
          (car+cdr
            (btree-fold-right (λ((k . v) (ks . vs)) `((,k ,@ks) . (,v ,@vs)))
                              '(() . ())
                              mapping)))

        ;; Mapping and folding

        (define (mapping-map proc comparator mapping)
          (assume (procedure? proc))
          (assume (comparator? comparator))
          (assume (mapping? mapping))
          (btree-fold (λ((k . v) m) (let1-values (k2 v2) (proc k v)
                                      (btree-set! m k2 v2)))
                      (empty-mapping comparator)
                      mapping))

        (define (mapping-for-each proc mapping)
          (assume (procedure? proc))
          (assume (mapping? mapping))
          (btree-fold (λ((k . v) _) (proc k v) #f) #f mapping))

        (define (mapping-fold proc nil mapping)
          (assume (procedure? proc))
          (assume (mapping? mapping))
          (btree-fold (λ((k . v) m) (proc k v m)) nil mapping))

        (define (mapping-map->list proc mapping)
          (assume (procedure? proc))
          (assume (mapping? mapping))
          (reverse
            (btree-fold (λ((k . v) xs) (cons (proc k v) xs))
                        '()
                        mapping)))

        (define (mapping-filter predicate? mapping)
          (assume (mapping? mapping))
          (assume (procedure? predicate?))
          (btree-fold (λ((k . v) m) (if (predicate? k v) m (btree-delete m k)))
                      mapping
                      mapping))

        (define (mapping-remove predicate? mapping)
          (assume (procedure? predicate?))
          (mapping-filter (λ(k v) (not (predicate? k v))) mapping))

        (define (mapping-partition predicate? mapping)
          (assume (mapping? mapping))
          (assume (procedure? predicate?))
          (let1 comparator (mapping-key-comparator mapping)
            (car+cdr
              (btree-fold (λ((k . v) (yes . no))
                            (if (predicate? k v)
                              (cons (btree-set! yes k v) no)
                              (cons yes (btree-set! no k v))))
                          (cons (empty-mapping comparator)
                                (empty-mapping comparator))
                          mapping))))

        ; mapping-filter!, mapping-remove!, and mapping-partition!
        ; cannot be usefully implemented in a linear-update fashion,
        ; because mappings cannot be mutated while being iterated over.
        (define mapping-filter! mapping-filter)
        (define mapping-remove! mapping-remove)
        (define mapping-partition! mapping-partition)

        ;; Copying and conversion

        (define (mapping-copy mapping)
          (assume (mapping? mapping))
          (btree-copy mapping))

        (define (mapping->alist mapping)
          (assume (mapping? mapping))
          (btree->alist mapping))

        (define (alist->mapping comparator alist)
          (assume (comparator? comparator))
          (alist->mapping! (empty-mapping comparator) alist))

        (define (alist->mapping! mapping alist)
          (assume (mapping? mapping))
          (assume (list? alist))
          (fold (λ((k . v) m) (btree-set! m k v)) mapping alist))

        ;; Submappings

        (define mapping=?
          (case-lambda
            ((comparator mapping1 mapping2)
              (assume (comparator? comparator))
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (btree=? comparator mapping1 mapping2))
            ((comparator mapping1 mapping2 . rest)
              (assume (comparator? comparator))
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (and (btree=? comparator mapping1 mapping2)
                   (apply mapping=? `(,comparator ,mapping2 ,@rest))))))

        (define mapping<?
          (case-lambda
            ((comparator mapping1 mapping2)
              (assume (comparator? comparator))
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (and (eq? (mapping-key-comparator mapping1)
                        (mapping-key-comparator mapping2))
                   (not (= (mapping-size mapping1) (mapping-size mapping2)))
                   (btree-subset? comparator mapping1 mapping2)))
            ((comparator mapping1 mapping2 . rest)
              (and (mapping<? comparator mapping1 mapping2)
                   (apply mapping<? `(,comparator ,mapping2 ,@rest))))))

        (define mapping>?
          (case-lambda
            ((comparator mapping1 mapping2)
              (assume (comparator? comparator))
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (and (eq? (mapping-key-comparator mapping1)
                        (mapping-key-comparator mapping2))
                   (not (= (mapping-size mapping1) (mapping-size mapping2)))
                   (btree-subset? comparator mapping2 mapping1)))
            ((comparator mapping1 mapping2 . rest)
              (and (mapping>? comparator mapping1 mapping2)
                   (apply mapping>? `(,comparator ,mapping2 ,@rest))))))

        (define mapping<=?
          (case-lambda
            ((comparator mapping1 mapping2)
              (assume (comparator? comparator))
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (and (eq? (mapping-key-comparator mapping1)
                        (mapping-key-comparator mapping2))
                   (btree-subset? comparator mapping1 mapping2)))
            ((comparator mapping1 mapping2 . rest)
              (and (mapping<=? comparator mapping1 mapping2)
                   (apply mapping<=? `(,comparator ,mapping2 ,@rest))))))

        (define mapping>=?
          (case-lambda
            ((comparator mapping1 mapping2)
              (assume (comparator? comparator))
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (and (eq? (mapping-key-comparator mapping1)
                        (mapping-key-comparator mapping2))
                   (btree-subset? comparator mapping2 mapping1)))
            ((comparator mapping1 mapping2 . rest)
              (and (mapping>=? comparator mapping1 mapping2)
                   (apply mapping>=? `(,comparator ,mapping2 ,@rest))))))

        ;; Set theory operations

        (define (%mapping-union% x y)
          (btree-fold (λ((k . v) m) (btree-set m k v)) y x))

        (define (%mapping-union!% x y)
          (btree-fold (λ((k . v) m) (btree-set! m k v)) y x))

        (define (%mapping-intersection% x y)
          (mapping-filter (λ(k _) (mapping-contains? y k)) x))

        (define (%mapping-difference% x y)
          (mapping-filter (λ(k _) (not (mapping-contains? y k))) x))

        (define (%mapping-xor% x y)
          (%mapping-union% (%mapping-difference% x y)
                           (%mapping-difference% y x)))

        (define (%mapping-xor!% x y)
          (%mapping-union!% (%mapping-difference% x y)
                            (%mapping-difference% y x)))

        (define mapping-union
          (case-lambda
            ((mapping1) mapping1)
            ((mapping1 mapping2)
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (%mapping-union% mapping1 mapping2))
            ((mapping1 mapping2 . rest)
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (apply mapping-union
                (cons (%mapping-union% mapping1 mapping2) rest)))))

        (define mapping-union!
          (case-lambda
            ((mapping1) mapping1)
            ((mapping1 mapping2)
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (%mapping-union!% mapping1 mapping2))
            ((mapping1 mapping2 . rest)
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (apply mapping-union!
                (cons (%mapping-union!% mapping1 mapping2) rest)))))

        (define mapping-intersection
          (case-lambda
            ((mapping1) mapping1)
            ((mapping1 mapping2)
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (%mapping-intersection% mapping1 mapping2))
            ((mapping1 mapping2 . rest)
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (apply mapping-intersection
                (cons (%mapping-intersection% mapping1 mapping2) rest)))))

        (define mapping-intersection! mapping-intersection)

        (define mapping-difference
          (case-lambda
            ((mapping1) mapping1)
            ((mapping1 mapping2)
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (%mapping-difference% mapping1 mapping2))
            ((mapping1 mapping2 . rest)
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (apply mapping-difference
                (cons (%mapping-difference% mapping1 mapping2) rest)))))

        (define mapping-difference! mapping-difference)

        (define mapping-xor
          (case-lambda
            ((mapping1) mapping1)
            ((mapping1 mapping2)
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (%mapping-xor% mapping1 mapping2))
            ((mapping1 mapping2 . rest)
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (apply mapping-xor
                (cons (%mapping-xor% mapping1 mapping2) rest)))))

        (define mapping-xor!
          (case-lambda
            ((mapping1) mapping1)
            ((mapping1 mapping2)
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (%mapping-xor!% mapping1 mapping2))
            ((mapping1 mapping2 . rest)
              (assume (mapping? mapping1))
              (assume (mapping? mapping2))
              (apply mapping-xor!
                (cons (%mapping-xor!% mapping1 mapping2) rest)))))

        ;; Additional procedures for mappings with ordered keys

        (define (mapping-min-key mapping)
          (assume (mapping? mapping))
          (call/cc
            (λ return
              (btree-fold (λ((k . _) _) (return k)) #f mapping)
              (error "mapping-min-key: mapping is empty"))))

        (define (mapping-max-key mapping)
          (assume (mapping? mapping))
          (call/cc
            (λ return
              (btree-fold-right (λ((k . _) _) (return k)) #f mapping)
              (error "mapping-max-key: mapping is empty"))))

        (define (mapping-min-value mapping)
          (assume (mapping? mapping))
          (call/cc
            (λ return
              (btree-fold (λ((_ . v) _) (return v)) #f mapping)
              (error "mapping-min-value: mapping is empty"))))

        (define (mapping-max-value mapping)
          (assume (mapping? mapping))
          (call/cc
            (λ return
              (btree-fold-right (λ((_ . v) _) (return v)) #f mapping)
              (error "mapping-max-value: mapping is empty"))))

        (define (mapping-min-entry mapping)
          (assume (mapping? mapping))
          (car+cdr
            (call/cc
              (λ return
                (btree-fold (λ(e _) (return e)) #f mapping)
                (error "mapping-min-entry: mapping is empty")))))

        (define (mapping-max-entry mapping)
          (assume (mapping? mapping))
          (car+cdr
            (call/cc
              (λ return
                (btree-fold-right (λ(e _) (return e)) #f mapping)
                (error "mapping-max-entry: mapping is empty")))))

        (define (mapping-key-predecessor mapping obj failure)
          (assume (mapping? mapping))
          (assume (procedure? failure))
          (let1 comparator (mapping-key-comparator mapping)
            (call/cc
              (λ return
                (btree-fold-right (λ((k . _) found?)
                                    (if (<? comparator k obj) (return k) #f))
                                  #f
                                  mapping)
                (failure)))))

        (define (mapping-key-successor mapping obj failure)
          (assume (mapping? mapping))
          (assume (procedure? failure))
          (let1 comparator (mapping-key-comparator mapping)
            (call/cc
              (λ return
                (btree-fold (λ((k . _) found?)
                              (if (<? comparator obj k) (return k) #f))
                            #f
                            mapping)
                (failure)))))

        (define (mapping-range= mapping obj)
          (mapping-ref mapping obj
            (λ () (empty-mapping (mapping-key-comparator mapping)))
            (λ value (btree-set! (empty-mapping (mapping-key-comparator mapping))
                                 obj
                                 value))))

        (define (mapping-range< mapping obj)
          (assume (mapping? mapping))
          (let1 <? (comparator-ordering-predicate (mapping-key-comparator mapping))
            (mapping-filter (λ(k _) (<? k obj)) mapping)))

        (define (mapping-range> mapping obj)
          (assume (mapping? mapping))
          (let1 <? (comparator-ordering-predicate (mapping-key-comparator mapping))
            (mapping-filter (λ(k _) (<? obj k)) mapping)))

        (define (mapping-range<= mapping obj)
          (assume (mapping? mapping))
          (let1 <? (comparator-ordering-predicate (mapping-key-comparator mapping))
            (mapping-filter (λ(k _) (not (<? obj k))) mapping)))

        (define (mapping-range>= mapping obj)
          (assume (mapping? mapping))
          (let1 <? (comparator-ordering-predicate (mapping-key-comparator mapping))
            (mapping-filter (λ(k _) (not (<? k obj))) mapping)))

        (define (mapping-fold/reverse proc nil mapping)
          (assume (procedure? proc))
          (assume (mapping? mapping))
          (btree-fold-right (λ((k . v) m) (proc k v m)) nil mapping))

        (define (mapping-split mapping obj)
          (values
            (mapping-range< mapping obj)
            (mapping-range<= mapping obj)
            (mapping-range= mapping obj)
            (mapping-range>= mapping obj)
            (mapping-range> mapping obj)))

        ; This is a placeholder implementation of mapping-catenate that doesn't
        ; check for monotonicity or provide any performance over mapping-union.
        (define (mapping-catenate comparator mapping1 key value mapping2)
          (assume (comparator? comparator))
          (assume (mapping? mapping1))
          (assume (mapping? mapping2))
          (%mapping-union% (btree-set mapping1 key value)
                           mapping2))

        (define (mapping-catenate! comparator mapping1 key value mapping2)
          (assume (comparator? comparator))
          (assume (mapping? mapping1))
          (assume (mapping? mapping2))
          (%mapping-union!% (btree-set! mapping1 key value)
                            mapping2))

        ; TODO: These can be implemented more efficiently
        (define mapping/ordered mapping)
        (define mapping-unfold/ordered mapping-unfold)
        (define alist->mapping/ordered alist->mapping)
        (define alist->mapping/ordered! alist->mapping!)
        (define mapping-range=! mapping-range=)
        (define mapping-range<! mapping-range<)
        (define mapping-range>! mapping-range>)
        (define mapping-range<=! mapping-range<=)
        (define mapping-range>=! mapping-range>=)
        (define mapping-split! mapping-split)
        (define mapping-map/monotone mapping-map)
        (define mapping-map/monotone! mapping-map)

        ;; Comparators

        (define make-mapping-comparator make-btree-comparator)
        (define mapping-comparator btree-comparator))))

  (begin
    (define (mapping->indent mapping)
      (make-indent-group
        (color (color-scheme-structure) "#<mapping {")
        (map (λ x (make-indent-group
                    (make-indent-group
                      #f
                      (list (form->indent (car x)))
                      (color (color-scheme-structure) ":"))
                    (list (form->indent (cdr x)))
                    #f))
             (mapping->alist mapping))
        (color (color-scheme-structure) "}>")))

    (register-datatype-debug-writer! mapping? mapping->indent)))
