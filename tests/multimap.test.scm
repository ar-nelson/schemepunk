(import (scheme base)
        (schemepunk syntax)
        (schemepunk comparator)
        (schemepunk set)
        (schemepunk multimap)
        (schemepunk test))

(test-group "Multimap"
  (define cmp (make-default-comparator))
  (define (make-mmap) (multimap cmp cmp))

  (test "create multimap"
    (assert-true (multimap? (make-mmap))))

  (test "insert and get single multimap item"
    (define mmap (make-mmap))
    (multimap-adjoin! mmap "foo" "bar")
    (assert-true (set? (multimap-ref mmap "foo")))
    (assert-equal (set->list (multimap-ref mmap "foo")) '("bar")))

  (test "insert and get multiple values for same key"
    (define mmap (make-mmap))
    (multimap-adjoin! mmap "foo" "bar")
    (multimap-adjoin! mmap "foo" "baz")
    (multimap-adjoin! mmap "foo" "qux")
    (assert-equal (set-size (multimap-ref mmap "foo")) 3))

  (test "insert and get multiple values with adjoin-set"
    (define mmap (make-mmap))
    (multimap-adjoin! mmap "foo" "foo")
    (multimap-adjoin-set! mmap "foo" (set cmp "bar" "baz" "qux"))
    (assert-equal (set-size (multimap-ref mmap "foo")) 4))

  (test "insert and get multiple keys"
    (define mmap (make-mmap))
    (multimap-adjoin! mmap "foo" "bar")
    (multimap-adjoin! mmap "baz" "qux")
    (assert-equal (set->list (multimap-ref mmap "foo")) '("bar"))
    (assert-equal (set->list (multimap-ref mmap "baz")) '("qux")))

  (test "insert and get single multimap item (immutable)"
    (define mmap (multimap-adjoin (make-mmap) "foo" "bar"))
    (assert-true (set? (multimap-ref mmap "foo")))
    (assert-equal (set->list (multimap-ref mmap "foo")) '("bar")))

  (test "insert and get multiple values for same key (immutable)"
    (define mmap (chain (make-mmap)
      (multimap-adjoin! <> "foo" "bar")
      (multimap-adjoin! <> "foo" "baz")
      (multimap-adjoin! <> "foo" "qux")))
    (assert-equal (set-size (multimap-ref mmap "foo")) 3))

  (test "insert and get multiple values with adjoin-set (immutable)"
    (define mmap (chain (make-mmap)
      (multimap-adjoin-set <> "foo" (set cmp "bar" "baz" "qux"))))
    (assert-equal (set-size (multimap-ref mmap "foo")) 3))

  (test "insert and get multiple keys (immutable)"
    (define mmap (chain (make-mmap)
      (multimap-adjoin! <> "foo" "bar")
      (multimap-adjoin! <> "baz" "qux")))
    (assert-equal (set->list (multimap-ref mmap "foo")) '("bar"))
    (assert-equal (set->list (multimap-ref mmap "baz")) '("qux")))

  (test "delete individual value"
    (define mmap (make-mmap))
    (multimap-adjoin! mmap "foo" "bar")
    (multimap-adjoin! mmap "foo" "baz")
    (multimap-delete-value! mmap "foo" "bar")
    (multimap-delete-value! mmap "foo" "bar")
    (multimap-delete-value! mmap "foo" "qux")
    (assert-equal (set->list (multimap-ref mmap "foo")) '("baz")))

  (test "delete entire key"
    (define mmap (make-mmap))
    (multimap-adjoin! mmap "foo" "bar")
    (multimap-adjoin! mmap "foo" "baz")
    (multimap-delete-key! mmap "foo")
    (assert-true (set-empty? (multimap-ref mmap "foo"))))

  (test "delete individual value (immutable)"
    (define mmap (chain (make-mmap)
      (multimap-adjoin <> "foo" "bar")
      (multimap-adjoin <> "foo" "baz")
      (multimap-delete-value <> "foo" "bar")
      (multimap-delete-value <> "foo" "bar")
      (multimap-delete-value <> "foo" "qux")))
    (assert-equal (set->list (multimap-ref mmap "foo")) '("baz")))

  (test "delete entire key (immutable)"
    (define mmap (chain (make-mmap)
      (multimap-adjoin <> "foo" "bar")
      (multimap-adjoin <> "foo" "baz")
      (multimap-delete-key <> "foo")))
    (assert-true (set-empty? (multimap-ref mmap "foo"))))

  (test "empty keys return an empty set"
    (define mmap (make-mmap))
    (assert-true (set? (multimap-ref mmap "foo")))
    (assert-true (set-empty? (multimap-ref mmap "foo"))))

  (test "duplicate values are ignored"
    (define mmap (make-mmap))
    (multimap-adjoin! mmap "foo" "bar")
    (multimap-adjoin! mmap "foo" "baz")
    (multimap-adjoin! mmap "foo" "bar")
    (assert-equal (set-size (multimap-ref mmap "foo")) 2))

  (test "copy creates a new map"
    (define original (make-mmap))
    (multimap-adjoin! original "foo" "bar")
    (define copy (multimap-copy original))
    (assert-false (eq? original copy))
    (assert-false (eq? (multimap->mapping original) (multimap->mapping copy)))
    (assert-false (eq? (multimap-ref original "foo") (multimap-ref copy "foo")))
    (multimap-adjoin! copy "foo" "baz")
    (assert-equal (set-size (multimap-ref copy "foo")) 2)
    (assert-equal (set-size (multimap-ref original "foo")) 1))

  (test "union of maps"
    (define a (make-mmap))
    (define b (make-mmap))
    (multimap-adjoin! a "foo" "bar")
    (multimap-adjoin! a "foo" "baz")
    (multimap-adjoin! b "foo" "bar")
    (multimap-adjoin! b "foo" "qux")
    (multimap-adjoin! b "bar" "fred")
    (multimap-union! a b)
    (assert-equal (set-size (multimap-ref a "foo")) 3)
    (assert-equal (set-size (multimap-ref a "bar")) 1)
    (assert-equal (set-size (multimap-ref b "foo")) 2)
    (assert-equal (set-size (multimap-ref b "bar")) 1))

  (test "union of maps (immutable)"
    (define a (make-mmap))
    (define b (make-mmap))
    (multimap-adjoin! a "foo" "bar")
    (multimap-adjoin! a "foo" "baz")
    (multimap-adjoin! b "foo" "bar")
    (multimap-adjoin! b "foo" "qux")
    (multimap-adjoin! b "bar" "fred")
    (define c (multimap-union a b))
    (assert-equal (set-size (multimap-ref a "foo")) 2)
    (assert-equal (set-size (multimap-ref a "bar")) 0)
    (assert-equal (set-size (multimap-ref b "foo")) 2)
    (assert-equal (set-size (multimap-ref b "bar")) 1)
    (assert-equal (set-size (multimap-ref c "foo")) 3)
    (assert-equal (set-size (multimap-ref c "bar")) 1))

  (test "difference of maps"
    (define a (make-mmap))
    (define b (make-mmap))
    (multimap-adjoin! a "foo" "bar")
    (multimap-adjoin! a "foo" "baz")
    (multimap-adjoin! a "bar" "fred")
    (multimap-adjoin! a "baz" "waldo")
    (multimap-adjoin! b "foo" "bar")
    (multimap-adjoin! b "foo" "qux")
    (multimap-adjoin! b "bar" "fred")
    (define c (multimap-difference a b))
    (assert-equal (set->list (multimap-ref c "foo")) (list "baz"))
    (assert-true (set-empty? (multimap-ref c "bar")))
    (assert-equal (set->list (multimap-ref c "baz")) (list "waldo"))))
