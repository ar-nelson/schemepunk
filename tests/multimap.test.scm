(import (scheme base)
        (schemepunk syntax)
        (schemepunk comparator)
        (schemepunk set)
        (schemepunk multimap)
        (schemepunk test))

(define (make-mmap)
  (make-multimap (make-default-comparator) (make-default-comparator)))

(test-suite "Multimap"
  (test "create multimap"
    (assert-true (multimap? (make-mmap))))

  (test "insert and get single multimap item"
    (let1 mmap (make-mmap)
      (multimap-add! mmap "foo" "bar")
      (assert-true (set? (multimap-ref mmap "foo")))
      (assert-equal (set->list (multimap-ref mmap "foo")) '("bar"))))

  (test "insert and get multiple values for same key"
    (let1 mmap (make-mmap)
      (multimap-add! mmap "foo" "bar")
      (multimap-add! mmap "foo" "baz")
      (multimap-add! mmap "foo" "qux")
      (assert-equal (set-size (multimap-ref mmap "foo")) 3)))

  (test "insert and get multiple keys"
    (let1 mmap (make-mmap)
      (multimap-add! mmap "foo" "bar")
      (multimap-add! mmap "baz" "qux")
      (assert-equal (set->list (multimap-ref mmap "foo")) '("bar"))
      (assert-equal (set->list (multimap-ref mmap "baz")) '("qux"))))

  (test "delete individual value"
    (let1 mmap (make-mmap)
      (multimap-add! mmap "foo" "bar")
      (multimap-add! mmap "foo" "baz")
      (multimap-delete-value! mmap "foo" "bar")
      (multimap-delete-value! mmap "foo" "bar")
      (multimap-delete-value! mmap "foo" "qux")
      (assert-equal (set->list (multimap-ref mmap "foo")) '("baz"))))

  (test "delete entire key"
    (let1 mmap (make-mmap)
      (multimap-add! mmap "foo" "bar")
      (multimap-add! mmap "foo" "baz")
      (multimap-delete-key! mmap "foo")
      (assert-true (set-empty? (multimap-ref mmap "foo")))))

  (test "empty keys return an empty set"
    (let1 mmap (make-mmap)
      (assert-true (set? (multimap-ref mmap "foo")))
      (assert-true (set-empty? (multimap-ref mmap "foo")))))

  (test "duplicate values are ignored"
    (let1 mmap (make-mmap)
      (multimap-add! mmap "foo" "bar")
      (multimap-add! mmap "foo" "baz")
      (multimap-add! mmap "foo" "bar")
      (assert-equal (set-size (multimap-ref mmap "foo")) 2)))

  (test "copy creates a new map"
    (let1 original (make-mmap)
      (multimap-add! original "foo" "bar")
      (let1 copy (multimap-copy original)
        (assert-false (eq? original copy))
        (assert-false
          (eq? (multimap->hash-table original) (multimap->hash-table copy)))
        (assert-false
          (eq? (multimap-ref original "foo") (multimap-ref copy "foo")))
        (multimap-add! copy "foo" "baz")
        (assert-equal (set-size (multimap-ref copy "foo")) 2)
        (assert-equal (set-size (multimap-ref original "foo")) 1))))

  (test "union of maps"
    (let ((a (make-mmap)) (b (make-mmap)))
      (multimap-add! a "foo" "bar")
      (multimap-add! a "foo" "baz")
      (multimap-add! b "foo" "bar")
      (multimap-add! b "foo" "qux")
      (multimap-add! b "bar" "fred")
      (multimap-union! a b)
      (assert-equal (set-size (multimap-ref a "foo")) 3)
      (assert-equal (set-size (multimap-ref a "bar")) 1)
      (assert-equal (set-size (multimap-ref b "foo")) 2)
      (assert-equal (set-size (multimap-ref b "bar")) 1))))
