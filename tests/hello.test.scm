(import (scheme base) (schemepunk test))

(test-suite "Example Tests"
  (test "Hello, world!"
    (assert-true "impossible" #t))

  #;(test "This test should fail"
      (assert-true "boom" #f)))
