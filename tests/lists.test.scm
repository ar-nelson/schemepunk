(import (scheme base)
        (schemepunk syntax)
        (schemepunk list)
        (schemepunk test))

(test-suite "List Utilities"
  (test "topological-sort"
    (assert-equal (topological-sort '((x z) (y x) (z)))
                                    '(z x y))))
