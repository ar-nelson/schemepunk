(import (scheme base)
        (schemepunk syntax)
        (schemepunk function)
        (schemepunk test))

(test-group "Function Combinators"
  (test-equal 'foo (identity 'foo))
  (test-equal 'foo ((const 'foo) 'bar))
  (test-equal '(y . x) ((flip cons) 'x 'y))
  (test-equal "-2" ((compose number->string - length) '(1 2)))
  (test-equal "-3" ((bind length - number->string) '(1 2 3)))
  (test-equal #f ((complement <) 1 2)))

