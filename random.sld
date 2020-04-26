(define-library (schemepunk random)
  (export random-integer random-real)
  (cond-expand
    (chicken
      (import (rename (chicken random) (pseudo-random-integer random-integer)
                                       (pseudo-random-real random-real))))
    (gerbil
      (import (only (gerbil gambit random) random-integer random-real)))
    (kawa
      (import (scheme base) (class java.lang Math))
      (begin
        (define (random-real) (Math:random))
        (define (random-integer n) (exact (floor (* (Math:random) n))))))
    (else
      (import (only (srfi 27) random-integer random-real)))))
