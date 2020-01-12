(import (scheme base)
        (schemepunk syntax)
        (schemepunk test))

(test-suite "Schemepunk Syntax"
  (test "->"
    (assert-eqv (-> "6" string->number (/ 2) (- 5)) -2))

  (test "->>"
    (assert-eqv (->> "6" string->number (/ 2) (- 5)) 14/3))

  (test "as->"
    (assert-eqv (as-> "6" x (string->number x) (/ x 2) (- 5 x)) 2))

  (test "λ"
    (assert-equal (map (λ x (+ x 2)) (map (λ (x) (+ x 3)) '(1 2 3))) '(6 7 8)))

  (test "λ->"
    (assert-equal (map (λ-> number->string (string-append "x")) '(1 2 3))
                  '("1x" "2x" "3x")))

  (test "λ->>"
    (assert-equal (map (λ->> number->string (string-append "x")) '(1 2 3))
                  '("x1" "x2" "x3")))

  (test "let1"
    (assert-eqv (let1 x 2 (define y 3) (+ x y)) 5))

  (test "one-of"
    (assert-true ((one-of 1 3 5 7) 5))
    (assert-false ((one-of 1 3 5 7) 2))
    (assert-true ((one-of 5 (is zero?)) 0))
    (assert-true ((one-of 5 (is zero?)) 5))
    (assert-false ((one-of 5 (is zero?)) 3)))

  (test-suite "Pattern Matching"
    (test "match quoted symbols"
      (assert-eqv
        (match 'bar ('foo 1) ('bar 2) ('baz 3))
        2))
    (test "match else"
      (assert-eqv (match 'bar ('foo 1) (else 2)) 2))
    (test "match fail"
      (assert-equal
        (guard (e (#t (error-object-message e)))
          (match 'bar ('foo "matched foo")))
        "match failed"))
    (test "match destructuring"
      (assert-equal
        (match '(foo bar baz)
          (('foo a) a)
          ((a b 'bar) `(,a ,b))
          (('foo b c) `(,c ,b)))
        '(baz bar)))
    (test "match ="
      (assert-true
        (match (+ 2 2)
          (= 5 #f)
          (= 4 #t)
          (else #f))))
    (test "match equal"
      (assert-true
        (match (list 1 2 3)
          (equal '(1 2) #f)
          (equal '(1 2 3) #t)
          (else #f))))
    (test "match is"
      (assert-true
        (match (- 2 2)
          (is string? #f)
          (is zero? #t)
          (else #f))))))
