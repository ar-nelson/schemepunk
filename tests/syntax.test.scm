(import (scheme base)
        (schemepunk syntax)
        (schemepunk test))

(test-suite "Schemepunk Syntax"
  (test "->"
    (assert-eqv -2 (-> "6" string->number (/ 2) (- 5))))

  (test "->>"
    (assert-eqv 14/3 (->> "6" string->number (/ 2) (- 5))))

  (test "as->"
    (assert-eqv 2 (as-> "6" x (string->number x) (/ x 2) (- 5 x))))

  (test "λ"
    (assert-equal '(6 7 8) (map (λ x (+ x 2)) (map (λ (x) (+ x 3)) '(1 2 3)))))

  (test "λ->"
    (assert-equal
      '("1x" "2x" "3x")
      (map (λ-> number->string (string-append "x")) '(1 2 3))))

  (test "λ->>"
    (assert-equal
      '("x1" "x2" "x3")
      (map (λ->> number->string (string-append "x")) '(1 2 3))))

  (test "let1"
    (assert-eqv 5 (let1 x 2 (define y 3) (+ x y))))

  (test "one-of"
    (assert-true ((one-of 1 3 5 7) 5))
    (assert-false ((one-of 1 3 5 7) 2))
    (assert-true ((one-of 5 (is zero?)) 0))
    (assert-true ((one-of 5 (is zero?)) 5))
    (assert-false ((one-of 5 (is zero?)) 3)))

  (test-suite "Pattern Matching"
    (test "match quoted symbols"
      (assert-eqv
        2
        (match 'bar ('foo 1) ('bar 2) ('baz 3))))
    (test "match else"
      (assert-eqv 2 (match 'bar ('foo 1) (else 2))))
    (test "match fail"
      (assert-equal
        "match failed"
        (guard (e (#t (error-object-message e)))
          (match 'bar ('foo "matched foo")))))
    (test "match destructuring"
      (assert-equal
        '(baz bar)
        (match '(foo bar baz)
          (('foo a) a)
          ((a b 'bar) `(,a ,b))
          (('foo b c) `(,c ,b)))))
    (test "match quasiquote"
      (assert-equal
        '(baz bar)
        (match '(foo bar baz)
          (`(foo ,b ,a) `(,a ,b)))))
    (test "match vectors"
      (assert-true
        (match #()
          (() #f)
          (#() #t)
          (else #f)))
      (assert-equal 3
        (match #(1 2 3)
          ((x 2 3) x)
          (#(1 x) x)
          (#(x 2 3 4) x)
          (#(1 2 x) x)
          (else 'no-match))))
    (test "match numbers"
      (assert-true
        (match (+ 2 2)
          (5 #f)
          (4 #t)
          (else #f))))
    (test "match strings"
      (assert-equal
        "qux"
        (match "baz"
          ("foo" "bar")
          ("baz" "qux")
          ("quux" "quuz"))))
    (test "match quoted lists"
      (assert-true
        (match (list 1 2 3)
          ('(1 2) #f)
          ('(1 2 3) #t)
          (else #f))))
    (test "match ?"
      (assert-true
        (match (- 2 2)
          ((? string?) #f)
          ((? zero?) #t)
          (else #f))))
    (test "match _"
      (assert-true
        (match "foo"
          ("bar" #f)
          (_ #t)
          (else #f))))
    (test "match …"
      (assert-equal
        '((1 3 5) (2 4 6))
        (match '(a (1 x 2) (3 x 4) (5 x 6))
          ((a (o 'y e) …) (list e o))
          ((a (o 'x e) …) (list o e))
          (else #f))))
    (test "match-guard"
      (assert-equal
        "foo"
        (match-guard ((('a x) x)
                      (('b x y) (string-append x y))
                      (('c _) "bar"))
          (raise '(b "f" "oo"))))
      (assert-equal
        "no error"
        (match-guard ((('a x) x)
                      (('b x y) (string-append x y))
                      (('c _) "bar"))
          "no error"))
      (assert-equal
        "not caught"
        (guard (e ((eqv? e 'a) "not caught"))
          (match-guard (('b "caught"))
            (raise 'a)))))))
