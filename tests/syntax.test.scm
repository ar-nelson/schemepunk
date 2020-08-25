(import (scheme base)
        (schemepunk syntax)
        (schemepunk test))

(test-group "Schemepunk Syntax"
  (test "λ"
    (assert-equal (map (λ x (+ x 2)) (map (λ (x) (+ x 3)) '(1 2 3))) '(6 7 8)))

  (test "λ with destructuring"
    (assert-equal (map (λ ((a b) (c d)) (list a b c d))
                       '((10 20) (30 40))
                       '((50 60) (70 80)))
                  '((10 20 50 60) (30 40 70 80))))

  (test "let1"
    (assert-eqv (let1 x 2 (define y 3) (+ x y)) 5))

  (test "one-of"
    (assert-true ((one-of 1 3 5 7) 5))
    (assert-false ((one-of 1 3 5 7) 2))
    (assert-true ((one-of 5 (is zero?)) 0))
    (assert-true ((one-of 5 (is zero?)) 5))
    (assert-false ((one-of 5 (is zero?)) 3)))

  (test "cut"
    (assert-eqv ((cut + 2 <>) 3) 5)
    (assert-eqv ((cut / <> 2 <>) 30 5) 3)
    (assert-eqv ((cut <> 3 <>) - 1) 2)
    (assert-eqv ((cut + <> 1 <...>) 2 3 4) 10))

  (test "cute"
    (let* ((side-effects '())
           (cute1 (cute cons <> (begin (set! side-effects `(foo ,@side-effects)) '(2 3))))
           (cute2 (cute cons <> side-effects)))
      (assert-equal (cute1 1) '(1 2 3))
      (assert-equal (cute1 5) '(5 2 3))
      (assert-equal (cute2 'bar) '(bar foo))
      (assert-equal side-effects '(foo))
      (assert-eqv ((cute + <> 1 <...>) 2 3 4) 10)))

  (test "format"
    (assert-equal (format #f "foo") "foo")
    (assert-equal (format #f "foo ~a ~s" "bar" "baz") "foo bar \"baz\"")
    (assert-equal (format #f "~~~%") "~\n")
    (parameterize ((current-output-port (open-output-string)))
      (format #t "1 ~a ~s" 2 3)
      (assert-equal (get-output-string (current-output-port)) "1 2 3")))

  (test "assume"
    (assume #t)
    (assume #t "a message"))

  (test "is"
    (assert-true (is 0 zero?))
    (assert-false (is 1 zero?))
    (assert-true (is 1 < 2))
    (assert-false (is 2 < 1))
    (assert-true (is 4 > 3 > 2 > 1))
    (assert-false (is 4 > 3 > 3 > 1)))

  (test "is with _"
    (assert-true ((is _ zero?) 0))
    (assert-false ((is _ zero?) 1))
    (assert-true ((is _ < 2) 1))
    (assert-false ((is 2 < _) 1))
    (assert-true ((is 4 > 3 > _ > 1) 2))
    (assert-false ((is _ > 3 > _ > 1) 4 3)))

  (test "isnt"
    (assert-false (isnt 0 zero?))
    (assert-true (isnt 1 zero?))
    (assert-false (isnt 1 < 2))
    (assert-true (isnt 2 < 1))
    (assert-false (isnt 4 > 3 > 2 > 1))
    (assert-true (isnt 4 > 3 > 3 > 1)))

  (test "define+"
    (let ()
      (define+ (foo) 1)
      (define+ (bar x) (+ x 1))
      (define+ (baz x y z) (list x y z))
      (define+ (qux a b :rest c) (list a b c))
      (assert-equal (foo) 1)
      (assert-equal (bar 2) 3)
      (assert-equal (baz 1 2 3) '(1 2 3))
      (assert-equal (qux 1 2 3 4) '(1 2 (3 4)))))

  (test "define+ with optionals"
    (let ()
      (define+ (foo x :optional (y 3)) (+ x y))
      (define+ (bar :optional (x 1) (y 2)) (+ x y))
      (define+ (baz x :optional y z) (list x y z))
      (define+ (qux x :optional (y 'y) :rest z) (list x y z))
      (assert-equal (foo 1) 4)
      (assert-equal (foo 1 2) 3)
      (assert-equal (bar) 3)
      (assert-equal (bar 4) 6)
      (assert-equal (bar 4 5) 9)
      (assert-equal (baz 1) '(1 #f #f))
      (assert-equal (baz 1 2 3) '(1 2 3))
      (assert-equal (qux 'x) '(x y ()))
      (assert-equal (qux 1 2 3 4) '(1 2 (3 4)))))

  (test-group "Chaining"
    (define (exclamation x) (string-append x "!"))
    (define (foo+bar x) (values (string-append x "foo") (string-append x "bar")))

    (test-equal "chain" "bazbarfoo!"
      (chain ""
             (string-append "foo" _)
             (string-append "bar" _)
             (string-append "baz" _)
             (exclamation _)))

    (test-equal "chain with mixed _ position" "barfoobaz"
      (chain ""
             (string-append _ "foo")
             (string-append "bar" _)
             (string-append _ "baz")))

    (test-equal "chain with _ in operator position" 3
      (chain +
             (_ 1 2)))

    (test-equal "chain without _" "barbazqux"
      (chain ""
             (string-append _ "foo")
             (string-append "bar" "baz")
             (string-append _ "qux")))

    (test-equal "chain multiple _" "quxfoo/quxbar"
      (chain "qux"
             (foo+bar _)
             (string-append _ "/" _)))

    (test-equal "chain _ ..." "bazquxfooquxbar"
      (chain "qux"
             (foo+bar _)
             (string-append "baz" _ ...)))

    (test-equal "chain _ _ ..." "quxfoobazquxbar"
      (chain "qux"
             (foo+bar _)
             (string-append _ "baz" _ ...)))

    (test-equal "chain-and" "bazbarfoo!"
      (chain-and ""
                 (string-append "foo" _)
                 (string-append "bar" _)
                 (string-append "baz" _)
                 (exclamation _)))

    (test-equal "chain-and with mixed _ position" "barfoobaz"
      (chain-and ""
                 (string-append _ "foo")
                 (string-append "bar" _)
                 (string-append _ "baz")))

    (test-equal "chain-and without _" "barbazqux"
      (chain-and ""
                 (string-append "foo" _)
                 (string-append "bar" "baz")
                 (string-append _ "qux")))

    (test-equal "chain-and short-circuit" #f
      (chain-and ""
                 (string-append "foo" _)
                 (equal? _ "bar")
                 (string-append "baz" _)
                 (exclamation _)))

    (test-equal "chain-and short-circuit first" #f
      (chain-and #f
                 (not _)))

    (test-equal "chain-when" "bazfoo"
      (chain-when ""
                  ((= (+ 2 2) 4) (string-append "foo" _))
                  ((= (+ 2 2) 5) (string-append "bar" _))
                  (#t (string-append "baz" _))))

    (test-equal "chain-when with mixed _ position" "barfooqux"
      (chain-when ""
                  (#t (string-append _ "foo"))
                  (#t (string-append "bar" _))
                  (#f (string-append _ "baz"))
                  (#t (string-append _ "qux"))))

    (test-equal "chain-when without _" "barqux"
      (chain-when ""
                  (#t (string-append _ "foo"))
                  (#t (string-append "bar"))
                  (#f (string-append _ "baz"))
                  (#t (string-append _ "qux"))))

    (test-equal "chain-lambda" "bazbarfoo!"
      ((chain-lambda (string-append "foo" _)
                     (string-append "bar" _)
                     (string-append "baz" _)
                     (exclamation _))
       ""))

    (test-equal "chain-lambda one step" "foobar"
      ((chain-lambda (string-append "foo" _)) "bar"))

    (test-equal "chain-lambda with mixed _ position" "barfoobaz"
      ((chain-lambda (string-append _ "foo")
                     (string-append "bar" _)
                     (string-append _ "baz"))
       ""))

    (test-equal "chain-lambda multiple _" "foobarbazqux"
      ((chain-lambda (string-append _ "bar" _)
                     (string-append _ "qux"))
       "foo"
       "baz"))

    (test-equal "chain-lambda without _" "barqux"
      ((chain-lambda (string-append "bar")
                     (string-append _ "qux"))))

    (test-equal "chain-lambda _ ..." "foobarbazqux"
      ((chain-lambda (string-append "foo" _ ...)
                     (string-append _ "qux"))
       "bar"
       "baz"))

    (test-equal "chain-lambda _ _ ..." "foobarbazquxquux"
      ((chain-lambda (string-append _ "bar" _ ...)
                     (string-append _ "quux"))
       "foo"
       "baz"
       "qux"))

    (test-equal "nest" '(1 2 (3 (4) 5))
      (nest (quote _)
            (1 2 _)
            (3 _ 5)
            (_)
            4))

    (test-equal "nest with custom _" '(1 2 (3 (4) 5))
      (nest <>
            (quote <>)
            (1 2 <>)
            (3 <> 5)
            (<>)
            4))

    (test-equal "nested nest" '(1 2 3 (4 5 6))
      (nest (nest _2 (quote _2) (1 2 3 _2) _ 6)
            (_ 5 _2)
            4))

    (test-equal "nest-reverse" '(1 2 (3 (4) 5))
      (nest-reverse 4
                    (_)
                    (3 _ 5)
                    (1 2 _)
                    (quote _)))

    (test-equal "nest-reverse with custom _" '(1 2 (3 (4) 5))
      (nest-reverse 4 <>
                    (<>)
                    (3 <> 5)
                    (1 2 <>)
                    (quote <>))))

  (test-group "Pattern Matching"
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
        (match '((1 2) (3 4) (5 6))
          (((x y) …) (list x y))
          (else #f)))
      (assert-equal
        '((1 3 5) (2 4 6))
        (match '(a (1 x 2) (3 x 4) (5 x 6))
          ((a (o 'y e) …) (list e o))
          ((a (o 'x e) …) (list o e))
          (else #f))))
    (test "match and"
      (assert-equal
        '((1 2 3) 2)
        (match '((1 2 3) (4 5 6))
          (((and x (y z w)) _) (list x z))
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
            (raise 'a)))))
    (test "define+ argument destructuring"
      (let ()
        (define+ (foo (x y)) (+ x y))
        (define+ (bar (a b) (c d e)) (list a b c d e))
        (assert-equal (foo '(1 2)) 3)
        (assert-equal (bar '(z y) '(x w v)) '(z y x w v))))
    (test "define+ argument destructuring :rest"
      (let ()
        (define+ (baz :rest ((x y) …)) (list x y))
        (define+ (qux :optional (x 'x) :rest ((y z) …)) (list x y z))
        (assert-equal (baz) '(() ()))
        (assert-equal (baz '(1 2) '(3 4) '(5 6)) '((1 3 5) (2 4 6)))
        (assert-equal (qux) '(x () ()))
        (assert-equal (qux 1 '(2 3) '(4 5) '(6 7))
                      '(1 (2 4 6) (3 5 7)))))))
