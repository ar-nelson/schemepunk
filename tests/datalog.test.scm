(import (scheme base)
        (schemepunk syntax)
        (schemepunk list)
        (schemepunk sort)
        (schemepunk comparator)
        (schemepunk datalog)
        (schemepunk test))

(define pair-comparator
  (make-pair-comparator symbol-comparator (make-default-comparator)))

(define alist-comparator
  (make-list-comparator pair-comparator list? null? car cdr))

(define (sort-alist alist)
  (list-sort! (cut <? pair-comparator <> <>) alist))

(define (sort-result result)
  (and result (list-sort! (cut <? alist-comparator <> <>) (map sort-alist result))))

(test-group "Datalog"
  (test "insert unary predicate, then query it"
    (let-datalog
      ((foo 'bar))
      (assert-equal (?- (foo X)) '(((X . bar))))))

  (test "query unary predicate with constant"
    (let-datalog
      ((foo 'bar))
      (assert-equal (?- (foo 'bar)) '(()))
      (assert-false (?- (foo 'baz)))))

  (test "query unary predicate with wildcard"
    (let-datalog
      ((foo 'bar))
      (assert-equal (?- (foo _)) '(()))
      (assert-false (?- (bar _)))))

  (test "insert result of expression as fact"
    (define x (+ 2 2))
    (let-datalog
      ((foo x))
      (assert-equal (?- (foo X)) '(((X . 4))))))

  (test "insert multiple values for unary predicate"
    (let-datalog
      ((foo 'bar) (foo 'baz) (foo 'qux))
      (assert-equal (sort-result (?- (foo X)))
                    '(((X . bar)) ((X . baz)) ((X . qux))))))

  (test "query different unary predicates"
    (let-datalog
      ((foo 'x) (bar 'y) (foo 'z))
      (assert-equal (sort-result (?- (foo X))) '(((X . x)) ((X . z))))
      (assert-equal (sort-result (?- (bar X))) '(((X . y))))))

  (test "insert binary predicate, then query it"
    (let-datalog
      ((foo 'bar 'baz))
      (assert-equal (sort-result (?- (foo X Y)))
                    '(((X . bar) (Y . baz))))))

  (test "query binary predicate with constants"
    (let-datalog
      ((foo 'bar 'baz) (foo 'bar 'qux) (foo 'qux 'baz))
      (assert-equal (sort-result (?- (foo 'bar X))) '(((X . baz)) ((X . qux))))
      (assert-equal (sort-result (?- (foo X 'baz))) '(((X . bar)) ((X . qux))))
      (assert-false (?- (foo 'baz X)))))

  (test "query binary predicate with wildcards"
    (let-datalog
      ((foo 'bar 'baz) (foo 'bar 'qux) (foo 'qux 'baz))
      (assert-equal (?- (foo 'bar _)) '(() ()))
      (assert-equal (?- (foo _ 'baz)) '(() ()))
      (assert-equal (?- (foo _ _)) '(() () ()))
      (assert-false (?- (foo _ _ _)))
      (assert-false (?- (foo 'baz _)))))

  (test "insert after query"
    (with-extended-datalog-db (λ ()
      (define-datalog (foo 'bar))
      (assert-true (?- (foo 'bar)))
      (assert-false (?- (foo 'baz)))
      (define-datalog (foo 'baz))
      (assert-true (?- (foo 'bar)))
      (assert-true (?- (foo 'baz))))))

  (test "simple unary rule"
    (let-datalog
      ((foo 'a)
       (foo 'b)
       (:- (bar X) (foo X)))
      (assert-equal (sort-result (?- (bar X))) '(((X . a)) ((X . b))))))

  (test "insert new fact after rule"
    (with-extended-datalog-db (λ ()
      (define-datalog (foo 'a) (foo 'b) (:- (bar X) (foo X)))
      (assert-equal (sort-result (?- (bar X))) '(((X . a)) ((X . b))))
      (define-datalog (foo 'c))
      (assert-equal (sort-result (?- (bar X))) '(((X . a)) ((X . b)) ((X . c)))))))

  (test "insert new rule after rule"
    (with-extended-datalog-db (λ ()
      (define-datalog (foo 'a) (foo 'b) (:- (bar X) (foo X)))
      (assert-equal (sort-result (?- (bar X))) '(((X . a)) ((X . b))))
      (define-datalog (:- (baz X) (foo X)))
      (assert-equal (sort-result (?- (baz X))) '(((X . a)) ((X . b)))))))

  (test "transitive rule"
    (let-datalog
      ((:- (foo X Z) (foo X Y) (foo Y Z))
       (foo 'a 'b)
       (foo 'b 'c)
       (foo 'c 'd))
      (assert-equal (sort-result (?- (foo X Y)))
                    '(((X . a) (Y . b))
                      ((X . a) (Y . c))
                      ((X . a) (Y . d))
                      ((X . b) (Y . c))
                      ((X . b) (Y . d))
                      ((X . c) (Y . d))))))

  (test "long dependency chain"
    (let-datalog
      ((:- (baz X) (foo X))
       (:- (qux X) (bar X))
       (:- (bazqux X Y) (baz X) (qux Y))
       (:- (fred Y X) (bazqux X Y))
       (foo 'a)
       (bar 'b)
       (foo 'c)
       (bar 'd))
      (assert-equal (sort-result (?- (fred X Y)))
                    '(((X . b) (Y . a))
                      ((X . b) (Y . c))
                      ((X . d) (Y . a))
                      ((X . d) (Y . c))))))

  (test "simple negated rule"
    (let-datalog
      ((foo 'a)
       (foo 'b)
       (bar 'b)
       (:- (baz X) (foo X) (not (bar X))))
      (assert-equal (?- (baz X)) '(((X . a))))))

  (test "stratified negated rule"
    (let-datalog
      ((:- (v X) (r X Y))
       (:- (v Y) (r X Y))
       (:- (t X Y) (r X Y))
       (:- (t X Y) (t X Z) (r Z Y))
       (:- (tc X Y) (v X) (v Y) (not (t X Y)))
       (r 'a 'b)
       (r 'b 'c))
      (assert-equal (sort-result (?- (v X)))
                    '(((X . a)) ((X . b)) ((X . c))))
      (assert-equal (sort-result (?- (t X Y)))
                    '(((X . a) (Y . b))
                      ((X . a) (Y . c))
                      ((X . b) (Y . c))))
      (assert-equal (sort-result (?- (tc X Y)))
                    '(((X . a) (Y . a))
                      ((X . b) (Y . a))
                      ((X . b) (Y . b))
                      ((X . c) (Y . a))
                      ((X . c) (Y . b))
                      ((X . c) (Y . c))))))

  (test "stratification cycle"
    (guard (e ((not (failure? e)) #t))
      (let-datalog
        ((:- (foo X) (bar X))
         (:- (bar X) (baz X) (not (foo X))))
        (?- (foo X))
        (fail "did not raise error"))))

  (test "variable = constant"
    (let-datalog
      ((:- (bar X Y) (foo X) (foo Y) (= X 'a))
       (foo 'a)
       (foo 'b)
       (foo 'c))
      (assert-equal (sort-result (?- (bar X Y)))
                    '(((X . a) (Y . a))
                      ((X . a) (Y . b))
                      ((X . a) (Y . c))))))

  (test "variable inequality"
    (let-datalog
      ((:- (bar X Y) (foo X) (foo Y) (!= X Y))
       (foo 'a)
       (foo 'b)
       (foo 'c))
      (assert-equal (sort-result (?- (bar X Y)))
                    '(((X . a) (Y . b))
                      ((X . a) (Y . c))
                      ((X . b) (Y . a))
                      ((X . b) (Y . c))
                      ((X . c) (Y . a))
                      ((X . c) (Y . b))))))

  (test "rule with guard"
    (let-datalog
      ((:- (bar X) (foo X) (? number? X))
       (foo 'a)
       (foo 1)
       (foo "fhqwhgads")
       (foo 2))
      (assert-equal (sort-result (?- (bar X))) '(((X . 1)) ((X . 2))))))

  (test "rule with destructuring param"
    (define list-a '(1 2 3))
    (define list-b '(5 4 (3 2 1)))
    (let-datalog
      ((list list-a)
       (list list-b)
       (even 2)
       (even 4)
       (even 6)
       (:- (even-second X) (list (_ X _)) (even X)))
      (assert-equal (sort-result (?- (even-second X))) '(((X . 2)) ((X . 4))))))

  (test "rule with destructuring ="
    (define list-a '(3 2 1))
    (define list-b `(5 4 ,list-a))
    (let-datalog
      ((list list-a)
       (list list-b)
       (:- (nested-list X Y) (list X) (list Y) (= X (5 4 Y)) (= Y (3 2 1))))
      (assert-equal (sort-result (?- (nested-list X Y)))
                    '(((X . (5 4 (3 2 1))) (Y . (3 2 1))))))))
