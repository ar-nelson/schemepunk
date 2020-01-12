(import (scheme base)
        (schemepunk syntax)
        (schemepunk list)
        (schemepunk sort)
        (schemepunk datalog)
        (schemepunk test))

(define (symbol<? x y)
  (string<? (symbol->string x) (symbol->string y)))

(define (var<? x y)
  (symbol<? (var->symbol x) (var->symbol y)))

(define (pair<? x y)
  (or (var<? (car x) (car y))
      (and (eq? (car x) (car y)) (symbol<? (cdr x) (cdr y)))))

(define (alist<? x y)
  (cond
    ((null? y) #f)
    ((null? x) (not (null? y)))
    (else (or (symbol<? (cdar x) (cdar y))
              (and (eq? (cdar x) (cdar y))
                   (alist<? (cdr x) (cdr y)))))))

(define (sort-alist alist)
  (list-sort! pair<? alist))

(define (sort-result result)
  (list-sort! alist<? (map sort-alist result)))

(cond-expand ((not kawa) (test-suite "Datalog"
  (test "sorting alists of symbols"
    (let-datalog (X Y Z)
      ()
      (assert-true (list-sorted? pair<? `((,X . foo) (,Y . bar) (,Z . baz))))
      (assert-false (list-sorted? pair<? `((,Y . foo) (,X . bar) (,Z . baz))))
      (assert-false (list-sorted? pair<? `((,X . foo) (,X . bar) (,X . baz))))
      (assert-true (list-sorted? pair<? `((,X . bar) (,X . baz) (,X . foo))))
      (assert-equal (sort-result `(((,Y . bar) (,X . foo)) ((,X . bar) (,Y . foo)))) 
                    `(((,X . bar) (,Y . foo)) ((,X . foo) (,Y . bar))))))

  (test "insert unary predicate, then query it"
    (let-datalog (X)
      ((foo 'bar))
      (assert-equal (sort-result (?- (foo X))) `(((,X . bar))))))

  (test "query unary predicate with constant"
    (let-datalog ()
      ((foo 'bar))
      (assert-equal (?- (foo 'bar)) '(()))
      (assert-equal (?- (foo 'baz)) '())))

  (test "insert multiple values for unary predicate"
    (let-datalog (X)
      ((foo 'bar) (foo 'baz) (foo 'qux))
      (assert-equal (sort-result (?- (foo X)))
                    `(((,X . bar)) ((,X . baz)) ((,X . qux))))))

  (test "query different unary predicates"
    (let-datalog (X)
      ((foo 'x) (bar 'y) (foo 'z))
      (assert-equal (sort-result (?- (foo X))) `(((,X . x)) ((,X . z))))
      (assert-equal (sort-result (?- (bar X))) `(((,X . y))))))

  (test "insert binary predicate, then query it"
    (let-datalog (X Y)
      ((foo 'bar 'baz))
      (assert-equal (sort-result (?- (foo X Y)))
                    `(((,X . bar) (,Y . baz))))))

  (test "query binary predicate with constants"
    (let-datalog (X)
      ((foo 'bar 'baz) (foo 'bar 'qux) (foo 'qux 'baz))
      (assert-equal (sort-result (?- (foo 'bar X))) `(((,X . baz)) ((,X . qux))))
      (assert-equal (sort-result (?- (foo X 'baz))) `(((,X . bar)) ((,X . qux))))
      (assert-equal (sort-result (?- (foo 'baz X))) '())))

  (test "insert after query"
    (let-datalog ()
      ((foo 'bar))
      (assert-equal (?- (foo 'bar)) '(()))
      (assert-equal (?- (foo 'baz)) '())
      (let-datalog ()
        ((foo 'baz))
        (assert-equal (?- (foo 'bar)) '(()))
        (assert-equal (?- (foo 'baz)) '(())))))

  (test "simple unary rule"
    (let-datalog (X)
      ((foo 'a)
       (foo 'b)
       (:- (bar X) (foo X)))
      (assert-equal (sort-result (?- (bar X))) `(((,X . a)) ((,X . b))))))

  (test "transitive rule"
    (let-datalog (X Y Z)
      ((:- (foo X Z) (foo X Y) (foo Y Z))
       (foo 'a 'b)
       (foo 'b 'c)
       (foo 'c 'd))
      (assert-equal (sort-result (?- (foo X Y)))
                    `(((,X . a) (,Y . b))
                      ((,X . a) (,Y . c))
                      ((,X . a) (,Y . d))
                      ((,X . b) (,Y . c))
                      ((,X . b) (,Y . d))
                      ((,X . c) (,Y . d))))))

  (test "long dependency chain"
    (let-datalog (X Y Z)
      ((:- (baz X) (foo X))
       (:- (qux X) (bar X))
       (:- (bazqux X Y) (baz X) (qux Y))
       (:- (fred Y X) (bazqux X Y))
       (foo 'a)
       (bar 'b)
       (foo 'c)
       (bar 'd))
      (assert-equal (sort-result (?- (fred X Y)))
                    `(((,X . b) (,Y . a))
                      ((,X . b) (,Y . c))
                      ((,X . d) (,Y . a))
                      ((,X . d) (,Y . c))))))

  (test "simple negated rule"
    (let-datalog (X)
      ((foo 'a)
       (foo 'b)
       (bar 'b)
       (:- (baz X) (foo X) (not bar X)))
      (assert-equal (?- (baz X)) `(((,X . a))))))

  (test "stratified negated rule"
    (let-datalog (X Y Z)
      ((:- (v X) (r X Y))
       (:- (v Y) (r X Y))
       (:- (t X Y) (r X Y))
       (:- (t X Y) (t X Z) (r Z Y))
       (:- (tc X Y) (v X) (v Y) (not t X Y))
       (r 'a 'b)
       (r 'b 'c))
      (assert-equal (sort-result (?- (v X)))
                    `(((,X . a)) ((,X . b)) ((,X . c))))
      (assert-equal (sort-result (?- (t X Y)))
                    `(((,X . a) (,Y . b))
                      ((,X . a) (,Y . c))
                      ((,X . b) (,Y . c))))
      (assert-equal (sort-result (?- (tc X Y)))
                    `(((,X . a) (,Y . a))
                      ((,X . b) (,Y . a))
                      ((,X . b) (,Y . b))
                      ((,X . c) (,Y . a))
                      ((,X . c) (,Y . b))
                      ((,X . c) (,Y . c))))))

  (test "stratification cycle"
    (guard (e ((not (failure? e)) #t))
      (let-datalog (X)
        ((:- (foo X) (bar X))
         (:- (bar X) (not foo X)))
        (fail "did not raise error"))))

  (test "variable inequality"
    (let-datalog (X Y)
      ((:- (bar X Y) (foo X) (foo Y) (!= X Y))
       (foo 'a)
       (foo 'b)
       (foo 'c))
      (assert-equal (sort-result (?- (bar X Y)))
                    `(((,X . a) (,Y . b))
                      ((,X . a) (,Y . c))
                      ((,X . b) (,Y . a))
                      ((,X . b) (,Y . c))
                      ((,X . c) (,Y . a))
                      ((,X . c) (,Y . b)))))))

))
