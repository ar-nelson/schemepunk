(define-library (schemepunk test)
  (export test-suite test end-test-runner
          assert-true assert-false
          assert-eq assert-eqv assert-equal assert-approximate
          fail failure?
          test-begin test-end test-group
          test-assert test-error
          test-eq test-eqv test-equal test-approximate
          current-test-comparator chibi-test-shim)

  (import (scheme base)
          (scheme write)
          (scheme process-context)
          (scheme cxr)
          (schemepunk syntax)
          (only (schemepunk list) snoc last drop-right)
          (schemepunk show)
          (schemepunk show report))

  (cond-expand
    (chicken
      (import (only (chicken condition) condition? print-error-message))
      (begin
        (define (test-error? e) (or (error-object? e) (condition? e)))
        (define write-test-error print-error-message)))
    (gauche
      (import (only (gauche base) report-error))
      (begin
        (define test-error? error-object?)
        (define write-test-error report-error)))
    (chibi
      (import (only (chibi) print-exception))
      (begin
        (define test-error? error-object?)
        (define (write-test-error err port)
          (parameterize ((current-error-port port))
            (print-exception err)))))
    (gerbil
      (import (only (gerbil core) exception? error-message error-irritants error-trace))
      (begin
        (define (test-error? e) (or (error-object? e) (exception? e)))
        (define (write-test-error err port)
          (format port "Test raised error:~%~%~a~%message: ~a~%irritants: ~a~%~a"
              err
              (error-message err)
              (error-irritants err)
              (error-trace err)))))
    (kawa
      (import (only (kawa reflect) instance?)
              (class java.lang Exception))
      (begin
        (define (test-error? e) (instance? e Exception))
        (define (write-test-error err port)
          (err:printStackTrace port))))
    ((library (rnrs conditions))
      (import (rnrs conditions))
      (begin
        (define (test-error? e) (or (error-object? e) (condition? e)))
        (define (write-test-error err port)
          (format port "Test raised error:~%~%~a" err)
          (cond
            ((condition? err)
              (when (who-condition? err)
                (format port "~%who: ~a" (condition-who err)))
              (when (message-condition? err)
                (format port "~%message: ~a" (condition-message err)))
              (when (irritants-condition? err)
                (format port "~%irritants: ~a" (condition-irritants err))))
            ((error-object? err)
              (format port "~%message: ~a~%irritants: ~a"
                (error-object-message err)
                (error-object-irritants err)))))))
    (else
      (begin
        (define test-error? error-object?)
        (define (write-test-error err port)
          (format port "Test raised error:~%~%~a~%message: ~a~%irritants: ~a"
            err
            (error-object-message err)
            (error-object-irritants err))))))

  (begin
    (define passed-count 0)
    (define failed '())
    (define current-test-group '())

    (define (indent)
      (make-string (* (length current-test-group) 2) #\space))

    (define (test-begin name)
      (show #t (indent) (as-underline name ":") nl)
      (set! current-test-group (snoc current-test-group name)))

    (define (test-end name)
      (cond
        ((null? current-test-group)
          (error "test-end without test-begin" (list name)))
        ((equal? (last current-test-group) name)
          (set! current-test-group (drop-right current-test-group 1)))
        (else
          (error "test-end name does not match test-begin"
                 (list name (last current-test-group))))))

    (define-syntax test-group
      (syntax-rules ()
        ((_ name . body)
           (let1 group-name name
             (test-begin group-name)
             (inline-defines . body)
             (test-end group-name)))))

    (define-syntax test-suite
      (syntax-rules ()
        ((_ . args) (test-group . args))))

    (define (pass-test name)
      (set! passed-count (+ passed-count 1))
      (show #t (indent) (as-green "✓ " name) nl))

    (define (fail-test name err)
      (set! failed (snoc failed `(,current-test-group ,name ,err)))
      (show #t (indent) (as-red "✗ " name) nl))

    (define-syntax test
      (syntax-rules ()
        ((test name body ...)
           (guard (err ((test-error? err)
                         (fail-test name
                           (let ((str (open-output-string)))
                             (write-test-error err str)
                             (as-red (get-output-string str)))))
                       (#t (fail-test name err)))
                  (begin (inline-defines body ...)
                         (pass-test name))))))

    (define (fail . xs)
      (raise `(test-failure ,@xs)))

    (define-syntax assert-true
      (syntax-rules ()
        ((assert-true condition)
          (unless condition (fail '(assert-true condition))))
        ((assert-true msg condition)
          (unless condition (fail msg)))))

    (define-syntax assert-false
      (syntax-rules ()
        ((assert-true condition)
          (when condition (fail '(assert-false condition))))
        ((assert-true msg condition)
          (when condition (fail msg)))))

    (define current-test-comparator (make-parameter equal?))

    (define (assert-eq actual expected)
      (parameterize ((current-test-comparator eq?))
        (assert-equal actual expected)))

    (define (assert-eqv actual expected)
      (parameterize ((current-test-comparator eqv?))
        (assert-equal actual expected)))

    (define (assert-equal actual expected)
      (unless ((current-test-comparator) actual expected)
        (fail actual expected)))

    (define (assert-approximate actual expected error)
      (unless (and (number? actual)
                   (>= actual (- expected error))
                   (<= actual (+ expected error)))
        (fail actual expected)))

    (define-syntax test-assert
      (syntax-rules ()
        ((_ name expr) (test name (assert-true expr)))
        ((_ expr) (test (format #f "~s" 'expr) (assert-true expr)))))

    (define-syntax test-eq
      (syntax-rules ()
        ((_ name expected actual)
          (test name
            (assert-eq actual expected)))
        ((_ expected actual)
          (test (format #f "~s" 'actual)
            (assert-eq actual expected)))))

    (define-syntax test-eqv
      (syntax-rules ()
        ((_ name expected actual)
          (test name
            (assert-eqv actual expected)))
        ((_ expected actual)
          (test (format #f "~s" 'actual)
            (assert-eqv actual expected)))))

    (define-syntax test-equal
      (syntax-rules ()
        ((_ name expected actual)
          (test name
            (assert-equal actual expected)))
        ((_ expected actual)
          (test (format #f "~s" 'actual)
            (assert-equal actual expected)))))

    (define-syntax test-approximate
      (syntax-rules ()
        ((_ name expected actual error)
          (test name
            (assert-approximate actual expected error)))
        ((_ expected actual error)
          (test (format #f "~s" 'actual)
            (assert-approximate actual expected error)))))

    (define-syntax test-error
      (syntax-rules ()
        ((_ name #t . body)
          (test name
            (guard (e ((not (failure? e)) #t))
              (begin . body)
              (fail "did not raise error"))))
        ((_ name expr)
          (test-error name #t expr))
        ((_ expr)
          (test-error (format #f "~s" 'expr) #t expr))))

    (define (failure? x)
      (and (pair? x) (eq? (car x) 'test-failure)))

    (define (reported-failure suite test err)
      (reported test
        (cond
          ((failure? err)
            (cond
              ((pair? (cddr err))
                (wrapped/blocks
                  "Expected "
                  (pretty-color (caddr err))
                  " but got "
                  (pretty-color (cadr err))))
              ((string? (cadr err))
                (wrapped (cadr err)))
              ((procedure? (cadr err))
                (cadr err))
              (else
                (pretty-color (cadr err)))))
          ((procedure? err)
            err)
          (else
            (wrapped/blocks
              (as-red "Test raised error: ")
              (pretty-color err))))))

    (define-syntax chibi-test-shim
      (syntax-rules ()
        ((_ test-name . chibi-test-body)
          (let-syntax
            ((test-name
               (syntax-rules ()
                 ((_ . args) (test-equal . args)))))
            . chibi-test-body))))

    (define (end-test-runner)
      (define failed-count (length failed))
      (show #t
        nl
        (as-bold (as-green
          passed-count
          (if (= 1 passed-count) " test" " tests")
          " passed"))
        nl)
      (unless (zero? failed-count)
        (show #t
          (as-bold (as-red
            failed-count
            (if (= 1 failed-count) " test" " tests")
            " failed"))
          nl
          (each-in-list (map (cut apply reported-failure <>) failed))
          report-line))
      (exit failed-count))))
