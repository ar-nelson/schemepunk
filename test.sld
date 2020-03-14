(define-library (schemepunk test)
  (export test-suite
          test
          end-test-runner
          assert-true
          assert-false
          assert-eq
          assert-eqv
          assert-equal
          fail
          failure?)

  (import (scheme base)
          (scheme write)
          (scheme process-context)
          (scheme cxr)
          (schemepunk debug)
          (schemepunk debug indent)
          (schemepunk term-colors))

  (cond-expand
    (gauche (import (only (gauche base) report-error)))
    (chibi (import (only (chibi) print-exception)))
    (else))

  (begin
    (define passed-count 0)
    (define failed '())
    (define current-suite (make-parameter '()))

    (define-syntax test-suite
      (syntax-rules ()
        ((test-suite name body ...)
           (begin (write-indent)
                  (write-string (string-append name ":"))
                  (newline)
                  (parameterize ((current-suite (snoc (current-suite) name)))
                    body ...)))))

    (define-syntax test
      (syntax-rules ()
        ((test name body ...)
           ;; Error stack traces must be printed directly here.
           ;; They cannot be printed inside of a function or macro,
           ;; or (at least in Gauche) that will become part of the trace.
           (guard (err ((cond-expand
                          (gerbil (or (error-object? err) (exception? err)))
                          (else (error-object? err)))
                        (fail-test name
                          (cond-expand
                            (gauche
                              (let ((str (open-output-string)))
                                (report-error err str)
                                (color red (get-output-string str))))
                            (chibi
                              (let ((str (open-output-string)))
                                (parameterize ((current-error-port str))
                                  (print-exception err))
                                (color red (get-output-string str))))
                            (gerbil
                              (let ((str (open-output-string)))
                                (display-exception err str)
                                (color red (get-output-string str))))
                            (else err))))
                       (#t (fail-test name err)))
                  (begin body ...
                         (pass-test name))))))

    (define (write-indent)
      (for-each (lambda (_) (write-string "  ")) (current-suite)))

    (define (snoc xs x) (append xs (list x)))

    (define (pass-test name)
      (set! passed-count (+ passed-count 1))
      (write-indent)
      (write-string (green (string-append "✓ " name)))
      (newline))

    (define (fail-test name err)
      (set! failed (snoc failed `(,(current-suite) ,name ,err)))
      (write-indent)
      (write-string (red (string-append "✗ " name)))
      (newline))

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

    (define (assert-eq actual expected)
      (unless (eq? actual expected)
        (fail (form->indent actual) (form->indent expected))))

    (define (assert-eqv actual expected)
      (unless (eqv? actual expected)
        (fail (form->indent actual) (form->indent expected))))

    (define (assert-equal actual expected)
      (unless (equal? actual expected)
        (fail (form->indent actual) (form->indent expected))))

    (define (fail . xs)
      (raise `(test-failure ,@xs)))

    (define (failure? x)
      (and (pair? x) (eq? (car x) 'test-failure)))

    (define (failure->report suite test err)
      (cond
        ((failure? err)
           (make-report test
             (cond
               ((pair? (cddr err))
                  (make-paragraph "Expected {0} but got {1}" (cadr err) (caddr err)))
               ((string? (car err))
                  (make-paragraph (car err)))
               ((or (colored-text? (car err)) (indent-group? (car err)))
                  (car err))
               (else
                  (form->indent (car err))))))
        ((or (colored-text? err) (indent-group? err))
          (make-report test err))
        (else
          (make-report test
            (color red "Test raised error:")
            (form->indent err)))))

    (define (end-test-runner)
      (write-string
        (green (string-append (number->string passed-count) " tests passed")))
      (newline)
      (unless (null? failed)
        (write-string
          (red (string-append (number->string (length failed)) " tests failed")))
        (newline)
        (write-reports (map (lambda (x) (apply failure->report x)) failed)))
      (exit (length failed)))))
