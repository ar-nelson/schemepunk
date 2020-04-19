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
          (schemepunk syntax)
          (schemepunk debug)
          (schemepunk debug indent)
          (schemepunk term-colors))

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
        (define (test-error? e) (or (error-object? e) (instance? e Exception)))
        (define (write-test-error err port)
          (format port "Test raised error:~%~%")
          (if (error-object? err)
            (format port "~a~%message: ~a~%irritants: ~a"
              err
              (error-object-message err)
              (error-object-irritants err))
            (err:printStackTrace port)))))
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
           (guard (err ((test-error? err)
                         (fail-test name
                           (let ((str (open-output-string)))
                             (write-test-error err str)
                             (color red (get-output-string str)))))
                       (#t (fail-test name err)))
                  (begin body ...
                         (pass-test name))))))

    (define (write-indent)
      (for-each (lambda (_) (write-string "  ")) (current-suite)))

    (define (snoc xs x) (append xs (list x)))

    (define (pass-test name)
      (set! passed-count (+ passed-count 1))
      (write-indent)
      (write-colored green (string-append "✓ " name))
      (newline))

    (define (fail-test name err)
      (set! failed (snoc failed `(,(current-suite) ,name ,err)))
      (write-indent)
      (write-colored red (string-append "✗ " name))
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
                  (make-paragraph "Expected {0} but got {1}" (caddr err) (cadr err)))
               ((string? (cadr err))
                  (make-paragraph (cadr err)))
               ((or (colored-text? (cadr err)) (indent-group? (cadr err)))
                  (cadr err))
               (else
                  (form->indent (cadr err))))))
        ((or (colored-text? err) (indent-group? err))
          (make-report test err))
        (else
          (make-report test
            (color red "Test raised error:")
            (form->indent err)))))

    (define (end-test-runner)
      (write-colored green
        (string-append (number->string passed-count) " tests passed"))
      (newline)
      (unless (null? failed)
        (write-colored red
          (string-append (number->string (length failed)) " tests failed"))
        (newline)
        (write-reports (map (lambda (x) (apply failure->report x)) failed)))
      (exit (length failed)))))
