(define-library (schemepunk test)
  (export test-suite
          test
          begin-test-suite
          end-test-suite
          end-test-runner
          pass-test
          fail-test
          assert-true
          assert-false
          assert-eq
          assert-eqv
          assert-equal
          fail
          failure?)

  (import (scheme base)
          (scheme write)
          (scheme process-context))

  (cond-expand
    ((library (gauche base))
       (import (only (gauche base) report-error))
       (begin
         (define-syntax error-string
           (syntax-rules ()
             ((error-string err)
                (let ((str (open-output-string)))
                  (report-error err str)
                  (get-output-string str)))))))
    ((library (chibi))
       (import (only (chibi) print-exception))
       (begin
         (define-syntax error-string
           (syntax-rules ()
             ((error-string err)
                (let ((str (open-output-string)))
                  (parameterize ((current-error-port str))
                    (print-exception err))
                  (get-output-string str)))))))
    (else
      (begin (define (error-string err) 
               (string-append red "ERROR: " reset (error-object-message err))))))

  (begin
    (define passed-count 0)
    (define failed '())
    (define current-suite '())

    (define-syntax test-suite
      (syntax-rules ()
        ((test-suite name body ...)
           (begin (begin-test-suite name)
                  body ...
                  (end-test-suite name)))))

    (define-syntax test
      (syntax-rules ()
        ((test name body ...)
           (guard (err ((failure? err) (fail-test name err))
                       ((error-object? err) (fail-test name `(error ,(error-string err))))
                       (else (fail-test name `(error ,err))))
                  (begin body ...
                         (pass-test name))))))

    (define (indent)
      (define str "")
      (for-each (lambda (_) (set! str (string-append str "  "))) current-suite)
      str)

    (define (snoc xs x) (append xs (list x)))

    (define (begin-test-suite name)
      (display (string-append (indent) name ":"))
      (newline)
      (set! current-suite (snoc current-suite name)))

    (define (end-test-suite name)
      (set! current-suite (cdr current-suite)))

    (define green (string #\x001b #\[ #\3 #\2 #\m))
    (define red (string #\x001b #\[ #\3 #\1 #\m))
    (define reset (string #\x001b #\[ #\0 #\m))

    (define (pass-test name)
      (set! passed-count (+ passed-count 1))
      (display (string-append (indent) green "✓ " name reset))
      (newline))

    (define (fail-test name err)
      (set! failed (snoc failed `(,current-suite ,name ,@(cdr err))))
      (display (string-append (indent) red "✗ " name reset))
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
      (unless (eq? actual expected) (fail actual expected)))

    (define (assert-eqv actual expected)
      (unless (eqv? actual expected) (fail actual expected)))

    (define (assert-equal actual expected)
      (unless (equal? actual expected) (fail actual expected)))

    (define (fail . xs)
      (raise `(test-failure ,@xs)))

    (define (failure? x)
      (and (pair? x) (eq? (car x) 'test-failure)))

    (define (print-failure suite name cause . rest)
      (newline)
      (display (string-append red "Failure: " name reset))
      (newline)
      (cond
        ((pair? rest)
           (let ((prn (if (and (pair? (cdr rest)) (eqv? (cadr rest) 'raw))
                          display
                          write)))
             (display (string-append red "expected " reset))
             (prn (car rest))
             (newline)
             (display (string-append red " but got " reset))
             (prn cause)))
        (else (display cause)))
      (newline))

    (define (end-test-runner)
      (display (string-append
        green (number->string passed-count) " tests passed" reset))
      (newline)
      (if (pair? failed) (begin
        (display (string-append
          red (number->string (length failed)) " tests failed" reset))
        (newline)
        (for-each (lambda (err) (apply print-failure err)) failed)))
      (exit (length failed)))))
