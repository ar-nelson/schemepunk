; Missing R7RS macros in Gambit

(define-macro (guard/gambit-patched handler . body)
  (let* ((var (car handler))
         (clauses (cdr handler))
         (else-clause (or (assq 'else clauses) (assq #t clauses))))
      `(##with-exception-catcher
         (lambda (,var)
           ,(if else-clause
              `(cond ,@clauses)
              `(cond ,@clauses (else (raise ,var)))))
         (lambda () ,@body))))

(define-syntax let*-values/gambit-patched
  (syntax-rules ()
    ((_ () . body) (begin . body))
    ((_ ((names value) . rest) . body)
      (call-with-values (lambda () value)
                        (lambda names (let*-values/gambit-patched rest . body))))))

(define-syntax let-values/gambit-patched
  (syntax-rules ()
    ((_ . xs) (let*-values/gambit-patched . xs))))
