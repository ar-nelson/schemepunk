(define-library (schemepunk syntax)
  (export λ λ=>
          chain chain-and chain-when chain-lambda nest nest-reverse
          let1 let1-values
          inline-defines syntax-symbol-case
          one-of none-of dotimes

          with-input-from-string with-output-to-string
          and-let* receive cut cute format assume is isnt
          nonnegative-integer?

          match match?
          match-lambda match-lambda* matchλ
          match-let match-let* match-letrec match-let1
          match-guard

          define+)

  (import (scheme base)
          (scheme case-lambda)
          (scheme write)
          (schemepunk function))

  (cond-expand
    (gerbil
      (import (only (gerbil core)
                ; Gerbil has a built-in concept of "phi levels".
                ; Symbols can be imported at the expression level (phi=0)
                ; or the syntax level (phi=1), and (scheme base) is phi=0,
                ; so anything used in syntax must import from (gerbil core).
                lambda let cond else and define defrules
                eof-object? eof-object reverse null? car cdr cons error

                syntax-case syntax with-syntax genident defrules
                underscore? ellipsis? identifier? free-identifier=?))
      (begin-syntax
        ; Gerbil doesn't define quasisyntax!
        ; This is a sloppy polyfill, but it works well enough for SRFI 197.
        (defrules %qs (unsyntax)
          ((_ (out ...) ())
            (syntax (out ...)))
          ((_ (out ...) ((unsyntax x) . in))
            (with-syntax ((unsyntax-name x))
              (%qs (out ... unsyntax-name) in)))
          ((_ (out ...) ((x . xs) . in))
            (with-syntax ((sublist-name (%qs () (x . xs))))
              (%qs (out ... sublist-name) in)))
          ((_ (out ...) (x . in))
            (%qs (out ... x) in)))

        (defrules quasisyntax ()
          ((_ (x . xs)) (%qs () (x . xs)))
          ((_ x) (syntax x)))

        (define (syntax-violation name msg . rest)
          (error msg (cons name rest)))))
    (else))

  (cond-expand
    ((or chicken (library (srfi 2)))
      (import (srfi 2)))
    (else
      (begin
        (define-syntax and-let*
          (syntax-rules ()
            ((_ () . body) (begin . body))
            ((_ ((expr) . rest) . body)
              (and expr (and-let* rest . body)))
            ((_ ((name value) . rest) . body)
              (let ((name value))
                (and name (and-let* rest . body)))))))))

  (cond-expand
    ((or chicken (library (srfi 8)))
      (import (srfi 8)))
    ((library (std srfi 8))
      (import (std srfi 8)))
    (else
      (begin
        (define-syntax receive
          (syntax-rules ()
            ((receive formals expression body ...)
              (call-with-values (lambda () expression)
                                (lambda formals body ...))))))))

  (cond-expand
    ((or chicken (library (srfi 26)))
      (import (only (srfi 26) cut cute)))
    (else
      (begin
        (define-syntax %cut%
          (syntax-rules (<> <...>)
            ((_ () params call)
              (lambda params call))
            ((_ (<...>) (params ...) (callee . args))
              (lambda (params ... . rest-slot)
                (apply callee (append (list . args) rest-slot))))
            ((_ (<> . xs) (params ...) (call ...))
              (%cut% xs (params ... slot) (call ... slot)))
            ((_ (x . xs) params (call ...))
              (%cut% xs params (call ... x)))))

        (define-syntax cut
          (syntax-rules ()
            ((_ . xs) (%cut% xs () ()))))

        (define-syntax %cute%
          (syntax-rules (<> <...>)
            ((_ () pre-eval params call)
              (let pre-eval (lambda params call)))
            ((_ (<...>) pre-eval (params ...) (callee . args))
              (let pre-eval
                (lambda (params ... . rest-slot)
                  (apply callee (append (list . args) rest-slot)))))
            ((_ (<> . xs) pre-eval (params ...) (call ...))
              (%cute% xs pre-eval (params ... slot) (call ... slot)))
            ((_ (x . xs) pre-eval params (call ...))
              (%cute% xs ((evaluated x) . pre-eval) params (call ... evaluated)))))

        (define-syntax cute
          (syntax-rules ()
            ((_ . xs) (%cute% xs () () ())))))))

  (cond-expand
    (kawa
      (import (only (kawa base) format)))
    ((or gerbil larceny)
      (cond-expand
        (gerbil (import (only (std format) fprintf)))
        (larceny (import (rename (srfi 28) (format fprintf)))))
      (begin
        (define (format destination . rest)
          (case destination
            ((#t) (apply fprintf (cons (current-output-port) rest)))
            ((#f) (let ((str (open-output-string)))
                    (apply fprintf (cons str rest))
                    (get-output-string str)))
            (else (apply fprintf (cons destination rest)))))))
    ((or chicken (library (srfi 28)))
      (import (srfi 28)))
    (else
      (begin
        (define (format destination format-string . params)
          (define port (case destination
                         ((#t) (current-output-port))
                         ((#f) (open-output-string))
                         (else destination)))
          (define escape? #f)
          (string-for-each
            (lambda (c)
              (if escape?
                (begin
                  (set! escape? #f)
                  (case c
                    ((#\a #\A) (display (car params) port)
                               (set! params (cdr params)))
                    ((#\s #\S) (write (car params) port)
                               (set! params (cdr params)))
                    ((#\%) (newline port))
                    ((#\~) (write-char #\~ port))
                    (else (write-char #\~ port)
                          (write-char c port))))
                (case c
                  ((#\~) (set! escape? #t))
                  (else (write-char c port)))))
            format-string)
          (if destination #f (get-output-string port))))))

  (cond-expand
    (chicken
      (import (only (chicken base) assert))
      (begin
        (define-syntax assume
          (syntax-rules ()
            ((_ ok?) (assert ok?))
            ((_ ok? msg . _) (assert ok? msg))))))
    ((library (srfi 145))
      (import (srfi 145)))
    ((library (std srfi 145))
      (import (std srfi 145)))
    (else
      (begin
        (define-syntax assume
          (syntax-rules ()
            ((_ ok? . msgs)
              (unless ok?
                (error "invalid assumption" (list 'ok? . msgs)))))))))

  (cond-expand
    ((and (not chicken) (library (srfi 197)))
      (import (srfi 197)))
    (gerbil
      (include "polyfills/srfi-197-syntax-case.scm"))
    (else
      (include "polyfills/srfi-197-impl.scm")))

  ; Gerbil Scheme is a special case for several macro definitions.
  ; It doesn't allow _ as a macro keyword[1], and it has inconsistent support
  ; for the let-syntax trick for distinguishing symbols in macros.
  ;
  ; What it *does* support is syntax-rules fenders[2]. Some Schemes allow
  ; syntax-rules clauses of the form (<pattern> <guard> <expression>), where
  ; <guard> is a boolean expression. This isn't part of R7RS, but we can use it
  ; in Gerbil to check for underscores and literal identifiers.
  ;
  ; [1]: https://github.com/vyzo/gerbil/issues/413
  ; [2]: http://www.r6rs.org/r6rs-editors/2006-August/001680.html
  (cond-expand
    (gerbil
      (begin
        (define-syntax syntax-symbol-case
          (syntax-rules ()
            ((_ symbol . clauses)
              (%syntax-symbol-case symbol clauses ()))))

        (define-syntax %syntax-symbol-case
          (syntax-rules (underscore identifier else)
            ((_ symbol () clauses)
              (let-syntax ((symbol-case (syntax-rules () . clauses)))
                (symbol-case symbol)))
            ((_ symbol ((underscore result) . rest) (clauses ...))
              (%syntax-symbol-case symbol
                                   rest
                                   (clauses ... ((_ x) (underscore? (syntax x)) result))))
            ((_ symbol ((identifier result) . rest) (clauses ...))
              (%syntax-symbol-case symbol
                                   rest
                                   (clauses ... ((_ x) (identifier? (syntax x)) result))))
            ((_ symbol ((else result)) (clauses ...))
              (%syntax-symbol-case symbol () (clauses ... ((_ _) result))))))))
    (else
      (begin
        (define-syntax syntax-symbol-case
          (syntax-rules ()
            ((_ symbol . clauses)
              (%syntax-symbol-case symbol clauses () #f _))))

        (define-syntax %syntax-symbol-case
          (syntax-rules (underscore identifier else)
            ((_ symbol () clauses identifier-clause __)
              (let-syntax ((symbol-case (syntax-rules (__) . clauses)))
                (symbol-case symbol identifier-clause)))
            ((_ symbol ((underscore result) . rest) (clauses ...) identifier-clause __)
              (%syntax-symbol-case symbol
                                   rest
                                   ; Unfortunately, Kawa macros aren't completely hygenic!
                                   ; Recursive macro definitions leak symbol names,
                                   ; so using %__1 or %__2 inside this macro could cause errors.
                                   (clauses ... ((_ __ %__2) result))
                                   identifier-clause
                                   __))
            ((_ symbol ((identifier result) . rest) (clauses ...) _ __)
              (%syntax-symbol-case symbol
                                   rest
                                   (clauses ... ((_ %__1 symbol) symbol))
                                   result
                                   __))
            ((_ symbol ((else result)) (clauses ...) identifier-clause __)
              (%syntax-symbol-case symbol
                                   ()
                                   (clauses ... ((_ %__1 %__2) result))
                                   identifier-clause
                                   __)))))))

  (cond-expand
    ((and (not chicken) (library (srfi 156)))
      (import (srfi 156)))
    (else
      (begin
        (define-syntax %infix/postfix
          (syntax-rules ()
            ((_ x pred?)
              (pred? x))
            ((_ left pred? right)
              (pred? left right))
            ((_ left pred? right . rest)
              (let ((right* right))
                (and (%infix/postfix left pred? right*)
                     (%infix/postfix right* . rest)))))))

      (cond-expand
        (gerbil
          (begin
            (define-syntax %extract-placeholders
              (syntax-rules ()
                ((_ final () () body)
                  (final (%infix/postfix . body)))
                ((_ final () args body)
                  (lambda args (final (%infix/postfix . body))))
                ((_ final (u op . rest) (args ...) (body ...)) (underscore? (syntax u))
                  (%extract-placeholders final rest (args ... arg) (body ... arg op)))
                ((_ final (arg op . rest) args (body ...))
                  (%extract-placeholders final rest args (body ... arg op)))
                ((_ final (u) (args ...) (body ...)) (underscore? (syntax u))
                  (%extract-placeholders final () (args ... arg) (body ... arg)))
                ((_ final (arg) args (body ...))
                  (%extract-placeholders final () args (body ... arg)))))))
        (else
          (begin
            (define-syntax %extract-placeholders
              (syntax-rules (_)
                ((_ final () () body)
                  (final (%infix/postfix . body)))
                ((_ final () args body)
                  (lambda args (final (%infix/postfix . body))))
                ((_ final (_ op . rest) (args ...) (body ...))
                  (%extract-placeholders final rest (args ... arg) (body ... arg op)))
                ((_ final (arg op . rest) args (body ...))
                  (%extract-placeholders final rest args (body ... arg op)))
                ((_ final (_) (args ...) (body ...))
                  (%extract-placeholders final () (args ... arg) (body ... arg)))
                ((_ final (arg) args (body ...))
                  (%extract-placeholders final () args (body ... arg))))))))

      (begin
        (define-syntax %identity-syntax
          (syntax-rules ()
            ((_ form) form)))

        (define-syntax is
          (syntax-rules ()
            ((_ . args)
              (%extract-placeholders %identity-syntax args () ()))))

        (define-syntax isnt
          (syntax-rules ()
            ((_ . args)
              (%extract-placeholders not args () ())))))))

  (begin
    (define (nonnegative-integer? x)
      (and (number? x) (integer? x) (>= x 0)))

    (define-syntax λ=>
      (syntax-rules () ((_ . xs) (chain-lambda . xs))))

    (define-syntax let1
      (syntax-rules ()
        ((let1 name value . body) (let ((name value)) . body))))

    (define-syntax let1-values
      (syntax-rules ()
        ((let1 names value . body) (let-values ((names value)) . body))))

    (define-syntax inline-defines
      (syntax-rules (define define-values)
        ((_ x) x)
        ((_ (define (name . args) . body) . rest)
          (letrec ((name (lambda args . body)))
            (inline-defines . rest)))
        ((_ (define name value) . rest)
          (let ((name value))
            (inline-defines . rest)))
        ((_ (define-values names value) . rest)
          (let-values ((names value))
            (inline-defines . rest)))
        ((_ x . xs)
          (begin x (inline-defines . xs)))))

    (define-syntax one-of
      (syntax-rules (is ?)
        ((one-of (is pred?)) pred?)
        ((one-of x)
          (lambda (y) (eqv? x y)))
        ((one-of (is pred?) . xs)
          (lambda (y) (or (pred? y) ((one-of . xs) y))))
        ((one-of (? pred?) . xs)
          (lambda (y) (or (pred? y) ((one-of . xs) y))))
        ((one-of x . xs)
          (lambda (y) (or (eqv? x y) ((one-of . xs) y))))))

    (define-syntax none-of
      (syntax-rules () ((none-of . xs) (complement (one-of . xs)))))

    (define-syntax dotimes
      (syntax-rules ()
        ((dotimes n . body)
           (let1 max-i n
             (do ((i 0 (+ i 1)))
                 ((>= i max-i))
                 . body)))))

    (define (with-input-from-string str thunk)
      (parameterize ((current-input-port (open-input-string str)))
        (thunk)))

    (define (with-output-to-string thunk)
      (parameterize ((current-output-port (open-output-string)))
        (thunk)
        (get-output-string (current-output-port))))

    ;; match: A simple pattern-matching macro, based on Alex Shinn's
    ;; match-simple.scm, which is itself based on Andrew Wright's `match`.
    ;;
    ;; Most Schemes include a version of `match`, but they all have subtle
    ;; differences and incompatibilities. This version should behave the same on
    ;; all supported Schemes.
    ;;
    ;; Notably, this version of match does allow `...` as an ellipsis, because
    ;; Gerbil does not support alternate ellipsis symbols in syntax-rules[3].
    ;; (And because, even in other Schemes, using an alternate ellipsis is
    ;; tricky and difficult to make work.)
    ;; Valid ellipsis symbols are `___` and `…`.
    ;;
    ;; This is a from-scratch reimplementation that builds a cond expression.
    ;; The version in match-simple.scm breaks Kawa compilation for some reason.
    ;;
    ;; [3]: https://github.com/vyzo/gerbil/issues/412

    (define-syntax match
      (syntax-rules (else)
        ((match subject (pattern . body) ... (else . else-clause))
          (let1 var subject
            (cond
              ((match? var pattern) (match-body var () pattern (begin . body))) ...
              (else . else-clause))))
        ((match subject (pattern . body) ...)
          (let1 var subject
            (cond
              ((match? var pattern) (match-body var () pattern (begin . body))) ...
              (else (error "match failed"
                           var
                           '(pattern ...))))))))

    (define-syntax match?
      (syntax-rules (? quote quasiquote unquote unquote-splicing and or not ___ …)
        ((_ subject ()) (null? subject))
        ((_ subject 'value) (equal? subject 'value))
        ((_ subject `,x) (match? subject x))
        ((_ subject `(,@x)) (match? subject x))
        ((_ subject `(,@x . _))
          (syntax-error ",@ pattern can only occur at the end of a list"))
        ((_ subject `(hd . tl))
          (and (pair? subject)
               (match? (car subject) `hd)
               (match? (cdr subject) `tl)))
        ((_ subject `value) (equal? subject 'value))
        ((_ subject (and x ...)) (and (match? subject x) ...))
        ((_ subject (or x ...)) (or (match? subject x) ...))
        ((_ subject (not x)) (not (match? subject x)))
        ((_ subject (? pred? . _)) (pred? subject))
        ((_ subject (pattern …)) (match? subject (pattern ___)))
        ((_ subject (pattern ___))
          (and (list? subject)
               (let loop ((x subject))
                 (or (null? x)
                     (and (match? (car x) pattern) (loop (cdr x)))))))
        ((_ subject (hd . tl))
          (and (pair? subject)
               (match? (car subject) hd)
               (match? (cdr subject) tl)))
        ((_ subject #(pat ...))
          (and (vector? subject)
               (match? (vector->list subject) (pat ...))))
        ((_ subject x)
          (syntax-symbol-case x
            (underscore #t)
            (identifier #t)
            (else (equal? subject x))))))

    (define-syntax match-body
      (syntax-rules (? quote quasiquote unquote unquote-splicing and or not ___ …)
        ((_ _ _ () body) body)
        ((_ _ _ 'x body) body)
        ((_ subject over `,x body) (match-body subject over x body))
        ((_ subject over `(,@x) body) (match-body subject over x body))
        ((_ subject over `(hd . tl) body)
          (match-body (car subject) over `hd
            (match-body (cdr subject) over `tl body)))
        ((_ _ _ `x body) body)
        ((_ _ _ (? _) body) body)
        ((_ subject over (? _ name) body) (match-body-let subject over name body))
        ((_ subject over (and pat ...) body)
          (nest %__placeholder (match-body subject over pat %__placeholder) ... body))
        ((_ subject over (or pat ...) body)
          (nest %__placeholder (match-body subject over pat %__placeholder) ... body))
        ((_ _ _ (not _) body) body)
        ((_ subject over (pat ___) body)
          (match-body ellipsis ((ellipsis subject) . over) pat body))
        ((_ subject over (pat …) body)
          (match-body ellipsis ((ellipsis subject) . over) pat body))
        ((_ subject over (hd . tl) body)
          (match-body (car subject) over hd
            (match-body (cdr subject) over tl body)))
        ((_ subject over #(pat ...) body)
          (match-body (vector->list subject) over (pat ...) body))
        ((_ subject over name body) (match-body-let subject over name body))))

    (define-syntax match-body-let
      (syntax-rules ()
        ((_ subject ((arg mapped) . over) x body)
          (syntax-symbol-case x
            (underscore body)
            (else (match-body-let (map (λ arg subject) mapped) over x body))))
        ((_ subject () x body)
          (syntax-symbol-case x
            (underscore body)
            (identifier (let1 x subject body))
            (else body)))))

    (define-syntax match-lambda-body
      (syntax-rules ()
        ((_ () args body) (lambda args body))
        ((_ (pattern . rest) (args ...) body)
          (match-lambda-body
            rest
            (args ... arg)
            (match-body arg () pattern body)))))

    (define-syntax λ
      (syntax-rules ()
        ((λ (patterns ...) . body)
          (match-lambda-body (patterns ...) () (begin . body)))
        ((λ arg . body)
          (lambda (arg) . body))))

    (define-syntax match-lambda
      (syntax-rules ()
        ((_ clause ...) (lambda (expr) (match expr clause ...)))))

    (define-syntax match-lambda*
      (syntax-rules ()
        ((_ clause ...) (lambda expr (match expr clause ...)))))

    (define-syntax matchλ
      (syntax-rules () ((_ . xs) (match-lambda . xs))))

    (define-syntax match-let*
      (syntax-rules ()
        ((_ ((pat expr) . rest) . body)
          (match-body expr () pat (match-let* rest . body)))
        ((_ () . body)
          (begin . body))))

    (define-syntax match-let
      (syntax-rules ()
        ((_ (vars ...) . body) (match-let* (vars ...) body))
        ((_ loop . rest) (match-named-let loop () . rest))))

    (define-syntax match-named-let
      (syntax-rules ()
        ((_ loop ((pat expr var) ...) () . body)
          (let loop ((var expr) ...)
            (match-let ((pat var) ...)
              . body)))
        ((_ loop (v ...) ((pat expr) . rest) . body)
          (match-named-let loop (v ... (pat expr tmp)) rest . body))))

    (define-syntax match-letrec
      (syntax-rules ()
        ((_ vars . body) (match-letrec-helper () vars . body))))

    (define-syntax match-letrec-helper
      (syntax-rules ()
        ((_ ((pat expr var) ...) () . body)
          (letrec ((var expr) ...)
            (match-let ((pat var) ...)
              . body)))
        ((_ (v ...) ((pat expr) . rest) . body)
          (match-letrec-helper (v ... (pat expr tmp)) rest . body))))

    (define-syntax match-let1
      (syntax-rules ()
        ((_ pat expr . body) (match-body expr () pat (begin . body)))))

    (define-syntax match-guard
      (syntax-rules (else)
        ((_ ((pattern . clause) ... (else . else-clause)) . body)
          (guard (err ((match? err pattern)
                        (match-body err () pattern (begin . clause))) ...
                      (else . else-clause))
            . body))
        ((_ ((pattern . clause) ...) . body)
          (guard (err ((match? err pattern)
                        (match-body err () pattern (begin . clause))) ...)
            . body)))))

  (cond-expand
    (chicken
      ; This should work in Kawa too, but, mysteriously, it doesn't.
      (include "polyfills/define-optionals.scm"))
    (gerbil
      (import (only (gerbil core) def))
      (begin
        (define-syntax %define-optionals
          (syntax-rules (:rest)
            ((_ name () params _ body)
              (def (name . params) body))
            ((_ name (:rest (rest-pattern ...)) (params ...) _ body)
              (def (name params ... . rest)
                (assume (match? rest (rest-pattern ...)))
                (match-body rest () (rest-pattern ...) body)))
            ((_ name (:rest rest-param) (params ...) _ body)
              (def (name params ... . rest-param) body))
            ((_ name ((param default) . rest) (params ...) _ body)
              (%define-optionals name
                                 rest
                                 (params ... (param default))
                                 ()
                                 body))
            ((_ name (param . rest) (params ...) _ body)
              (%define-optionals name
                                 rest
                                 (params ... (param #f))
                                 ()
                                 body))))))

    ; This Gauche macro *should* work. It doesn't, and I have no idea why.
    #;(gauche
      (begin
        (define-syntax %define-optionals
          (syntax-rules (:rest)
            ((_ name (optionals ... :rest (rest-pattern ...)) (params ...) _ body)
              (define (name params ... :optional optionals ... :rest rest)
                (assume (match? rest (rest-pattern ...)))
                (match-body rest () (rest-pattern ...) body)))
            ((_ name (optionals ... :rest rest-param) (params ...) _ body)
              (define (name params ... :optional optionals ... :rest rest-param)
                body))
            ((_ name (optionals ...) (params ...) _ body)
              (define (name params ... :optional optionals ...)
                body))))))

    (else
      (begin
        (define-syntax %define-optionals
          (syntax-rules (:rest)
            ((_ name () params (overloads ...) body)
              (define name (case-lambda overloads ... (params body))))
            ((_ name (:rest (rest-pattern ...)) (params ...) (overloads ...) body)
              (define name
                (case-lambda
                  overloads ...
                  ((params ... . rest)
                    (assume (match? rest (rest-pattern ...)))
                    (match-body rest () (rest-pattern ...) body)))))
            ((_ name (:rest rest-param) (params ...) (overloads ...) body)
              (define name (case-lambda overloads ... ((params ... . rest-param) body))))
            ((_ name ((param default) . rest) (params ...) (overloads ...) body)
              (%define-optionals name
                                 rest
                                 (params ... param)
                                 (overloads ... ((params ...) (name params ... default)))
                                 body))
            ((_ name (param . rest) (params ...) (overloads ...) body)
              (%define-optionals name
                                 rest
                                 (params ... param)
                                 (overloads ... ((params ...) (name params ... #f)))
                                 body)))))))

  (begin
    (define-syntax %define-params
      (syntax-rules (:optional :rest)
        ((_ name () params body)
          (define (name . params) body))
        ((_ name (:rest (rest-pattern ...)) (params ...) body)
          (define (name params ... . rest)
            (assume (match? rest (rest-pattern ...)))
            (match-body rest () (rest-pattern ...) body)))
        ((_ name (:rest rest-param) (params ...) body)
          (define (name params ... . rest-param) body))
        ((_ name (:optional . rest) params body)
          (%define-optionals name rest params () body))
        ((_ name ((pattern ...) . rest) (params ...) body)
          (%define-params name rest (params ... arg)
            (begin
              (assume (match? arg (pattern ...)))
              (match-body arg () (pattern ...) body))))
        ((_ name (param . rest) (params ...) body)
          (%define-params name rest (params ... param) body))))

    (define-syntax define+
      (syntax-rules ()
        ((_ (name . params) . body)
          (%define-params name params () (begin . body)))))))
