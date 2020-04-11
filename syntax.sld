(define-library (schemepunk syntax)
  (export λ
          ->
          ->>
          as->
          λ->
          λ->>
          let1
          let1-values
          one-of
          none-of
          compl
          dotimes
          and-let*
          match
          match?
          match-lambda
          match-lambda*
          matchλ
          match-let
          match-let*
          match-letrec
          match-let1
          match-guard)

  (import (scheme base))

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

  (begin
    (define-syntax λ
      (syntax-rules ()
        ((λ (args ...) . body) (lambda (args ...) . body))
        ((λ arg . body) (lambda (arg) . body))))

    (define-syntax ->
      (syntax-rules ()
        ((-> x) x)
        ((-> x (fn . args) . rest)
          (-> (fn x . args) . rest))
        ((-> x fn . rest)
          (-> (fn x) . rest))))

    (define-syntax ->>
      (syntax-rules ()
        ((->> x) x)
        ((->> x (fn args ...) . rest)
          (->> (fn args ... x) . rest))
        ((->> x fn . rest)
          (->> (fn x) . rest))))

    (define-syntax as->
      (syntax-rules ()
        ((as-> x name) x)
        ((as-> x name expr . rest)
          (as-> (let ((name x)) expr) name . rest))))

    (define-syntax λ->
      (syntax-rules ()
        ((λ-> . rest)
          (λ x (-> x . rest)))))

    (define-syntax λ->>
      (syntax-rules ()
        ((λ->> . rest)
          (λ x (->> x . rest)))))

    (define-syntax let1
      (syntax-rules ()
        ((let1 name value . body) (let ((name value)) . body))))

    (define-syntax let1-values
      (syntax-rules ()
        ((let1 names value . body) (let-values ((names value)) . body))))

    (define-syntax one-of
      (syntax-rules (is ?)
        ((one-of (is pred?)) pred?)
        ((one-of x)
          (λ y (eqv? x y)))
        ((one-of (is pred?) . xs)
          (λ y (or (pred? y) ((one-of . xs) y))))
        ((one-of (? pred?) . xs)
          (λ y (or (pred? y) ((one-of . xs) y))))
        ((one-of x . xs)
          (λ y (or (eqv? x y) ((one-of . xs) y))))))

    (define-syntax none-of
      (syntax-rules () ((none-of . xs) (compl (one-of . xs)))))

    (define (compl pred?)
      (λ(x) (not (pred? x))))

    (define-syntax dotimes
      (syntax-rules ()
        ((dotimes n . body)
           (let1 max-i n
             (do ((i 0 (+ i 1)))
                 ((>= i max-i))
                 . body)))))

    ;; match: A simple pattern-matching macro, based on Alex Shinn's
    ;; match-simple.scm, which is itself based on Andrew Wright's `match`.
    ;;
    ;; Most Schemes include a version of `match`, but they all have subtle
    ;; differences and incompatibilities. This version should behave the same on
    ;; all supported Schemes.
    ;;
    ;; Notably, this version of match does allow `...` as an ellipsis, because
    ;; Gerbil does not support alternate ellipsis symbols in syntax-rules[1].
    ;; (And because, even in other Schemes, using an alternate ellipsis is
    ;; tricky and difficult to make work.)
    ;; Valid ellipsis symbols are `___` and `…`.
    ;;
    ;; This is a from-scratch reimplementation that builds a cond expression.
    ;; The version in match-simple.scm breaks Kawa compilation for some reason.
    ;;
    ;; [1]: https://github.com/vyzo/gerbil/issues/412

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
          ; This magic expression comes from Alex Shinn's match-simple.scm.
          ; It tells symbols apart from other literals, in pure hygenic R7RS!
          (let-syntax ((sym? (syntax-rules ()
                               ((sym? x) #t)
                               ((sym? y) (equal? subject x)))))
            (sym? abracadabra)))))

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
          (->> body (match-body subject over pat) ...))
        ((_ subject over (or pat ...) body)
          (->> body (match-body subject over pat) ...))
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
        ((_ subject over name body) (match-body-let subject over name body)))))

  ; Gerbil Scheme is a special case for this macro. It doesn't allow _ as
  ; a macro keyword[2], and it has inconsistent support for the let-syntax
  ; trick that this macro depends on.
  ;
  ; What it *does* support is syntax-rules fenders[3]. Some Schemes allow
  ; syntax-rules clauses of the form (<pattern> <guard> <expression>), where
  ; <guard> is a boolean expression. This isn't part of R7RS, but we can use it
  ; in Gerbil to check for underscores and literal identifiers.
  ;
  ; [2]: https://github.com/vyzo/gerbil/issues/413
  ; [3]: http://www.r6rs.org/r6rs-editors/2006-August/001680.html
  (cond-expand
    (gerbil
      (import (only (gerbil core) defrules syntax underscore? identifier?))
      (begin
        (defrules match-body-let ()
          ((_ _ _ x body) (underscore? (syntax x)) body)
          ((_ subject ((arg mapped) . over) x body) (identifier? (syntax x))
            (match-body-let (map (λ arg subject) mapped) over x body))
          ((_ subject () x body) (identifier? (syntax x))
            (let1 x subject body))
          ((_ _ _ _ body) body))))
    (else
      (begin
        (define-syntax match-body-let
          (syntax-rules (_)
            ((_ subject over _ body) body)
            ((_ subject ((arg mapped) . over) x body)
              (match-body-let (map (λ arg subject) mapped) over x body))
            ((_ subject () x body)
              (let-syntax
                ((sym? (syntax-rules ()
                         ((sym? x) (let1 x subject body))
                         ((sym? y) body))))
                (sym? abracadabra))))))))

  (begin
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
            . body))))))
