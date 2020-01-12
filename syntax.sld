(define-library (schemepunk syntax)
  (export λ
          ->
          ->>
          as->
          λ->
          λ->>
          let1
          one-of
          none-of
          compl
          match
          match?
          match-let
          let-match
          let*-match)

  (import (scheme base)
          (scheme case-lambda))

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

    (define-syntax one-of
      (syntax-rules (is)
        ((one-of (is pred?)) pred?)
        ((one-of x)
           (λ y (eqv? x y)))
        ((one-of (is pred?) . xs)
           (λ y (or (pred? y) ((one-of . xs) y))))
        ((one-of x . xs)
           (λ y (or (eqv? x y) ((one-of . xs) y))))))

    (define-syntax none-of
      (syntax-rules () ((none-of . xs) (compl (one-of . xs)))))

    (define (compl pred?)
      (λ(x) (not (pred? x))))

    ;; match: A simple pattern-matching macro, similar to ML-family languages.
    ;;
    ;; Quoted values are literals, and are compared with 'eqv?'.
    ;; The keywords '=', 'equal', and 'is' can be used at the start of clauses.
    ;; (= x) matches when the subject is eqv? to x.
    ;; (equal x) matches when the subject is equal? to x.
    ;; (is? f) matches when the predicate f applied to the subject is true.

    (define-syntax match
      (syntax-rules (else)
        ((match subject (clause ...) ... (else . else-clause))
           (let ((var subject))
             (cond
               ((match-clause? var clause ...) (match-body var clause ...)) ...
               (else . else-clause))))
        ((match subject (clause ...) ...)
           (let ((var subject))
             (cond
               ((match-clause? var clause ...) (match-body var clause ...)) ...
               (else (error "match failed" var)))))))

    (define-syntax match-clause?
      (syntax-rules (= equal is)
        ((match-clause? subject = value . _) (eqv? subject value))
        ((match-clause? subject equal value . _) (equal? subject value))
        ((match-clause? subject is pred? . _) (pred? subject))
        ((match-clause? subject pattern . _) (match? subject pattern))))

    (define-syntax match?
      (syntax-rules (= quote equal is)
        ((match? subject ()) (null? subject))
        ((match? subject (= value)) (eqv? subject value))
        ((match? subject 'value) (eqv? subject 'value))
        ((match? subject (equal value)) (equal? subject value))
        ((match? subject (is pred? . _)) (pred? subject))
        ((match? subject (hd . tl))
           (and (pair? subject)
                (match? (car subject) hd)
                (match? (cdr subject) tl)))
        ((match? subject _) #t)))

    (define-syntax match-body
      (syntax-rules (= equal is)
        ((match-body _ = _ . body) (begin . body))
        ((match-body _ equal _ . body) (begin . body))
        ((match-body _ is _ . body) (begin . body))
        ((match-body subject pattern . body)
           (match-let ((subject pattern)) () . body))))

    (define-syntax match-let
      (syntax-rules (= quote equal is _)
        ((match-let () () . body) (begin . body))
        ((match-let () bindings . body) (let bindings . body))
        ((match-let ((subject (= x)) . rest) bindings . body)
           (match-let rest bindings . body))
        ((match-let ((subject 'x) . rest) bindings . body)
           (match-let rest bindings . body))
        ((match-let ((subject (equal x)) . rest) bindings . body)
           (match-let rest bindings . body))
        ((match-let ((subject (is pred?)) . rest) bindings . body)
           (match-let rest bindings . body))
        ((match-let ((subject (is pred? name)) . rest) bindings . body)
           (match-let_ subject name rest bindings body))
        ((match-let ((subject ()) . rest) bindings . body)
           (match-let rest bindings . body))
        ((match-let ((subject (hd . tl)) . rest) bindings . body)
           (match-let (((car subject) hd) . (((cdr subject) tl) . rest))
                      bindings
                      . body))
        ((match-let ((subject name) . rest) bindings . body)
           (match-let_ subject name rest bindings body))))

    (cond-expand
      ((library (gerbil core))
         ; The underscore is a reserved magic character in Gerbil.
         ; It cannot be used as a keyword in macros.
         ; However, (let ((_ foo))) treats the _ as a wildcard/ignore,
         ; so treating _ as a normal variable does what you'd expect.
         (define-syntax match-let_
           (syntax-rules ()
             ((match-let_ subject name rest bindings body)
                (match-let rest ((name subject) . bindings) . body)))))
      (else
         (define-syntax match-let_
           (syntax-rules (_)
             ((match-let_ subject _ rest bindings body)
                (match-let rest bindings . body))
             ((match-let_ subject name rest bindings body)
                (match-let rest ((name subject) . bindings) . body))))))

    ;; let-match: use 'match' patterns to destructure lists.

    (define-syntax let-match
      (syntax-rules ()
        ((let-match bindings . body) (%let-match bindings () . body))))

    (define-syntax %let-match
      (syntax-rules ()
        ((%let-match ((pattern value) . rest) named . body)
           (%let-match rest ((name pattern value) . named) . body))
        ((%let-match () ((name pattern value) ...) . body)
           (let ((name value) ...)
             (match-let ((name pattern) ...) ()  . body)))))

    (define-syntax let*-match
      (syntax-rules ()
        ((let*-match () . body) (begin . body))
        ((let*-match ((pattern value) . rest) . body)
           (let ((name value))
             (match-let ((name pattern)) ()
               (let*-match rest . body))))))))
