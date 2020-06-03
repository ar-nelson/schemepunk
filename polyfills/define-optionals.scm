;; Chicken, Gambit, and Kawa support #!optional as a prefix for optional args,
;; but this is invalid syntax in most other Schemes, so it needs its own file.

(define-syntax %define-optionals
  (syntax-rules (:rest)
    ((_ name (optionals ... :rest (rest-pattern ...)) (params ...) _ body)
      (define (name params ... #!optional optionals ... #!rest rest)
        (assume (match? rest (rest-pattern ...)))
        (match-body rest () (rest-pattern ...) body)))
    ((_ name (optionals ... :rest rest-param) (params ...) _ body)
      (define (name params ... #!optional optionals ... #!rest rest-param)
        body))
    ((_ name (optionals ...) (params ...) _ body)
      (define (name params ... #!optional optionals ...)
        body))))
