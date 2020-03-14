(define-library (schemepunk debug indent json)
  (export json->indent)

  (import (scheme base)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk json)
          (schemepunk term-colors)
          (schemepunk debug indent))

  (begin
    (define (json->indent json)
      (match json
        (('object) (color yellow "{}"))
        (('object . pairs)
          (make-indent-group
            (color yellow "{")
            (snoc (map (λ x (make-indent-group
                               (make-colored-text (list
                                 (cons cyan (json->string (car x)))
                                 (cons yellow ": ")))
                               (list (json->indent (cdr x)))
                               (color yellow ",")))
                       (drop-right pairs 1))
                  (make-indent-group
                    (make-colored-text (list
                      (cons cyan (json->string (car (last pairs))))
                      (cons yellow ": ")))
                    (list (json->indent (cdr (last pairs))))
                    #f))
            (color yellow "}")))
        (is null? "[]")
        (is list?
          (make-indent-group
            "["
            (snoc (map (λ x (make-indent-group #f
                                               (list (json->indent x))
                                               ","))
                       (drop-right json 1))
                  (json->indent (last json)))
            "]"))
        (is string? (color green (json->string json)))
        (is number? (color magenta (json->string json)))
        ('true (color red "true"))
        ('false (color red "false"))
        ('null (color red "null"))
        (else "<NOT JSON>")))))
