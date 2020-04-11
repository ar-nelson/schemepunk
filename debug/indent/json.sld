(define-library (schemepunk debug indent json)
  (export json->indent

          color-json-object
          color-json-object-key
          color-json-array
          color-json-string
          color-json-number
          color-json-true
          color-json-false
          color-json-null
          color-json-error)

  (import (scheme base)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk vector)
          (schemepunk json)
          (schemepunk term-colors)
          (schemepunk debug indent))

  (begin
    (define color-json-object (make-parameter yellow))
    (define color-json-object-key (make-parameter cyan))
    (define color-json-array (make-parameter white))
    (define color-json-string (make-parameter green))
    (define color-json-number (make-parameter magenta))
    (define color-json-true (make-parameter red))
    (define color-json-false (make-parameter red))
    (define color-json-null (make-parameter red))
    (define color-json-error (make-parameter red))

    (define (json->indent json)
      (match json
        (() (color (color-json-object) "{}"))
        ((? list?)
          (make-indent-group
            (color (color-json-object) "{")
            (snoc (map (λ x (make-indent-group
                               (make-colored-text (list
                                 (cons (color-json-object-key)
                                       (json->string (car x)))
                                 (cons (color-json-object) ": ")))
                               (list (json->indent (cdr x)))
                               (color (color-json-object) ",")))
                       (drop-right json 1))
                  (make-indent-group
                    (make-colored-text (list
                      (cons (color-json-object-key)
                            (json->string (car (last json))))
                      (cons (color-json-object) ": ")))
                    (list (json->indent (cdr (last json))))
                    #f))
            (color (color-json-object) "}")))
        (#() (color (color-json-array) "[]"))
        ((? vector?)
          (let1 xs (vector->list json)
            (make-indent-group
              (color (color-json-array) "[")
              (snoc (map (λ x (make-indent-group #f
                                                 (list (json->indent x))
                                                 (color (color-json-array) ",")))
                         (drop-right xs 1))
                    (json->indent (last xs)))
              (color (color-json-array) "]"))))
        ((? string?) (color (color-json-string) (json->string json)))
        ((? number?) (color (color-json-number) (json->string json)))
        ('true (color (color-json-true) "true"))
        ('false (color (color-json-false) "false"))
        ('null (color (color-json-null) "null"))
        (else (color (color-json-error) "<NOT JSON>"))))))
