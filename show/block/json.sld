(define-library (schemepunk show block json)
  (export json->block with-default-json-colors

          json-color-object
          json-color-object-key
          json-color-array
          json-color-string
          json-color-number
          json-color-true
          json-color-false
          json-color-null
          json-color-error)

  (import (scheme base)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk json)
          (schemepunk term-colors)
          (schemepunk show span)
          (schemepunk show block)
          (schemepunk show block datum))

  (begin
    (define json-color-object (make-parameter #f))
    (define json-color-object-key (make-parameter #f))
    (define json-color-array (make-parameter #f))
    (define json-color-string (make-parameter #f))
    (define json-color-number (make-parameter #f))
    (define json-color-true (make-parameter #f))
    (define json-color-false (make-parameter #f))
    (define json-color-null (make-parameter #f))
    (define json-color-error (make-parameter #f))

    (define (with-default-json-colors thunk)
      (parameterize ((json-color-object yellow)
                     (json-color-object-key cyan)
                     (json-color-array white)
                     (json-color-string green)
                     (json-color-number magenta)
                     (json-color-true red)
                     (json-color-false red)
                     (json-color-null red)
                     (json-color-error red))
        (thunk)))

    (define (json->block json)
      (match json
        (()
         (make-block
           (list (text-span "{}" (json-color-object)))))
        ((? pair?)
          (make-block
            (list (text-span "{" (json-color-object)))
            (snoc (append-map
                    (λ((k . v))
                      (list (make-block
                              (list (text-span (json->string k)
                                               (json-color-object-key))
                                    (text-span ":" (json-color-object))
                                    (whitespace-span))
                              (list (json->block v))
                              (list (text-span "," (json-color-object))))
                            (whitespace-span)))
                    (drop-right json 1))
                  (make-block
                    (list (text-span (json->string (car (last json)))
                                     (json-color-object-key))
                          (text-span ":" (json-color-object))
                          (whitespace-span))
                    (list (json->block (cdr (last json))))))
            (list (text-span "}" (json-color-object)))))
        (#()
          (make-block
            (list (text-span "[]" (json-color-array)))))
        ((? vector?)
          (let1 xs (vector->list json)
            (make-block
              (list (text-span "[" (json-color-array)))
              (snoc (append-map
                      (λ x
                        (list (make-block
                                '()
                                (list (json->block x))
                                (list (text-span "," (json-color-array))))
                              (whitespace-span)))
                      (drop-right xs 1))
                    (json->block (last xs)))
              (list (text-span "]" (json-color-array))))))
        ((? string?)
          (make-block
            (list (text-span (json->string json) (json-color-string)))))
        ((? number?)
          (make-block
            (list (text-span (json->string json) (json-color-number)))))
        ('true
          (make-block
            (list (text-span "true" (json-color-true)))))
        ('false
          (make-block
            (list (text-span "false" (json-color-false)))))
        ('null
          (make-block
            (list (text-span "null" (json-color-null)))))
        (datum
          (make-block
            (list (text-span "(NOT JSON:" (json-color-error))
                  (whitespace-span))
            (list (datum->block datum))
            (list (text-span ")" (json-color-error)))))))))
