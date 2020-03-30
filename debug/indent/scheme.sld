(define-library (schemepunk debug indent scheme)
  (export form->indent

          color-scheme-list
          color-scheme-vector
          color-scheme-set
          color-scheme-hash-table
          color-scheme-symbol
          color-scheme-string
          color-scheme-number
          color-scheme-record
          color-scheme-special)

  (import (scheme base)
          (scheme write)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk vector)
          (schemepunk set)
          (schemepunk hash-table)
          (schemepunk term-colors)
          (schemepunk debug indent))

  (cond-expand
    ((library (srfi 99))
       (import (only (srfi 99) record?
                               record-rtd
                               rtd-name
                               rtd-all-field-names
                               rtd-accessor)))
    (else
       (begin (define (record? x) #f))))

  (begin
    (define color-scheme-list (make-parameter cyan))
    (define color-scheme-vector (make-parameter cyan))
    (define color-scheme-set (make-parameter yellow))
    (define color-scheme-hash-table (make-parameter yellow))
    (define color-scheme-symbol (make-parameter white))
    (define color-scheme-string (make-parameter green))
    (define color-scheme-number (make-parameter magenta))
    (define color-scheme-record (make-parameter yellow))
    (define color-scheme-special (make-parameter red))

    (define (list->indents xs)
      (if (proper-list? xs)
          (map form->indent xs)
          (let loop ((contents '()) (next xs))
            (if (pair? next)
                (loop (cons (form->indent (car next)) contents) (cdr next))
                (reverse `(,(form->indent next)
                           ,(color (color-scheme-list) ".")
                           ,@contents))))))

    (define (form->indent form)
      (match form
        (((? symbol? head) x . xs)
           (make-indent-group
             (make-colored-text
               `((,(color-scheme-list) . "(")
                 (,(color-scheme-symbol) . ,(symbol->string head))
                 (#f . " ")))
             (cons (form->indent x) (list->indents xs))
             (color (color-scheme-list) ")")))
        (() (color (color-scheme-list) "()"))
        ((? pair?)
           (make-indent-group
             (color (color-scheme-list) "(")
             (list->indents form)
             (color (color-scheme-list) ")")))
        ((? vector?)
           (make-indent-group
             (color (color-scheme-vector) "#(")
             (list->indents (vector->list form))
             (color (color-scheme-vector) ")")))
        ((? set?)
           (make-indent-group
             (color (color-scheme-set) "#<[")
             (list->indents (set->list form))
             (color (color-scheme-set) "]>")))
        ((? hash-table?)
           (make-indent-group
             (color (color-scheme-hash-table) "#<{")
             (map (λ x (make-indent-group
                         (make-indent-group
                           #f
                           (list (form->indent (car x)))
                           (color (color-scheme-hash-table) ":"))
                         (list (form->indent (cdr x)))
                         #f))
                  (hash-table->alist form))
             (color (color-scheme-hash-table) "}>")))
        ((? symbol?) (color (color-scheme-symbol) (symbol->string form)))
        ((? string?)
           (let1 str (open-output-string)
             (write form str)
             (color (color-scheme-string) (get-output-string str))))
        ((? number?)
           (color (color-scheme-number) (number->string form)))
        ((? record?)
           (cond-expand
             ((library (srfi 99))
                (let* ((rtd (record-rtd form))
                       (name (symbol->string (rtd-name rtd)))
                       (fields (vector->list (rtd-all-field-names rtd))))
                  (if (null? fields)
                    (color (color-scheme-record) (string-append "#<" name ">"))
                    (make-indent-group
                      (color (color-scheme-record) (string-append "#<" name " "))
                      (map (λ field
                             (make-indent-group
                               (color (color-scheme-record)
                                 (string-append (symbol->string field) ":"))
                               (list (form->indent
                                       ((rtd-accessor rtd field) form)))
                               #f))
                           fields)
                      (color (color-scheme-record) ">")))))
             (else (error "not implemented"))))
        (else
           (let1 str (open-output-string)
             (write form str)
             (color (color-scheme-special) (get-output-string str))))))))
