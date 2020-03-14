(define-library (schemepunk debug indent scheme)
  (export form->indent)

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
    (define (list->indents xs)
      (if (proper-list? xs)
          (map form->indent xs)
          (let loop ((contents '()) (next xs))
            (if (pair? next)
                (loop (cons (form->indent (car next)) contents) (cdr next))
                (reverse (cons (form->indent next)
                               (cons (color cyan ".") contents)))))))

    (define (form->indent form)
      (match form
        (((is symbol? head) x . xs)
           (make-indent-group
             (make-colored-text
               `((,cyan . "(")
                 (#f . ,(symbol->string head))
                 (#f . " ")))
             (cons (form->indent x) (list->indents xs))
             (color cyan ")")))
        (() (color cyan "()"))
        (is pair?
           (make-indent-group
             (color cyan "(")
             (list->indents form)
             (color cyan ")")))
        (is vector?
           (make-indent-group
             (color cyan "#(")
             (list->indents (vector->list form))
             (color cyan ")")))
        (is set?
           (make-indent-group
             (color yellow "#<[")
             (list->indents (set->list form))
             (color yellow "]>")))
        (is hash-table?
           (make-indent-group
             (color yellow "#<{")
             (map (λ x (make-indent-group
                         (make-indent-group
                           #f
                           (list (form->indent (car x)))
                           (color yellow ":"))
                         (list (form->indent (cdr x)))
                         #f))
                  (hash-table->alist form))
             (color yellow "}>")))
        (is symbol? (symbol->string form))
        (is string?
           (let1 str (open-output-string)
             (write form str)
             (color green (get-output-string str))))
        (is number?
           (color magenta (number->string form)))
        (is record?
           (cond-expand
             ((library (srfi 99))
                (let* ((rtd (record-rtd form))
                       (name (symbol->string (rtd-name rtd)))
                       (fields (vector->list (rtd-all-field-names rtd))))
                  (if (null? fields)
                    (color yellow (string-append "#<" name ">"))
                    (make-indent-group
                      (color yellow (string-append "#<" name " "))
                      (map (λ field
                             (make-indent-group
                               (color yellow
                                 (string-append (symbol->string field) ":"))
                               (list (form->indent
                                       ((rtd-accessor rtd field) form)))
                               #f))
                           fields)
                      (color yellow ">")))))
             (else (error "not implemented"))))
        (else
           (let1 str (open-output-string)
             (write form str)
             (color red (get-output-string str))))))))
