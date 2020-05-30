(define-library (schemepunk debug indent scheme)
  (export form->indent list->indents
          register-datatype-debug-writer!

          color-scheme-list
          color-scheme-vector
          color-scheme-structure
          color-scheme-symbol
          color-scheme-string
          color-scheme-number
          color-scheme-record
          color-scheme-special)

  (import (scheme base)
          (scheme write)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk term-colors)
          (schemepunk debug indent))

  (cond-expand
    ((or chicken (library (srfi 99)))
       (import (only (srfi 99) record?
                               record-rtd
                               rtd-name
                               rtd-all-field-names
                               rtd-accessor)))
    (kawa
      (import (only (kawa base) record?)
              (class kawa.lang Record))
      (begin
        (define (record-rtd record::Record)
          (cons (string->symbol (record:getTypeName))
                (list->vector (Record:typeFieldNames record:class))))
        (define rtd-name car)
        (define rtd-all-field-names cdr)
        (define (rtd-accessor rtd name)
          (lambda (record::Record)
            (record:get (symbol->string name) #f)))))
    (else
       (begin (define (record? _) #f)
              (define-syntax record-rtd (syntax-rules () ((_ . _) #f)))
              (define (rtd-name _) #f)
              (define (rtd-all-field-names _) #f)
              (define (rtd-accessor x y) #f))))

  (begin
    (define color-scheme-list (make-parameter cyan))
    (define color-scheme-vector (make-parameter cyan))
    (define color-scheme-structure (make-parameter yellow))
    (define color-scheme-symbol (make-parameter white))
    (define color-scheme-string (make-parameter green))
    (define color-scheme-number (make-parameter magenta))
    (define color-scheme-record (make-parameter yellow))
    (define color-scheme-special (make-parameter red))

    (define *datatype-debug-writers* '())

    (define (register-datatype-debug-writer! type-test? datum->indent)
      (assume (procedure? type-test?))
      (assume (procedure? datum->indent))
      (set! *datatype-debug-writers*
        (cons (cons type-test? datum->indent)
              *datatype-debug-writers*)))

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
        ((? symbol?) (color (color-scheme-symbol) (symbol->string form)))
        ((? string?)
           (let1 str (open-output-string)
             (write form str)
             (color (color-scheme-string) (get-output-string str))))
        ((? number?)
           (color (color-scheme-number) (number->string form)))
        ((? (λ y (any (λ x ((car x) y)) *datatype-debug-writers*)))
          ((cdr (find (λ x ((car x) form)) *datatype-debug-writers*)) form))
        ((? record?)
           (or
             (and-let* ((rtd (record-rtd form))
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
                   (color (color-scheme-record) ">"))))
             (let1 str (open-output-string)
               (write form str)
               (color (color-scheme-special) (get-output-string str)))))
        (else
           (let1 str (open-output-string)
             (write form str)
             (color (color-scheme-special) (get-output-string str))))))))
