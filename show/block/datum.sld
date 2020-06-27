(define-library (schemepunk show block datum)
  (export datum->block datum->block/simple datum->block/shared
          with-default-datum-colors register-datum-writer!

          datum-color-list datum-color-vector datum-color-structure
          datum-color-symbol datum-color-string datum-color-number
          datum-color-record datum-color-special datum-color-special-form)

  (import (scheme base)
          (scheme char)
          (scheme cxr)
          (scheme lazy)
          (scheme write)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk generator)
          (schemepunk term-colors)
          (schemepunk show span)
          (schemepunk show block))

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
              (define (rtd-accessor . _) #f))))

  (begin
    (define datum-color-list (make-parameter #f))
    (define datum-color-vector (make-parameter #f))
    (define datum-color-structure (make-parameter #f))
    (define datum-color-symbol (make-parameter #f))
    (define datum-color-string (make-parameter #f))
    (define datum-color-number (make-parameter #f))
    (define datum-color-record (make-parameter #f))
    (define datum-color-special (make-parameter #f))
    (define datum-color-special-form (make-parameter #f))

    (define *datum-writers* '())

    (define (register-datum-writer! type-test? writer)
      (assume (procedure? type-test?))
      (assume (procedure? writer))
      (set! *datum-writers*
        (cons (cons type-test? writer) *datum-writers*)))

    (define (break-after-second-arg? sym)
      (case sym
        ((define-library define-syntax let-syntax letrec-syntax syntax-rules
          case define define-record-type define-values do guard if lambda
          let let* let*-values let-values letrec letrec* parameterize set!
          unless when) #t)
        (else #f)))

    (define (special-form? sym)
      (case sym
        ((define-library export include include-ci include-library-declarations
          library cond-expand define-syntax let-syntax letrec-syntax syntax-rules
          and begin case case-lambda cond define define-record-type define-values
          delay delay-force do else guard if import lambda let let* let*-values
          let-values letrec letrec* or parameterize quasiquote quote set! unless
          unquote unquote-splicing when) #t)
        (else #f)))

    (define (prefix-form sym)
      (case sym
        ((quote) "'")
        ((quasiquote) "`")
        ((unquote) ",")
        ((unquote-splicing) ",@")
        (else #f)))

    (define (with-default-datum-colors thunk)
      (parameterize ((datum-color-list cyan)
                     (datum-color-vector light-cyan)
                     (datum-color-structure yellow)
                     (datum-color-symbol white)
                     (datum-color-string green)
                     (datum-color-number magenta)
                     (datum-color-record yellow)
                     (datum-color-special red)
                     (datum-color-special-form light-blue))
        (thunk)))

    (define (datum->string datum)
      (with-output-to-string (λ() (write datum))))

    (define datum-shared-context (make-parameter #f))

    (define (pair->block-body pair)
      (match-let* (((head . tail) pair)
                   (ctx (datum-shared-context))
                   ((seen _ labels mode) ctx)
                   (head-block (datum->block head)))
        (cons head-block
          (cond
            ((null? tail) '())
            ((and (eqv? mode 'simple) (pair? tail) (memq tail seen))
              (list (whitespace-span)
                    (delay
                      (parameterize ((datum-shared-context ctx))
                        (make-block (pair->block-body tail))))))
            ((or (not (pair? tail))
                 (and (not (eqv? mode 'simple))
                      (or (memq tail seen) (assq tail labels))))
              (list (whitespace-span)
                    (text-span "." (datum-color-list))
                    (whitespace-span)
                    (datum->block tail)))
            (else
              (parameterize ((datum-shared-context
                               (case mode
                                 ((shared) ctx)
                                 (else (cons (cons tail seen) (cdr ctx))))))
                (case mode ((shared) (set-car! ctx (cons tail (car ctx)))))
                (cons (whitespace-span) (pair->block-body tail))))))))

    (define (%datum->block datum)
      (match datum
        (((? symbol? head) x . xs)
          (let1 body (pair->block-body datum)
            (cond
              ((and (null? xs) (prefix-form head) (= 3 (length body)))
                (block-with-prefix (let1 prefixed (caddr body)
                                     (if (promise? prefixed) (force prefixed) prefixed))
                                   (text-span (prefix-form head) (datum-color-list))))
              ((and (pair? xs) (break-after-second-arg? head) (>= (length body) 5))
                (make-block
                  (list
                    (make-block
                      (list (text-span "(" (datum-color-list))
                            (text-span (datum->string head) (datum-color-special-form))
                            (cadr body))
                      (list (caddr body)))
                    (cadddr body))
                  (cddddr body)
                  (list (text-span ")" (datum-color-list)))))
              (else
                (make-block
                  (list
                    (text-span "(" (datum-color-list))
                    (text-span (datum->string head) (if (special-form? head)
                                                      (datum-color-special-form)
                                                      (datum-color-symbol)))
                    (cadr body))
                  (cddr body)
                  (list (text-span ")" (datum-color-list))))))))
        (()
          (make-block
            (list (text-span "()" (datum-color-list)))))
        ((? pair?)
          (make-block
            (list (text-span "(" (datum-color-list)))
            (pair->block-body datum)
            (list (text-span ")" (datum-color-list)))))
        (#()
          (make-block
            (list (text-span "#()" (datum-color-vector)))))
        ((? vector?)
          (make-block
            (list (text-span "#(" (datum-color-vector)))
            (pair->block-body (vector->list datum))
            (list (text-span ")" (datum-color-vector)))))
        ((? bytevector?)
          (make-block
            (list (text-span "#u8(" (datum-color-vector)))
            (chain (bytevector->generator datum)
                   (gmap (λ=> (number->string <> 16)
                              (string-upcase)
                              (format #f "#x~a")
                              (text-span <> (datum-color-number))))
                   (generator->list)
                   (intercalate (whitespace-span)))
            (list (text-span ")" (datum-color-vector)))))
        ((? symbol?)
          (make-block
            (list (text-span (datum->string datum) (datum-color-symbol)))))
        ((? string?)
          (make-block
            (list (text-span (datum->string datum) (datum-color-string)))))
        ((? number?)
          (make-block
            (list (text-span (number->string datum) (datum-color-number)))))
        ((? (λ y (any (λ x ((car x) y)) *datum-writers*)))
          ((cdr (find (λ x ((car x) datum)) *datum-writers*)) datum))
        ((? record?)
          (or
            (and-let* ((rtd (record-rtd datum))
                       (fields (vector->list (rtd-all-field-names rtd))))
              (if (null? fields)
                (text-span (format #f "#<~s>" (rtd-name rtd))
                           (datum-color-record))
                (make-block
                  (list
                    (text-span (format #f "#<~s" (rtd-name rtd))
                               (datum-color-record))
                    (whitespace-span))
                  (chain
                    fields
                    (map
                      (λ field
                        (make-block
                          (list
                            (text-span (format #f "~s:" field)
                                       (datum-color-record)))
                          (list
                            (datum->block ((rtd-accessor rtd field) datum))))))
                    (intercalate (whitespace-span)))
                  (list
                    (text-span ">" (datum-color-record))))))
            (make-block
              (list (text-span (datum->string datum) (datum-color-record))))))
        (else
          (make-block
            (list (text-span (datum->string datum) (datum-color-special)))))))

    (define (datum->block datum)
      (define ctx (datum-shared-context))
      (match ctx
        ((seen next-label labels mode)
          (if (and (not (eqv? mode 'simple)) (memq datum seen))
            (let1 label (match (assq datum labels)
                          ((_ . label) label)
                          (else
                            (set-car! (cdr ctx) (+ next-label 1))
                            (set-car! (cddr ctx) `((,datum . ,next-label) ,@labels))
                            next-label))
              (text-span (format #f "#~a#" label) (datum-color-structure)))
            (case mode
              ((simple)
                (delay
                  (parameterize ((datum-shared-context `((,datum ,@seen) ,@(cdr ctx))))
                    (%datum->block datum))))
              ((cycles)
                (parameterize ((datum-shared-context `((,datum ,@seen) ,@(cdr ctx))))
                  (let ((label (chain-and (assq datum labels) (cdr)))
                        (block (%datum->block datum)))
                    (if label
                      (block-with-prefix
                        block
                        (text-span (format #f "#~a=" label) (datum-color-structure)))
                      block))))
              ((shared)
                (when (or (pair? datum) (vector? datum) (record? datum))
                  (set-car! ctx (cons datum (car ctx))))
                (let ((label (chain-and (assq datum labels) (cdr)))
                      (block (%datum->block datum)))
                  (if label
                    (block-with-prefix
                      block
                      (text-span (format #f "#~a=" label) (datum-color-structure)))
                    block))))))
        (else
          (parameterize ((datum-shared-context (list '() 0 '() 'cycles)))
            (match-let* ((without-shared (datum->block datum))
                         ((_ next-label labels _) (datum-shared-context)))
              (if (positive? next-label)
                (parameterize ((datum-shared-context `(() -1 ,labels cycles)))
                  (datum->block datum))
                without-shared))))))

    (define (datum->block/simple datum)
      (parameterize ((datum-shared-context (list '() 0 '() 'simple)))
        (force (datum->block datum))))

    (define (datum->block/shared datum)
      (parameterize ((datum-shared-context (list '() 0 '() 'shared)))
        (match-let* ((without-shared (datum->block datum))
                     ((_ next-label labels _) (datum-shared-context)))
          (if (positive? next-label)
            (parameterize ((datum-shared-context `(() -1 ,labels shared)))
              (datum->block datum))
            without-shared))))))
