(define-library (schemepunk show block datum)
  (export datum->block datum->block/simple datum->block/shared
          with-default-datum-colors register-datum-writer!

          datum-color-list datum-color-vector datum-color-structure
          datum-color-symbol datum-color-string datum-color-number
          datum-color-record datum-color-special datum-color-special-form)

  (import (scheme base)
          (scheme char)
          (scheme lazy)
          (scheme write)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk box)
          (schemepunk generator)
          (schemepunk term-colors)
          (schemepunk show span)
          (schemepunk show block)
          (schemepunk show numeric))

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

    (define-record-type Datum-Shared-Context
      (make-datum-shared-context mode seen next-label labels)
      datum-shared-context?
      (mode ctx-mode)
      (seen %ctx-seen)
      (next-label ctx-next-label)
      (labels %ctx-labels))

    (define (ctx-seen ctx)
      (case (ctx-mode ctx)
        ((shared) (unbox (%ctx-seen ctx)))
        (else (%ctx-seen ctx))))

    (define (ctx-labels ctx)
      (unbox (%ctx-labels ctx)))

    (define (ctx-cons-seen ctx datum)
      (make-datum-shared-context
        (ctx-mode ctx)
        (cons datum (%ctx-seen ctx))
        (ctx-next-label ctx)
        (%ctx-labels ctx)))

    (define (ctx-add-seen! ctx datum)
      (update-box! (%ctx-seen ctx) (cut cons datum <>))
      ctx)

    (define (ctx-add-label! ctx datum)
      (define label (unbox (ctx-next-label ctx)))
      (update-box!
        (%ctx-labels ctx)
        (λ xs (cons (cons datum label) xs)))
      (set-box! (ctx-next-label ctx) (+ label 1))
      label)

    (define datum-shared-context (make-parameter #f))

    (define (pair->block-body pair)
      (let* ((head (car pair))
             (tail (cdr pair))
             (ctx (datum-shared-context))
             (seen (ctx-seen ctx))
             (mode (ctx-mode ctx))
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
                      (or (memq tail seen) (assq tail (ctx-labels ctx)))))
              (list (whitespace-span)
                    (text-span "." (datum-color-list))
                    (whitespace-span)
                    (datum->block tail)))
            (else
              (parameterize ((datum-shared-context
                               (case mode
                                 ((shared) (ctx-add-seen! ctx tail))
                                 (else (ctx-cons-seen ctx tail)))))
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
                   (gmap (λ=> (number->string _ 16)
                              (string-upcase _)
                              (format #f "#x~a" _)
                              (text-span _ (datum-color-number)))
                         _)
                   (generator->list _)
                   (intercalate (whitespace-span) _))
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
                (make-block
                  (list
                    (text-span "#,(record" (datum-color-record))
                    (whitespace-span))
                  (list
                    (text-span (symbol->string (rtd-name rtd))
                               (datum-color-record)))
                  (list
                    (text-span ")" (datum-color-record))))
                (make-block
                  (list
                    (make-block
                      (list
                        (text-span "#,(record" (datum-color-record))
                        (whitespace-span))
                      (list
                        (text-span (symbol->string (rtd-name rtd))
                                   (datum-color-record))))
                    (whitespace-span))
                  (intercalate (whitespace-span)
                    (map
                      (λ field
                        (make-block
                          (list
                            (text-span (format #f "(~s" field)
                                       (datum-color-record))
                            (whitespace-span))
                          (list
                            (datum->block ((rtd-accessor rtd field) datum)))
                          (list
                            (text-span ")" (datum-color-record)))))
                      fields))
                  (list
                    (text-span ")" (datum-color-record))))))
            (make-block
              (list (text-span (datum->string datum) (datum-color-record))))))
        (else
          (make-block
            (list (text-span (datum->string datum) (datum-color-special)))))))

    (define (datum->block datum)
      (define ctx (datum-shared-context))
      (if ctx
        (let ((mode (ctx-mode ctx))
              (seen (ctx-seen ctx))
              (labels (ctx-labels ctx)))
          (if (and (not (eqv? mode 'simple)) (memq datum seen))
            (let1 label (match (assq datum labels)
                          ((_ . label) label)
                          (else (ctx-add-label! ctx datum)))
              (text-span (format #f "#~a#" label) (datum-color-structure)))
            (case mode
              ((simple)
                (delay
                  (parameterize ((datum-shared-context (ctx-cons-seen ctx datum)))
                    (%datum->block datum))))
              ((cycles)
                (parameterize ((datum-shared-context (ctx-cons-seen ctx datum)))
                  (let ((label (chain-and (assq datum labels) (cdr _)))
                        (block (%datum->block datum)))
                    (if label
                      (block-with-prefix
                        block
                        (text-span (format #f "#~a=" label) (datum-color-structure)))
                      block))))
              ((shared)
                (when (or (pair? datum) (vector? datum) (record? datum))
                  (ctx-add-seen! ctx datum))
                (let ((label (chain-and (assq datum labels) (cdr _)))
                      (block (%datum->block datum)))
                  (if label
                    (block-with-prefix
                      block
                      (text-span (format #f "#~a=" label) (datum-color-structure)))
                    block))))))
        (parameterize ((datum-shared-context (make-datum-shared-context
                                               'cycles
                                               '()
                                               (box 0)
                                               (box '()))))
          (let* ((without-shared (datum->block datum))
                 (labels (ctx-labels (datum-shared-context))))
            (if (pair? labels)
              (parameterize ((datum-shared-context (make-datum-shared-context
                                                     'cycles
                                                     '()
                                                     (box -1)
                                                     (box labels))))
                (datum->block datum))
              without-shared)))))

    (define (datum->block/simple datum)
      (parameterize ((datum-shared-context (make-datum-shared-context
                                             'simple
                                             '()
                                             (box 0)
                                             (box '()))))
        (force (datum->block datum))))

    (define (datum->block/shared datum)
      (parameterize ((datum-shared-context (make-datum-shared-context
                                             'shared
                                             (box '())
                                             (box 0)
                                             (box '()))))
        (let* ((without-shared (datum->block datum))
               (labels (ctx-labels (datum-shared-context))))
          (if (pair? labels)
            (parameterize ((datum-shared-context (make-datum-shared-context
                                                   'shared
                                                   (box '())
                                                   (box -1)
                                                   (box labels))))
              (datum->block datum))
            without-shared))))))
