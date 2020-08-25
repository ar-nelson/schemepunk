;; A minimal, fast JSON parser.
;; Supports event-based parsing, or can parse to a data structure where objects
;; are pairs with 'object as the car and an alist as the cdr.
(define-library (schemepunk json)
  (export escape-json-string
          string->json
          json->string
          read-json
          write-json
          make-json-context
          json-context?
          read-json-event)

  (import (scheme base)
          (scheme char)
          (scheme read)
          (scheme write)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk vector))

  (begin
    ;; stack is a list of booleans, where #t = array, #f = object
    ;; state is one of (comma colon key value)
    (define-record-type Json-Context
      (%make-json-context stack state pos)
      json-context?
      (stack json-context-stack set-json-context-stack!)
      (state json-context-state set-json-context-state!)
      (pos json-context-pos set-json-context-pos!))

    (define (make-json-context)
      (%make-json-context '() 'value -1))

    (define+ (read-json-event ctx :optional (port (current-input-port)))
      (%read-json-event ctx port))

    (define (next-char! ctx port)
      (set-json-context-pos! ctx
        (+ 1 (json-context-pos ctx)))
      (read-char port))

    (define (pop-stack! ctx)
      (set-json-context-stack! ctx (cdr (json-context-stack ctx)))
      (json-context-stack ctx))

    (define (%read-json-event ctx port)
      (define c (next-char! ctx port))
      (cond
        ((eof-object? c)
           (if (pair? (json-context-stack ctx))
               (values 'error "unexpected EOF")
               (values 'done #f)))
        ((char-whitespace? c) (%read-json-event ctx port))
        (else (case (json-context-state ctx)
          ((comma)
             (if (car (json-context-stack ctx))
               (case c
                 ((#\,) (set-json-context-state! ctx 'value)
                        (%read-json-event ctx port))
                 ((#\]) (when (null? (pop-stack! ctx))
                          (set-json-context-state! ctx 'value))
                        (values 'array-end #f))
                 (else
                   (values 'error
                           (format #f "unexpected ~a; expected , or ]" c))))
               (case c
                 ((#\,) (set-json-context-state! ctx 'key)
                        (%read-json-event ctx port))
                 ((#\}) (when (null? (pop-stack! ctx))
                          (set-json-context-state! ctx 'value))
                        (values 'object-end #f))
                 (else
                   (values 'error
                           (format #f "unexpected ~a; expected , or }" c))))))
          ((colon) (case c
             ((#\:) (set-json-context-state! ctx 'value)
                    (%read-json-event ctx port))
             (else
               (values 'error (format #f "unexpected ~a; expected :" c)))))
          ((key) (case c
             ((#\") (set-json-context-state! ctx 'colon)
                    (let-values (((event value) (read-json-string ctx port)))
                      (values (case event ((string) 'key) (else event)) value)))
             ((#\}) (set-json-context-state! ctx
                      (if (null? (pop-stack! ctx)) 'value 'comma))
                    (values 'object-end #f))
             (else
               (values 'error (format #f "unexpected ~a; expected key" c)))))
          (else
            (case c
              ((#\[) (set-json-context-stack! ctx
                       (cons #t (json-context-stack ctx)))
                     (values 'array-start #f))
              ((#\{) (set-json-context-stack! ctx
                       (cons #f (json-context-stack ctx)))
                     (set-json-context-state! ctx 'key)
                     (values 'object-start #f))
              ((#\])
                 (cond
                   ((null? (json-context-stack ctx))
                      (values 'error "unexpected ] at top level"))
                   ((car (json-context-stack ctx))
                      (unless (null? (pop-stack! ctx))
                        (set-json-context-state! ctx 'comma))
                      (values 'array-end #f))
                   (else
                      (values 'error "unexpected ]; expected }"))))
              ((#\")
                 (unless (null? (json-context-stack ctx))
                   (set-json-context-state! ctx 'comma))
                 (read-json-string ctx port))
              ((#\t)
                 (unless (null? (json-context-stack ctx))
                   (set-json-context-state! ctx 'comma))
                 (if (equal? (read-string 3 port) "rue")
                     (begin (set-json-context-pos! ctx
                              (+ 3 (json-context-pos ctx)))
                            (values 'boolean #t))
                     (values 'error "expected 't' to start 'true'")))
              ((#\f)
                 (unless (null? (json-context-stack ctx))
                   (set-json-context-state! ctx 'comma))
                 (if (equal? (read-string 4 port) "alse")
                     (begin (set-json-context-pos! ctx
                              (+ 4 (json-context-pos ctx)))
                            (values 'boolean #f))
                     (values 'error "expected 'f' to start 'false'")))
              ((#\n)
                 (unless (null? (json-context-stack ctx))
                   (set-json-context-state! ctx 'comma))
                 (if (equal? (read-string 3 port) "ull")
                     (begin (set-json-context-pos! ctx
                              (+ 3 (json-context-pos ctx)))
                            (values 'null #f))
                     (values 'error "expected 'n' to start 'null'")))
              ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\-)
                 (unless (null? (json-context-stack ctx))
                   (set-json-context-state! ctx 'comma))
                 (read-json-number ctx port c))
              ((#\} #\, #\:)
                 (values 'error (format #f "unexpected ~a; expected value" c)))
              (else (values 'error "illegal character"))))))))

    (define (read-json-string ctx port)
      (define str (open-output-string))
      (define eof-msg "unexpected EOF in string (unclosed string?)")
      (let loop ()
        (let1 c (next-char! ctx port)
          (case c
            ((#\") (values 'string (get-output-string str)))
            ((#\\)
               (let1 e (next-char! ctx port)
                 (case e
                   ((#\\ #\/ #\' #\") (write-char e str) (loop))
                   ((#\n) (write-string "\n" str) (loop))
                   ((#\r) (write-string "\r" str) (loop))
                   ((#\t) (write-string "\t" str) (loop))
                   ((#\b) (write-string "\b" str) (loop))
                   ((#\v) (write-char #\x0b str) (loop))
                   ((#\f) (write-char #\x0c str) (loop))
                   ((#\u)
                      (let1 u (read-hex-escape ctx port)
                       (cond
                         ((eof-object? u) (values 'error eof-msg))
                         ((not u) (values 'error "bad unicode escape"))
                         (else (write-char u str) (loop)))))
                   (else (values 'error (if (eof-object? e)
                                            eof-msg
                                            "bad escape"))))))
            (else (if (eof-object? c)
                      (values 'error eof-msg)
                      (begin (write-char c str) (loop))))))))

    (define (read-hex-escape ctx port)
      (define hex (read-string 4 port))
      (if (eof-object? hex)
          hex
          (let1 ord (string->number hex 16)
            (set-json-context-pos! ctx
              (+ 4 (json-context-pos ctx)))
            (and ord (integer->char ord)))))

    (define (read-json-number ctx port first-char)
      (define str (open-output-string))
      (define digit? (char-numeric? first-char))
      (define dot? (eqv? first-char #\.))
      (define e? #f)
      (write-char first-char str)
      (let loop ()
        (let1 c (peek-char port)
          (case c
            ((#\.) (if dot?
                       (values 'error (if e? "bad exponent" "bad number"))
                       (begin (set! dot? #t)
                              (write-char c str)
                              (next-char! ctx port)
                              (loop))))
            ((#\e #\E)
               (if e?
                   (values 'error "bad exponent")
                   (begin (set! e? #t)
                          (set! dot? #t)
                          (write-char #\e str)
                          (next-char! ctx port)
                          (let1 sign (next-char! ctx port)
                            (case sign
                              ((#\- #\+)
                                 (write-char sign str)
                                 (loop))
                              ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                                 (write-char #\+ str)
                                 (write-char sign str)
                                 (loop))
                              (else (values 'error "bad exponent")))))))
            ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
               (set! digit? #t)
               (write-char c str)
               (next-char! ctx port)
               (loop))
            (else
              (if digit?
                  (values 'number (get-output-string str))
                  (values 'error "dot or minus with no digits")))))))

    (define+ (read-json :optional (port (current-input-port)))
      (define ctx (make-json-context))
      (let loop ((current #f) (stack '()))
        (let1-values (event value) (%read-json-event ctx port)
          (case event
            ((array-start object-start)
              (loop '() (cons current stack)))
            ((key) (loop value (cons current stack)))
            ((done) (error "EOF while parsing JSON"))
            ((error)
              (error (format #f
                             "JSON parser (char ~s): ~a"
                             (json-context-pos ctx)
                             value)))
            (else
              (let ((result (case event
                              ((array-end) (reverse-list->vector current))
                              ((object-end) current)
                              ((string) value)
                              ((number) (string->number value))
                              ((boolean) (if value 'true 'false))
                              ((null) 'null)))
                    (new-current (case event
                                   ((array-end object-end) (car stack))
                                   (else current)))
                    (new-stack (case event
                                 ((array-end object-end) (cdr stack))
                                 (else stack))))
                (cond
                  ((not new-current) result)
                  ((string? new-current)
                    (loop `((,new-current . ,result) ,@(car new-stack))
                          (cdr new-stack)))
                  (else (loop (cons result new-current) new-stack)))))))))

    (define (string->json str)
      (read-json (open-input-string str)))

    (define (alist? xs)
      (and (list? xs) (every (λ x (and (pair? x) (string? (car x)))) xs)))

    (define (%write-json json port)
      (match json
        ('null (write-string "null" port))
        ('true (write-string "true" port))
        ('false (write-string "false" port))
        ((? number?)
          (if (integer? json)
            (write (exact json) port)
            (let1 str (number->string (inexact json))
              (write-string
                ; Remove the leaving 0 from 0.x decimal numbers.
                ; This makes the output format consistent across Schemes.
                ; (Gauche and Chibi include the 0, but Gambit drops it.)
                (cond
                  ((eqv? #\0 (string-ref str 0))
                    (substring str 1 (string-length str)))
                  ((and (eqv? #\- (string-ref str 0))
                        (eqv? #\0 (string-ref str 1))
                    (string-append "-"
                      (substring str 2 (string-length str)))))
                  (else str))
                port))))
        ((? string?)
          (write-char #\" port)
          (write-string (escape-json-string json) port)
          (write-char #\" port))
        ((? vector?)
          (write-char #\[ port)
          (let1 first #t
            (vector-for-each
              (λ x (if first (set! first #f) (write-char #\, port))
                   (%write-json x port))
              json))
          (write-char #\] port))
        ((? alist?)
          (write-char #\{ port)
          (unless (null? json)
            (let loop ((xs json))
              (let-values (((k v) (car+cdr (car xs))))
                (write-char #\" port)
                (write-string (escape-json-string k) port)
                (write-string "\":" port)
                (%write-json v port)
                (unless (null? (cdr xs))
                  (write-char #\, port)
                  (loop (cdr xs))))))
          (write-char #\} port))
        ((? list?)
          ; Handle non-object lists, just in case we get non-JSON data.
          (%write-json (list->vector json) port))
        (else
          (write-string "\"!! NOT JSON: " port)
          (chain (format #f "~s" json) (escape-json-string _) (write-string _ port))
          (write-char #\" port))))

    (define+ (write-json json :optional (port (current-output-port)))
      (%write-json json port))

    (define (json->string json)
      (with-output-to-string (λ() (write-json json))))

    (define (escape-json-string str)
      (with-output-to-string (λ()
        (string-for-each
          (λ c (case c
            ((#\" #\\) (write-string (string #\\ c)))
            ((#\newline) (write-string "\\n"))
            ((#\return) (write-string "\\r"))
            ((#\tab) (write-string "\\t"))
            ((#\backspace) (write-string "\\b"))
            ((#\x0b) (write-string "\\v"))
            ((#\x0c) (write-string "\\f"))
            (else (write-char c))))
          str))))))
