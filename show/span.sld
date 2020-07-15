(define-library (schemepunk show span)
  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme write)
          (schemepunk syntax)
          (schemepunk term-colors))

  (export span? span-type span-text span-color span-length
          text-span whitespace-span newline-span
          span-map-text span-with-color
          char-generator->span-generator
          write-span)

  (begin
    (define-record-type Span
      (make-span type text color)
      span?
      (type span-type)
      (text span-text)
      (color span-color))

    (define (span-length span)
      (string-length (span-text span)))

    (define text-span
      (case-lambda
        ((text) (make-span 'text text #f))
        ((text color) (make-span 'text text color))))

    (define whitespace-span
      (case-lambda
        (() (make-span 'whitespace " " #f))
        ((text) (make-span 'whitespace text #f))))

    (define newline-span
      (case-lambda
        (() (make-span 'newline (with-output-to-string newline) #f))
        ((text) (make-span 'newline text #f))))

    (define (span-map-text proc span)
      (make-span (span-type span) (proc (span-text span)) (span-color span)))

    (define (span-with-color span color)
      (make-span (span-type span) (span-text span) color))

    (define+ (char-generator->span-generator next-char
                                             :optional
                                             (word-separator? char-whitespace?)
                                             (read-ansi-escapes? #f))
      ; This could be written more efficiently with make-coroutine-generator.
      ; But, because make-coroutine-generator spawns a thread in Kawa,
      ; and this procedure may be called hundreds of times in a loop,
      ; it is instead written in straightforward single-threaded style.
      (define mode 'text)
      (define color #f)
      (define span (open-output-string))
      (λ ()
        (let loop ((c (next-char)))
          (cond
            ((eof-object? c)
              (let1 text (get-output-string span)
                (set! span (open-output-string))
                (if (zero? (string-length text))
                  c
                  (make-span mode text color))))
            ((and read-ansi-escapes? (eqv? c #\escape))
              (match (next-char)
                (#\[
                  (let ((old-color color)
                        (text (get-output-string span)))
                    (cond
                      ((zero? (string-length text))
                        (set! color (merge-colors color (read-color next-char)))
                        (loop (next-char)))
                      (else
                        (set! span (open-output-string))
                        (set! color (read-color next-char))
                        (make-span mode text old-color)))))
                (c2
                  (write-char c span)
                  (loop c2))))
            (else
              (let1 new-mode (match c ((or #\return #\newline) 'newline)
                                      ((? word-separator?) 'whitespace)
                                      (else 'text))
                (if (eq? mode new-mode)
                  (begin (write-char c span)
                         (loop (next-char)))
                  (let ((old-mode mode)
                        (text (get-output-string span)))
                    (set! mode new-mode)
                    (set! span (open-output-string))
                    (write-char c span)
                    (if (zero? (string-length text))
                      (loop (next-char))
                      (make-span old-mode text color))))))))))

    (define+ (write-span span :optional (port (current-output-port)))
      (assume (span? span))
      (if (span-color span)
        (write-in-color (span-color span) (span-text span) port)
        (write-string (span-text span) port)))))
