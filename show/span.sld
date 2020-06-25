(define-library (schemepunk show span)
  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme write)
          (schemepunk syntax)
          (schemepunk term-colors))

  (export span? span-type span-text span-color span-length
          text-span whitespace-span newline-span
          span-with-color
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

    (define (span-with-color span color)
      (make-span (span-type span) (span-text span) color))

    (define (char-generator->span-generator next-char)
      ; This could be written more efficiently with make-coroutine-generator.
      ; But, because make-coroutine-generator spawns a thread in Kawa,
      ; and this procedure may be called hundreds of times in a loop,
      ; it is instead written in straightforward single-threaded style.
      (define spans
        (let loop ((mode 'text) (span (open-output-string)) (spans '()))
          (let* ((c (next-char))
                 (next-mode (match c
                              ((? eof-object?) #f)
                              ((or #\return #\newline) 'newline)
                              ((? char-whitespace?) 'whitespace)
                              (else 'text))))
            (if (eq? mode next-mode)
              (begin (write-char c span)
                     (loop mode span spans))
              (let* ((text (get-output-string span))
                     (next-spans (if (zero? (string-length text))
                                   spans
                                   (cons (make-span mode text #f) spans))))
                (if next-mode
                  (let1 next-span (open-output-string)
                    (write-char c next-span)
                    (loop next-mode next-span next-spans))
                  (reverse next-spans)))))))
      (lambda ()
        (if (null? spans)
          (eof-object)
          (let1 span (car spans)
            (set! spans (cdr spans))
            span))))

    (define (write-span span)
      (assume (span? span))
      (if (span-color span)
        (write-in-color (span-color span) (span-text span))
        (write-string (span-text span))))))
