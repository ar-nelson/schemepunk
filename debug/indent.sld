(define-library (schemepunk debug indent)
  (export indent-size
          output-width
          get-terminal-width
          make-colored-text
          colored-text?
          make-indent-group
          indent-group?
          color
          unindented-length
          write-indented
          write-unindented)

  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme case-lambda)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk term-colors))

  (cond-expand
    ((and chicken unix)
      (import (ioctl))
      (begin (define (get-terminal-width)
               (cadr (ioctl-winsize)))))
    (gauche
      (import (only (text console) call-with-console
                                   make-default-console
                                   query-screen-size))
      (begin (define (get-terminal-width)
               (guard (e (#t 80))
                 (let-values
                   (((_ w) (call-with-console (make-default-console)
                                              query-screen-size)))
                   w)))))
    (chibi
      (import (rename (chibi stty) (get-terminal-width %get-terminal-width%)))
      (begin (define (get-terminal-width)
               (%get-terminal-width% (current-output-port)))))
    (gerbil
      (import (std misc process))
      (begin (define (get-terminal-width)
               (or (guard (e (#t #f))
                     (read (open-input-string (run-process (list "tput" "cols")))))
                   80))))
    (else (begin (define (get-terminal-width) 80))))

  (begin
    (define indent-size (make-parameter 2))
    (define output-width (make-parameter 80))

    (define-record-type Colored-Text
      (make-colored-text spans)
      colored-text?
      (spans colored-text-spans))

    (define-record-type Indent-Group
      (make-indent-group head contents tail)
      indent-group?
      (head indent-group-head)
      (contents indent-group-contents)
      (tail indent-group-tail))

    (define (color c t)
      (make-colored-text (list (cons c t))))

    (define (unindented-length x)
      (cond
        ((indent-group? x)
           (let ((head (indent-group-head x))
                 (contents (indent-group-contents x))
                 (tail (indent-group-tail x)))
             (+ (if head (unindented-length head) 0)
                (fold +
                  (if (null? contents) 0 (- (length contents) 1))
                  (map unindented-length contents))
                (if tail (unindented-length tail) 0))))
        ((colored-text? x)
           (chain (colored-text-spans x)
                  (map cdr)
                  (map string-length)
                  (fold + 0)))
        ((string? x) (string-length x))
        ((not x) 0)
        (else (error "wrong type for unindented-length" x))))

    (define (write-newline-indent n)
      (newline)
      (dotimes n (write-char #\space)))

    (define (write-colored-text x)
      (for-each
        (λ span
          (if (car span)
              (write-colored (car span) (cdr span))
              (write-string (cdr span))))
        (colored-text-spans x)))

    (define (write-unindented x)
      (cond
        ((indent-group? x)
           (let ((head (indent-group-head x))
                 (contents (indent-group-contents x))
                 (tail (indent-group-tail x)))
             (when head (write-unindented head))
             (when (pair? contents)
               (write-unindented (car contents))
               (for-each (λ y (write-char #\space)
                              (write-unindented y))
                         (cdr contents)))
             (when tail (write-unindented tail))))
        ((colored-text? x) (write-colored-text x))
        ((string? x) (write-string x))
        (else (error "wrong type for write-unindented" x))))

    (define (%write-indented% x indent extra-tail-length)
      (cond
        ((indent-group? x)
           (let* ((size (indent-size))
                  (max-length (output-width))
                  (head (indent-group-head x))
                  (contents (indent-group-contents x))
                  (tail (indent-group-tail x))
                  (head-length (unindented-length head))
                  (contents-lengths (map unindented-length contents))
                  (tail-length (+ (unindented-length tail) extra-tail-length))
                  (contents? (pair? contents))
                  (break-anywhere?
                    (> (+ indent
                          head-length
                          (fold +
                            (if (null? contents) 0 (- (length contents) 1))
                            contents-lengths)
                          tail-length)
                       max-length))
                  (break-after-head?
                    (and head
                         break-anywhere?
                         contents?
                         (> head-length (* size 2))
                         (any (λ l (> (+ indent head-length l) max-length))
                              (cons (+ tail-length (last contents-lengths))
                                    contents-lengths)))))
             (when head
               (%write-indented% head indent (if contents? tail-length 0)))
             (when contents?
               (cond
                 (break-after-head?
                   (let1 body-indent (+ indent size)
                     (for-each (λ y (write-newline-indent body-indent)
                                    (%write-indented% y body-indent 0))
                               (drop-right contents 1))
                     (write-newline-indent body-indent)
                     (%write-indented% (last contents) body-indent tail-length)))
                 (break-anywhere?
                   (let1 body-indent (+ indent head-length)
                     (for-each (λ y (%write-indented% y body-indent 0)
                                    (write-newline-indent body-indent))
                               (drop-right contents 1))
                     (%write-indented% (last contents) body-indent tail-length)))
                 ((pair? contents)
                     (write-unindented (car contents))
                     (for-each (λ y (write-char #\space)
                                    (write-unindented y))
                               (cdr contents)))))
             (when tail (write-unindented tail))))
        ((colored-text? x) (write-colored-text x))
        ((string? x) (write-string x))
        (else (error "wrong type for write-indented" x))))

    (define+ (write-indented x :optional (indent 0))
      (%write-indented% x indent 0))))
