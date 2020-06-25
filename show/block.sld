(define-library (schemepunk show block)
  (export make-block block? block-head block-body block-tail
          block-length unindented-length
          block-with-prefix
          block->span-generator block->span-generator/indented)

  (import (scheme base)
          (scheme case-lambda)
          (scheme read)
          (scheme lazy)
          (scheme write)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk generator)
          (schemepunk show span))

  (begin
    (define-record-type Block
      (%make-block head body tail)
      block?
      (head block-head)
      (body block-body)
      (tail block-tail))

    (define make-block
      (case-lambda
        ((body) (make-block '() body '()))
        ((head body) (make-block head body '()))
        ((head body tail)
          (assume (list? head))
          (assume (and (pair? body) (proper-list? body)))
          (assume (list? tail))
          (%make-block head body tail))))

    (define (block-length block)
      (assume (block? block))
      (+ (unindented-length (block-head block))
         (unindented-length (block-body block))
         (unindented-length (block-tail block))))

    (define (whitespace? x)
      (and (span? x)
           (case (span-type x) ((whitespace newline) #t) (else #f))))

    (define (unindented-length blocks)
      (assume (list? blocks))
      (chain blocks
             (map (matchλ ((? promise? x) (unindented-length (list (force x))))
                          ((? block? x) (block-length x))
                          ((? span? x) (span-length x))
                          ((? string? x) (string-length x))))
             (fold + 0)))

    (define (block-with-prefix block prefix)
      (assume (block? block))
      (make-block
        (cons prefix (block-head block))
        (block-body block)
        (block-tail block)))

    (define (indent-span n)
      (chain (λ() (newline) (dotimes n (write-char #\space)))
             (with-output-to-string)
             (newline-span)))

    (define (trim-trailing-whitespace blocks)
      (if (and (pair? blocks) (whitespace? (last blocks)))
        (trim-trailing-whitespace (drop-right blocks 1))
        blocks))

    (define (block->span-generator block)
      (define blocks (list block))
      (define (next)
        (if (null? blocks)
          (eof-object)
          (let1-values (x rest) (car+cdr blocks)
            (match x
              ((? promise?)
                (set! blocks (cons (force x) rest))
                (next))
              ((? block?)
                (set! blocks
                  (append (block-head x) (block-body x) (block-tail x) rest))
                (next))
              ((? span?)
                (set! blocks rest)
                x)
              ((? string?)
                (set! blocks rest)
                (text-span x))
              (else
                (error "expected block, span, or string" (car blocks)))))))
      (assume (block? block))
      next)

    (define+ (block->span-generator/indented block :optional (width 80) (indent 2))
      (define blocks (list block))
      (define current-indent 0)
      (define extra-tail-length 0)
      (define (next)
        (if (null? blocks)
          (eof-object)
          (let1-values (x rest) (car+cdr blocks)
            (match x
              (('indent n)
                (set! current-indent n)
                (set! blocks rest)
                (next))
              (('extra-tail-length n)
                (set! extra-tail-length n)
                (set! blocks rest)
                (next))
              ((? promise?)
                (set! blocks (cons (force x) rest))
                (next))
              ((? block?)
                (let*
                  ((trimmed-head (trim-trailing-whitespace (block-head x)))
                   (head-length (unindented-length trimmed-head))
                   (elements (remove whitespace? (block-body x)))
                   (elem-lengths (map (λ=> (list) (unindented-length)) elements))
                   (tail-length (+ (unindented-length (block-tail x))
                                   extra-tail-length))
                   (break-anywhere?
                     (and (pair? elements)
                          (> (+ current-indent (block-length x)) width)))
                   (break-after-head?
                     (and break-anywhere?
                          (pair? (block-head x))
                          (> head-length (* indent 2))
                          (any (λ=> (+ current-indent head-length) (> <> width))
                               (cons (+ tail-length (last elem-lengths))
                                     elem-lengths)))))
                  (set! blocks
                    (cond
                      (break-after-head?
                        (let1 body-indent (+ current-indent indent)
                          `((extra-tail-length ,tail-length)
                            ,@trimmed-head
                            (indent ,body-indent)
                            ,@(append-map
                                (cute list (indent-span body-indent) <>)
                                elements)
                            ,@(block-tail x)
                            (extra-tail-length ,extra-tail-length)
                            (indent ,current-indent)
                            ,@rest)))
                      (break-anywhere?
                        (let1 body-indent (+ current-indent
                                             (unindented-length (block-head x)))
                          `((extra-tail-length ,tail-length)
                            ,@(block-head x)
                            (indent ,body-indent)
                            ,(car elements)
                            ,@(append-map
                                (cute list (indent-span body-indent) <>)
                                (cdr elements))
                            ,@(block-tail x)
                            (extra-tail-length ,extra-tail-length)
                            (indent ,current-indent)
                            ,@rest)))
                      (else
                        (append (generator->list (block->span-generator x)) rest)))))
                (next))
              ((? span?)
                (set! blocks rest)
                x)
              ((? string?)
                (set! blocks rest)
                (text-span x))
              (else
                (error "expected block, span, or string" x))))))
      (assume (block? block))
      (assume (positive? width))
      (assume (positive? indent))
      next)))
