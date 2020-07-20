(define-library (schemepunk show report)
  (export reported report-line wrapped/blocks)

  (import (scheme base)
          (schemepunk syntax)
          (schemepunk show))

  (begin
    (define report-line
      (fn (width)
        (each fl nl (as-yellow (make-string width #\─)) nl)))

    (define (reported title . content)
      (fn (width)
        (chain (trimmed/right (- width 4) title)
               (as-light-blue)
               (each "┤ " <> " ├")
               (padded width)
               (as-yellow)
               (with ((pad-char #\─) (ellipsis "…")))
               (each fl nl <> nl nl (each-in-list content))
               (terminal-aware))))

    (define (contains-newline? str)
      (call/cc (λ return
        (string-for-each
          (cut case <> ((#\newline) (return #t)))
          str)
        #f)))

    (define (wrapped/blocks . fmts)
      (let loop ((str "") (fmts fmts) (nl? #f))
        (cond
          ((null? fmts)
            (if (and nl? (not (zero? (string-length str))))
              (each nl nl (wrapped str))
              (wrapped str)))
          ((string? (car fmts))
            (loop (string-append str (car fmts)) (cdr fmts) nl?))
          ((procedure? (car fmts))
            (call-with-output (car fmts) (λ fmt-str
              (fn (string-width width)
                (cond
                  ((and (< (string-width fmt-str) (* width 2/3))
                        (not (contains-newline? fmt-str)))
                    (loop (string-append str fmt-str) (cdr fmts) nl?))
                  ((zero? (string-length str))
                    (each fmt-str (loop "" (cdr fmts) #t)))
                  (nl?
                    (each nl nl (wrapped str) nl nl fmt-str (loop "" (cdr fmts) #t)))
                  (else
                    (each (wrapped str) nl nl fmt-str (loop "" (cdr fmts) #t))))))))
          (else
            (loop str (pretty-color fmt) nl?)))))))
