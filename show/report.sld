(define-library (schemepunk show report)
  (export reported report-line wrapped/blocks code-snapshot)

  (import (except (scheme base) string-length substring string make-string)
          (scheme cxr)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk string)
          (schemepunk stream)
          (schemepunk sort)
          (schemepunk show))

  (begin
    (define report-line
      (fn (width)
        (each fl nl (as-yellow (make-string width #\─)) nl)))

    (define (reported title . content)
      (fn (width)
        (chain (trimmed/right (- width 4) title)
               (as-light-blue _)
               (each "┤ " _ " ├")
               (padded width _)
               (as-yellow _)
               (with ((pad-char #\─) (ellipsis "…")) _)
               (each fl nl _ nl nl (each-in-list content))
               (terminal-aware _))))

    (define (contains-newline? str)
      (call/cc (λ return
        (string-for-each
          (λ x (case x ((#\newline) (return #t))))
          str)
        #f)))

    (define (wrapped/blocks . fmts)
      (let loop ((str "") (fmts fmts) (nl? #f))
        (cond
          ((null? fmts)
            (if (and nl? (isnt str string-null?))
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
                  ((string-null? str)
                    (each fmt-str (loop "" (cdr fmts) #t)))
                  (nl?
                    (each nl nl (wrapped str) nl nl fmt-str (loop "" (cdr fmts) #t)))
                  (else
                    (each (wrapped str) nl nl fmt-str (loop "" (cdr fmts) #t))))))))
          (else
            (loop str (cons (pretty-color (car fmts)) (cdr fmts)) nl?)))))

    ; annotations: (line start-col end-col color message)
    (define (code-snapshot filename source-fmt line-numbers? . annotations)
      (assume (pair? annotations))
      (fn (width string-width word-separator?)
        (call-with-output (each source-fmt) (λ source-text
          (let1 lines (stream-cons '() (line-stream (open-input-string source-text)))
            (with ((ellipsis "…"))
              (each
                (if filename
                  (each (as-bold (as-black (trimmed/right width filename)) nl))
                  nothing)
                (chain annotations
                  (map (cut annotation-range->width-range <> string-width lines) _)
                  (list-sort annotation<? _)
                  (chunk-annotations width line-numbers? _)
                  (joined
                    (cut code-snapshot-chunk width string-width lines line-numbers? <>)
                    _
                    (each fl (as-gray "…"))))
                fl)))))))

    (define (code-snapshot-chunk width string-width source-lines-stream line-numbers? annotations)
      (define lines (map car annotations))
      (define min-line (chain lines (fold min +inf.0 _) (exact _) (- _ 2) (max _ 1)))
      (define max-line (chain lines (fold max 0 _) (+ _ 3)))
      (define line-number-width
        (if line-numbers?
          (+ 1 (string-width (number->string max-line)))
          2))
      (define code-width (- width line-number-width))
      (define code-lines
        (chain source-lines-stream
               (stream-drop min-line _)
               (stream-take (- max-line min-line) _)
               (stream->list _)))
      (define widths (map string-width code-lines))
      (define offset
        (compute-offset annotations code-width (fold max 0 widths)))
      (define relative-annotations
        (map (λ((line start end . rest))
               `(,(- line min-line)
                 ,(min (- code-width 1) (max 0 (- start offset)))
                 ,(min code-width (max 1 (- end offset)))
                 ,@rest))
             annotations))
      (let loop ((line 0)
                 (groups (group-by-line relative-annotations))
                 (out '()))
        (if (< line (length code-lines))
          (let ((code (list-ref code-lines line))
                (group (and (pair? groups) (= line (caar groups)) (car groups))))
            (loop
              (+ line 1)
              (if group (cdr groups) groups)
              `(,@out
                ,fl
                ,(padded (- line-number-width 1)
                   (if line-numbers?
                     (if group
                       (as-bold ((cadddr (cadr group)) (+ min-line line)))
                       (as-gray (+ min-line line)))
                     nothing))
                " "
                ,(trimmed/right code-width
                   (cond
                     ((zero? offset) code)
                     ((<= (list-ref widths line) offset) nothing)
                     (else (trimmed (- (list-ref widths line) offset) code))))
                ,@(if group
                    `(,fl
                      ,(columnar
                         line-number-width nothing
                         code-width
                         (format-annotations offset code (cdr group))))
                    '()))))
          (each-in-list out))))

    (define (line-stream port)
      (stream-cons
        (with-output-to-string (λ()
          (do ((c (read-char port) (read-char port)))
              ((or (eof-object? c) (eqv? c #\newline)))
            (write-char c))))
        (line-stream port)))

    (define (annotation<? x y)
      (or (< (car x) (car y))
          (and (= (car x) (car y))
               (< (caddr x) (caddr y)))))

    (define (annotation-overlaps? x y)
      (or (<= (cadr x) (cadr y) (caddr x))
          (<= (cadr x) (caddr y) (caddr x))))

    (define (annotation-range->width-range annotation string-width lines)
      (match-let* (((line start end . rest) annotation)
                   (code (stream-ref lines line))
                   (start/0 (max 0 (- start 1)))
                   (end/0 (max 0 (- end 1)))
                   (relative-start (string-width code 0 start/0))
                   (relative-end
                     (+ relative-start (string-width code start/0 end/0))))
        `(,line ,relative-start ,relative-end ,@rest)))

    (define (compute-offset annotations width max-line-width)
      (define leftmost
        (chain annotations (map caddr _) (fold min +inf.0 _) (exact _)))
      (define rightmost
        (chain annotations (map cadr _) (fold max 0 _)))
      (if (and (<= max-line-width width) (< rightmost width))
        0
        (chain (+ leftmost rightmost)
               (quotient _ 2)
               (- _ (quotient width 2))
               (min _ (- (max rightmost max-line-width) width))
               (max _ 0))))

    (define (chunk-annotations width line-numbers? annotations)
      (let loop ((anns annotations) (last-line #f) (chunk '()) (skipped '()))
        (cond
          ((null? anns)
            (cond
              ((pair? skipped)
                (cons chunk (chunk-annotations width line-numbers? skipped)))
              ((null? chunk)
                '())
              (else
                (list chunk))))
          ((and last-line (> (caar anns) (+ last-line 2)))
            (cons chunk (chunk-annotations width line-numbers? (append skipped anns))))
          ((and (eqv? last-line (caar anns))
                (or (> (cadar anns)
                       (+ (caddar chunk)
                          (- width
                             3
                             (if line-numbers?
                               (string-length (number->string last-line))
                               1))))
                    (any (is (car anns) annotation-overlaps? _)
                         (filter (λ=> (car _) (= _ last-line)) chunk))))
            (loop (cdr anns) last-line chunk (snoc skipped (car anns))))
          (else
            (loop (cdr anns) (caar anns) (snoc chunk (car anns)) skipped)))))

    (define (group-by-line annotations)
      (let loop ((remaining annotations) (groups '()))
        (if (null? remaining)
          groups
          (let*-values
            (((line) (caar remaining))
             ((group not-group) (partition (λ=> (car _) (= _ line)) remaining)))
            (loop not-group (snoc groups (cons line group)))))))

    ; splits in reverse, but that doesn't matter; only used for min-text-width
    (define (split-string word-separator? str)
      (define in (open-input-string str))
      (let loop ((out (open-output-string)) (words '()))
        (match (read-char in)
          ((? eof-object?) words)
          ((? word-separator?)
            (let sep-loop ()
              (match (read-char in)
                ((? eof-object?) words)
                ((? word-separator?) (sep-loop))
                (c (let1 next-out (open-output-string)
                     (write-char c next-out)
                     (loop next-out (cons (get-output-string out) words)))))))
          (c (write-char c out) (loop out words)))))

    (define *min-wrap-width* 15)

    (define (format-annotations offset line annotations)
      (fn (width string-width word-separator?)
        (let loop ((anns annotations) (msgs '()))
          (match anns
            (((_ _ _ _ #f) . rest)
              (loop rest (snoc msgs "")))
            (((_ _ _ _ fmt) . rest)
              (call-with-output (each fmt) (λ msg (loop rest (snoc msgs msg)))))
            (()
              (let ((underlines
                      (each-in-list
                        (map
                          (λ((_ start end color _))
                            (each (space-to start)
                                  (color (make-string (- end start) #\^))))
                          annotations)))
                    (labels
                      (chain
                        (fold
                          (label-row-fold-step width)
                          '((0 ()))
                          annotations
                          (map (λ=> (split-string word-separator? _)
                                    (map string-width _)
                                    (fold max *min-wrap-width* _))
                               msgs)
                          (map string-width msgs)
                          (snoc (map cadr (cdr annotations)) width))
                        (map cadr _)
                        (filter pair? _)
                        (reverse _)
                        ((λ xs
                           (if (null? xs)
                             nothing
                             (each
                               (each-in-list (map (cut apply columnar <>) xs))
                               nl))) _))))
                (each underlines fl labels)))))))

    (define (label-row-fold-step width)
      (λ((_ start end color text-fmt)
         min-text-width
         max-text-width
         right-edge
         ((left-edge cols) . rest))
        (if text-fmt
          (let* ((min-wrap (min min-text-width max-text-width))
                 (left-space (- end left-edge))
                 (right-space (- right-edge 1 start))
                 (left-fit (>= left-space min-wrap))
                 (right-fit (>= right-space min-wrap))
                 (fmt (wrapped (color text-fmt))))
            (cond
              ((or left-fit right-fit)
                (cons
                  (if (> left-space right-space)
                    `(,(min (+ end 1) width)
                      (,@cols right ,(- end left-edge) ,fmt 1 ,nothing))
                    (let1 new-left-edge (min (+ start max-text-width 1)
                                             right-edge)
                      `(,new-left-edge (,@cols
                                        ,@(if (<= start left-edge)
                                            '()
                                            (list (- start left-edge) nothing))
                                        left
                                        ,(- new-left-edge start)
                                        ,fmt))))
                  rest))
              ((>= (- width start) min-wrap)
                `((0 ())
                  (,width (,@cols
                           ,@(if (<= start left-edge)
                               '()
                               (list (- start left-edge) nothing))
                           left
                           ,(- width start)
                           ,fmt)
                   ,@rest)))
              ((>= end min-wrap)
                `((,(min (+ end 1) width) (right ,end ,fmt))
                  (,left-edge ,cols)
                  ,@rest))
              (else
                `((0 ())
                  (,width (left ,width ,fmt))
                  (,left-edge ,cols)
                  ,@rest))))
          `((,left-edge ,cols) ,@rest))))))
