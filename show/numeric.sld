(define-library (schemepunk show numeric)
  (export numeric->string numeric->string/si)

  (import (scheme base)
          (scheme char)
          (scheme complex)
          (scheme inexact)
          (scheme write)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk string))

  (begin
    ;; string utilities
    (define (string-replace-all str ch1 ch2)
      (with-output-to-string (λ()
        (string-for-each
          (λ ch (display (if (eqv? ch ch1) ch2 ch)))
          str))))

    (define (string-intersperse-right str sep rule)
      (let lp ((i (string-length str))
               (rule rule)
               (res '()))
        (let* ((offset (if (pair? rule) (car rule) rule))
               (i2 (if offset (- i offset) 0)))
          (if (<= i2 0)
            (apply string-append (cons (string-copy str 0 i) res))
            (lp i2
                (if (and (pair? rule) (not (null? (cdr rule))))
                  (cdr rule)
                  rule)
                (cons sep (cons (string-copy str i2 i) res)))))))

    ;; numeric formatting
    (define (char-mirror c)
      (case c ((#\() #\)) ((#\[) #\]) ((#\{) #\}) ((#\<) #\>) (else c)))

    (define (integer-log a base)
      (if (zero? a)
        0
        ;; (exact (ceiling (/ (log (+ a 1)) (log base))))
        (do ((ndigits 1 (+ ndigits 1))
             (p base (* p base)))
            ((> p a) ndigits))))

    ;; General formatting utilities.
    (define (get-scale q radix)
      (expt radix (- (integer-log q radix) 1)))

    (define (char-digit d)
      (cond ((char? d) d)
            ((< d 10) (integer->char (+ d (char->integer #\0))))
            (else (integer->char (+ (- d 10) (char->integer #\a))))))

    (define (round-up ls radix)
      (let lp ((ls ls) (res '()))
        (cond
          ((null? ls)
            (cons 1 res))
          ((not (number? (car ls)))
            (lp (cdr ls) (cons (car ls) res)))
          ((= (car ls) (- radix 1))
            (lp (cdr ls) (cons 0 res)))
          (else
            (append (reverse res) (cons (+ 1 (car ls)) (cdr ls)))))))

    (define (maybe-round n d ls radix)
      (let* ((q (quotient n d))
             (digit (* 2 (if (>= q radix)
                           (quotient q (get-scale q radix))
                           q))))
        (if (or (> digit radix)
                (and (= digit radix)
                     (let ((prev (find integer? ls)))
                       (and prev (odd? prev)))))
          (round-up ls radix)
          ls)))

    (define (maybe-trim-zeros i res inexact? precision dec-ls)
      (if (and (not precision) (positive? i))
        (let lp ((res res))
          (cond
            ((and (pair? res) (eqv? 0 (car res))) (lp (cdr res)))
            ((and (pair? res)
                  (eqv? (car dec-ls) (car res))
                  (null? (cdr dec-ls)))
              (if inexact?
                (cons 0 res)      ; "1.0"
                (cdr res)))       ; "1"
            (else res)))
        res))

    ;; General slow loop to generate digits one at a time, for
    ;; non-standard radixes or writing rationals with a fixed
    ;; precision.
    (define (display-general n-orig radix precision dec-sep)
      (let* ((p (exact n-orig))
             (n (numerator p))
             (d (denominator p))
             (dec-ls (if (char? dec-sep)
                       (list dec-sep)
                       (reverse (string->list dec-sep)))))
        (let lp ((n n)
                 (i (if (zero? p) -1 (- (integer-log p radix))))
                 (res '()))
          (cond
            ;; Use a fixed precision if specified, otherwise generate
            ;; 15 decimals.
            ((if precision (< i precision) (< i 16))
              (let ((res (if (zero? i)
                           (append dec-ls (if (null? res) (cons 0 res) res))
                           res))
                    (q (quotient n d)))
                (cond
                  ((< i -1)
                    (let* ((scale (expt radix (- -1 i)))
                           (digit (quotient q scale))
                           (n2 (- n (* d digit scale))))
                      (lp n2 (+ i 1) (cons digit res))))
                  (else
                    (lp (* (remainder n d) radix)
                        (+ i 1)
                        (cons q res))))))
            (else
              (chain (maybe-round n d res radix)
                     (maybe-trim-zeros i <> (inexact? n-orig) precision dec-ls)
                     (reverse)
                     (map char-digit)
                     (list->string)))))))

    ;; Generate a fixed precision decimal result by post-editing the
    ;; result of string->number.
    (define (display-fixed n radix precision dec-sep)
      (cond
        ((and (eqv? radix 10) (zero? precision) (inexact? n))
          (number->string (exact (round n))))
        ((and (eqv? radix 10) (or (integer? n) (inexact? n)))
          (let* ((s (number->string n))
                 (end (string-length s))
                 (dec (string-index s (is _ char=? #\.)))
                 (digits (and dec (- end dec))))
            (cond
             ((string-index s (is _ char=? #\e))
               (display-general n radix precision dec-sep))
             ((not digits)
               (string-append s (if (char? dec-sep) (string dec-sep) dec-sep)
                                (make-string precision #\0)))
             ((<= digits precision)
               (string-append s (make-string (- precision digits -1) #\0)))
             (else
               (let* ((last (- end (- digits precision 1)))
                      (res (string-copy s 0 last)))
                 (if (and
                       (< last end)
                       (let1 next (digit-value (string-ref s last))
                         (or (> next 5)
                             (and (= next 5)
                                  (> last 0)
                                  (memv (digit-value (string-ref s (- last 1)))
                                        '(1 3 5 7 9))))))
                   (chain (string->list res)
                          (map digit-value)
                          (reverse)
                          (round-up <> radix)
                          (map char-digit)
                          (reverse)
                          (list->string))
                   res))))))
        (else
          (display-general n radix precision dec-sep))))

    ;; Generate any unsigned real number.
    (define (display-positive-real n radix precision dec-sep)
      (cond
        (precision
          (display-fixed n radix precision dec-sep))
        ((memv radix (if (exact? n) '(2 8 10 16) '(10)))
          (number->string n radix))
        (else
          (display-general n radix precision dec-sep))))

    ;; Insert commas according to the current comma-rule.
    (define (insert-commas str comma-rule comma-sep dec-sep)
      (let* ((dec-pos (or (if (string? dec-sep)
                            (string-contains str dec-sep)
                            (string-index str (is _ char=? dec-sep)))
                          (string-length str)))
             (left (string-copy str 0 dec-pos))
             (right (string-copy str dec-pos))
             (sep (cond ((char? comma-sep) (string comma-sep))
                        ((string? comma-sep) comma-sep)
                        ((eqv? #\, dec-sep) ".")
                        (else ","))))
        (string-append
          (string-intersperse-right left sep comma-rule)
          right)))

    (define-syntax is-neg-zero?
      (syntax-rules ()
        ((_ n)
          (is-neg-zero? (-0.0) n))
        ((_ (0.0) n)                ; -0.0 is not distinguished?
          #f)
        ((_ (-0.0) n)
          (eqv? -0.0 n))))

    (define (negative?* n)
      (or (negative? n)
          (is-neg-zero? n)))

    (define+ (numeric->string n
                              :optional
                              (radix 10)
                              (precision #f)
                              (sign-rule #f)
                              (comma-rule #f)
                              (comma-sep #\,)
                              (decimal-sep #f)
                              (decimal-align #f))
      (define dec-sep (or decimal-sep (if (eqv? comma-sep #\.) #\, #\.)))
      ;; Post-process a positive real number with decimal char fixup
      ;; and commas as needed.
      (define (wrap-comma n)
        (if (and (not precision) (exact? n) (not (integer? n)))
          (string-append (wrap-comma (numerator n))
                         "/"
                         (wrap-comma (denominator n)))
          (let* ((s0 (display-positive-real n radix precision dec-sep))
                 (s1 (if (or (eqv? #\. dec-sep)
                             (equal? "." dec-sep))
                       s0
                       (string-replace-all s0 #\. dec-sep))))
            (if comma-rule
              (insert-commas s1 comma-rule comma-sep dec-sep)
              s1))))
      ;; Wrap the sign of a real number, forcing a + prefix or using
      ;; parentheses (n) for negatives according to sign-rule.
      (define (wrap-sign n sign-rule)
        (cond
          ((negative?* n)
            (cond
              ((char? sign-rule)
                (string-append (string sign-rule)
                               (wrap-comma (- n))
                               (string (char-mirror sign-rule))))
              ((pair? sign-rule)
                (string-append (car sign-rule)
                               (wrap-comma (- n))
                               (cdr sign-rule)))
              (else
                (string-append "-" (wrap-comma (- n))))))
          ((eq? #t sign-rule)
            (string-append "+" (wrap-comma n)))
          (else
            (wrap-comma n))))
      ;; Format a single real number with padding as necessary.
      (define (display-real n sign-rule)
        (cond
          ((finite? n)
            (let* ((s (wrap-sign n sign-rule))
                   (dec-pos (if decimal-align
                              (or (if (string? dec-sep)
                                    (string-contains s dec-sep)
                                    (string-index s (is _ char=? dec-sep)))
                                  (string-length s))
                              0))
                   (diff (- (or decimal-align 0) dec-pos 1)))
              (if (positive? diff)
                (string-append (make-string diff #\space) s)
                s)))
          (else
            (number->string n))))
      ;; Write any number.
      (cond
        ((and radix (not (and (integer? radix) (<= 2 radix 36))))
          (error "invalid radix for numeric formatting" radix))
        ((zero? (imag-part n))
          (display-real (real-part n) sign-rule))
        (else
          (string-append (display-real (real-part n) sign-rule)
                         (display-real (imag-part n) #t)
                         "i"))))

    (define si-scale
      (let* ((names10 #("" "k" "M" "G" "T" "E" "P" "Z" "Y"))
             (names-10 #("" "m" "µ" "n" "p" "f" "a" "z" "y"))
             (names2 (vector-map
                       (matchλ ("" "") ("k" "Ki") (s (string-append s "i")))
                       names10))
             (names-2 (vector-map
                        (matchλ ("" "") (s (string-append s "i")))
                        names-10)))
        (define (round-to n k)
          (/ (round (* n k)) k))
        (λ (n base)
          (let* ((log-n (log n))
                 (names (if (negative? log-n)
                          (if (= base 1024) names-2 names-10)
                          (if (= base 1024) names2 names10)))
                 (k (min (exact ((if (negative? log-n) ceiling floor)
                                 (/ (abs log-n) (log base))))
                         (- (vector-length names) 1)))
                 (n2 (round-to (/ n (expt base (if (negative? log-n) (- k) k)))
                               10)))
            (values (if (integer? n2) (exact n2) (inexact n2))
                    (vector-ref names k))))))

    (define+ (numeric->string/si n
                                 :optional
                                 (base 1000)
                                 (separator "")
                                 (radix 10)
                                 (precision #f)
                                 (sign-rule #f)
                                 (comma-rule #f)
                                 (comma-sep #\,)
                                 (decimal-sep #f)
                                 (decimal-align #f))
      (let1-values (scaled suffix) (si-scale n base)
        ; TODO: Apply decimal-align after appending suffix
        (string-append (numeric->string scaled
                                        radix
                                        precision
                                        sign-rule
                                        comma-rule
                                        comma-sep
                                        decimal-sep
                                        decimal-align)
                       separator
                       suffix)))))
