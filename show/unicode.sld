(define-library (schemepunk show unicode)
  (export terminal-aware
          string-terminal-width string-terminal-width/wide
          substring-terminal-width substring-terminal-width/wide
          upcased downcased)
  (cond-expand
    (chicken
      (import (except (scheme base)
                string-length string-ref string-set! make-string string substring
                string->list list->string string-fill! write-char read-char)
              (utf8)))
    (else
      (import (scheme base))))

  (import (scheme char)
          (schemepunk syntax)
          (schemepunk function)
          (schemepunk generator)
          (schemepunk term-colors)
          (schemepunk show base)
          (schemepunk show span))

  (cond-expand
    (gerbil
      (import (only (std srfi 151) bit-set?)))
    (chicken
      (import (chicken bitwise))
      (begin
        (define (bit-set? index i)
          (not (zero? (bitwise-and i (arithmetic-shift 1 index)))))))
    ((and (not gauche) (library (srfi 151)))
      (import (only (srfi 151) bit-set?)))
    (else
      (import (only (srfi 60) bit-set?))))

  (include "./unicode-char-width.scm")

  (begin
    (define+ (string-terminal-width str
                                    :optional
                                    (start 0)
                                    (end +inf.0)
                                    (tab-width 8)
                                    (ambiguous-is-wide? #f))
      (assume (string? str))
      (assume (integer? start))
      (assume (positive? end))
      (assume (integer? tab-width))
      (assume (<= 0 start end))
      (let ((amb? (if ambiguous-is-wide? char-ambiguous-width? (const #f)))
            (in (open-input-string str)))
        (let loop ((total 0) (i start))
          (if (>= i end)
            total
            (match (read-char in)
              ((? eof-object?) total)
              (#\tab (loop (+ total tab-width) (+ i 1)))
              (#\escape
                (read-color (λ() (set! i (+ i 1)) (read-char in)))
                (loop total i))
              ((? char-zero-width?) (loop total (+ i 1)))
              ((or (? char-full-width?) (? amb?)) (loop (+ total 2) (+ i 1)))
              (else (loop (+ total 1) (+ i 1))))))))

    (define+ (string-terminal-width/wide str
                                         :optional
                                         (start 0)
                                         (end +inf.0)
                                         (tab-width 8))
      (string-terminal-width str start end tab-width #t))

    (define reset-escape (color->string reset))

    (define+ (substring-terminal-width str
                                       :optional
                                       (from 0)
                                       (to +inf.0)
                                       (tab-width 8)
                                       (ambiguous-is-wide? #f))
      (assume (string? str))
      (assume (integer? from))
      (assume (positive? to))
      (assume (integer? tab-width))
      (assume (<= 0 from to))
      (let ((amb? (if ambiguous-is-wide? char-ambiguous-width? (const #f)))
            (reset-at-end? #f)
            (in (open-input-string str))
            (out (open-output-string)))
        (let loop ((width 0) (color #f))
          (if (and color (>= width from))
            (begin (write-color color out)
                   (loop width #f))
            (match (read-char in)
              ((? eof-object?) #f)
              (#\escape
                (let1 new-color (read-color (cut read-char in))
                  (set! reset-at-end? (not (color-is-reset? new-color)))
                  (loop width (merge-colors color new-color))))
              (c
                (let* ((w (match c
                            (#\tab tab-width)
                            ((? char-zero-width?) 0)
                            ((or (? char-full-width?) (? amb?)) 2)
                            (else 1)))
                       (w+ (+ width w)))
                  (cond
                    ((> w+ to) #f)
                    ((or (> width from) (and (not (zero? w)) (= width from)))
                      (write-char c out)
                      (loop w+ #f))
                    (else
                      (loop w+ color))))))))
          (when reset-at-end? (reset-color out))
        (get-output-string out)))

    (define+ (substring-terminal-width/wide str
                                            :optional
                                            (from 0)
                                            (to +inf.0)
                                            (tab-width 8))
      (substring-terminal-width str from to tab-width #t))

    (define (terminal-aware . fmts)
      (fn (ambiguous-is-wide?)
        (with ((string-width (if ambiguous-is-wide?
                               string-terminal-width/wide
                               string-terminal-width))
               (substring/width (if ambiguous-is-wide?
                                  substring-terminal-width/wide
                                  substring-terminal-width)))
          (each-in-list fmts)))))

  (cond-expand
    (chicken
      (import (utf8-case-map))
      (begin
        (define (upcased . fmts)
          (λ=> ((each-in-list fmts))
               (gmap (cut span-map-text utf8-string-upcase <>))))

        (define (downcased . fmts)
          (λ=> ((each-in-list fmts))
               (gmap (cut span-map-text utf8-string-downcase <>))))))
    (else
      (begin
        (define (upcased . fmts)
          (λ=> ((each-in-list fmts))
               (gmap (cut span-map-text string-upcase <>))))

        (define (downcased . fmts)
          (λ=> ((each-in-list fmts))
               (gmap (cut span-map-text string-downcase <>))))))))
