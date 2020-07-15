(define-library (schemepunk show unicode)
  (export terminal-aware
          string-terminal-width string-terminal-width/wide
          substring-terminal-width substring-terminal-width/wide
          upcased downcased)
  (import (scheme base)
          (scheme char)
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
    (define (skip-escape str i)
      (define len (string-length str))
      (and (> len (+ i 2))
           (eqv? #\[ (string-ref str (+ i 1)))
           (let loop ((i (+ i 2)))
             (and (< i len)
                  (let1 c (string-ref str i)
                    (if (or (char-numeric? c) (eqv? c #\;))
                      (loop (+ i 1))
                      (+ i 1)))))))

    (define+ (string-terminal-width str
                                    :optional
                                    (start 0)
                                    (end (string-length str))
                                    (tab-width 8)
                                    (ambiguous-is-wide? #f))
      (define amb? (if ambiguous-is-wide? char-ambiguous-width? (const #f)))
      (assume (string? str))
      (assume (integer? start))
      (assume (integer? end))
      (assume (integer? tab-width))
      (assume (<= 0 start end (string-length str)))
      (let loop ((total 0) (i start))
        (if (>= i end)
          total
          (match (string-ref str i)
            (#\tab (loop (+ total tab-width) (+ i 1)))
            (#\escape (loop total (or (skip-escape str i) (+ i 1))))
            ((? char-zero-width?) (loop total (+ i 1)))
            ((or (? char-full-width?) (? amb?)) (loop (+ total 2) (+ i 1)))
            (else (loop (+ total 1) (+ i 1)))))))

    (define+ (string-terminal-width/wide str
                                         :optional
                                         (start 0)
                                         (end (string-length str))
                                         (tab-width 8))
      (string-terminal-width str start end tab-width #t))

    (define reset-escape (color->string reset))

    (define+ (substring-terminal-width str
                                       :optional
                                       (start 0)
                                       (end +inf.0)
                                       (tab-width 8)
                                       (ambiguous-is-wide? #f))
      (define amb? (if ambiguous-is-wide? char-ambiguous-width? (const #f)))
      (define reset-at-end? #f)
      (assume (string? str))
      (assume (integer? start))
      (assume (positive? end))
      (assume (integer? tab-width))
      (assume (<= 0 start end))
      (with-output-to-string (λ ()
        (let loop ((width 0) (i 0) (color #f))
          (cond
            ((>= i (string-length str)) #f)
            ((and color (>= width start))
              (write-color color)
              (loop width i #f))
            ((and (eqv? #\escape (string-ref str i)) (skip-escape str i))
              => (λ escape-end
                   (let1 escape (substring str i escape-end)
                     (set! reset-at-end? (not (equal? escape reset-escape)))
                     (cond
                       ((>= width start)
                         (write-string escape)
                         (loop width escape-end #f))
                       (else
                         (chain (cute read-char (open-input-string escape))
                                (read-color)
                                (merge-colors color)
                                (loop width escape-end)))))))
            (else
              (let* ((c (string-ref str i))
                     (w (match c
                          (#\tab tab-width)
                          ((? char-zero-width?) 0)
                          ((or (? char-full-width?) (? amb?)) 2)
                          (else 1)))
                     (w+ (+ width w)))
                (cond
                  ((> w+ end) #f)
                  ((or (> width start) (and (not (zero? w)) (= width start)))
                    (write-char c)
                    (loop w+ (+ i 1) #f))
                  (else
                    (loop w+ (+ i 1) color)))))))
        (when reset-at-end? (reset-color)))))

    (define+ (substring-terminal-width/wide str
                                            :optional
                                            (start 0)
                                            (end +inf.0)
                                            (tab-width 8))
      (substring-terminal-width str start end tab-width #t))

    (define (terminal-aware . fmts)
      (fn (ambiguous-is-wide?)
        (with ((string-width (if ambiguous-is-wide?
                               string-terminal-width/wide
                               string-terminal-width))
               (substring/width (if ambiguous-is-wide?
                                  substring-terminal-width/wide
                                  substring-terminal-width)))
          (each-in-list fmts))))

    (define (upcased . fmts)
      (λ=> ((each-in-list fmts))
           (gmap (cut span-map-text string-upcase <>))))

    (define (downcased . fmts)
      (λ=> ((each-in-list fmts))
           (gmap (cut span-map-text string-downcase <>))))))
