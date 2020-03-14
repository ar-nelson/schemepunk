(define-library (schemepunk debug report)
  (export make-report
          make-paragraph
          write-report
          write-reports
          write-report-separator)

  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme write)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk debug indent)
          (schemepunk term-colors))

  (begin
    (define write-report-separator
      (case-lambda
        ((caption)
           (write-color yellow)
           (if (> (string-length caption) (- (output-width) 4))
             (begin
               (write-string "┤ ")
               (write-colored bold-blue
                 (substring caption 0 (- (output-width) 5)))
               (write-color yellow)
               (write-string "… ├"))
             (let1 line-width (- (output-width) (string-length caption) 4)
               (dotimes line-width
                 (write-string "─"))
               (write-string "┤ ")
               (write-colored bold-blue caption)
               (write-color yellow)
               (write-string " ├")))
           (reset-color)
           (newline))
        (()
           (write-color yellow)
           (dotimes (output-width)
             (write-string "─"))
           (reset-color)
           (newline))))

    (define-record-type Report
      (%make-report% title paragraphs)
      report?
      (title report-title)
      (paragraphs report-paragraphs))

    (define (make-report title . paragraphs)
      (%make-report% title paragraphs))

    (define (make-paragraph template . params)
      (define words '())
      (define len (string-length template))
      (define word-start 0)
      (define word-buffer "")
      (define state 'space)
      (do ((i 0 (+ i 1))) ((>= i len))
        (let1 c (string-ref template i)
          (cond
            ((char-whitespace? c)
               (case state
                 ((word var)
                    (set! words
                      (cons (substring template word-start i) words)))
                 ((buffer-word buffer-var)
                    (set! words
                      (cons (string-append word-buffer
                                           (substring template word-start i))
                            words))))
               (set! state 'space))
            ((eqv? c #\{)
               (case state
                 ((space)
                    (set! state 'var))
                 ((word var)
                    (set! word-buffer (substring template word-start i))
                    (set! state 'buffer-var))
                 ((buffer-word buffer-var)
                    (set! word-buffer
                      (string-append word-buffer (substring template word-start i)))
                    (set! state 'buffer-var)))
               (set! word-start i))
            ((and (eqv? c #\}) (> i (+ word-start 1)))
               (case state
                 ((var buffer-var)
                    (let1 param (->> (substring template (+ word-start 1) i)
                                     string->number
                                     (list-ref params))
                      (if (string? param)
                        (begin (set! word-buffer
                                 (case state
                                   ((buffer-var)
                                      (string-append word-buffer param))
                                   (else param)))
                               (set! word-start (+ i 1))
                               (set! state 'buffer-word))
                        (begin (case state
                                 ((buffer-var)
                                    (set! words (cons word-buffer words))))
                               (set! words (cons param words))
                               (set! state 'space)))))
                 ((space)
                    (set! word-start i)
                    (set! state 'word))))
            (else
               (case state
                 ((var)
                    (unless (char-numeric? c) (set! state 'word)))
                 ((buffer-var)
                    (unless (char-numeric? c) (set! state 'buffer-word)))
                 ((space)
                    (set! word-start i)
                    (set! state 'word)))))))
      (case state
        ((word var)
           (set! words (cons (substring template word-start len) words)))
        ((buffer-word buffer-var)
           (set! words (cons (string-append word-buffer
                                            (substring template word-start len))
                             words))))
      (reverse words))

    (define (write-paragraph words)
      (define cols (output-width))
      (define col 0)
      (for-each (λ word
        (when (< col 0)
          (newline)
          (newline)
          (set! col 0))
        (let1 len (unindented-length word)
          (cond
            ((and (not (string? word)) (> len (/ cols 2)))
               (newline)
               (newline)
               (write-indented word)
               (set! col -1))
            ((> (+ col 1 len) cols)
               (newline)
               (write-unindented word)
               (set! col len))
            (else
               (when (> col 0) (write-char #\space))
               (write-unindented word)
               (set! col (+ col 1 len))))))
        words)
      (newline)
      (newline))

    (define (write-reports reports)
      (parameterize ((output-width (max 40 (min 120 (get-terminal-width)))))
        (for-each (λ report
          (write-report-separator (report-title report))
          (newline)
          (for-each (λ paragraph
            (if (list? paragraph)
                (write-paragraph paragraph)
                (begin (write-indented paragraph)
                       (newline)
                       (newline))))
            (report-paragraphs report)))
          reports)
        (write-report-separator)))

    (define (write-report report)
      (write-reports (list report)))))
