(define-library (schemepunk path)
  (export current-directory
          path-separator
          path-assemble
          path-disassemble
          path-join
          path-normalize
          path-root?
          path-directory
          path-strip-directory
          path-absolute?
          path-relative?
          relative-path->absolute-path)

  (import (scheme base)
          (scheme write)
          (schemepunk syntax)
          (schemepunk list))

  (cond-expand
    (chibi (import (only (chibi filesystem) current-directory)))
    (chicken (import (only (chicken process-context) current-directory)))
    (gauche (import (only (file util) current-directory)))
    (gerbil (import (only (gerbil core) current-directory)))
    (kawa
      (import (only (kawa base) current-path))
      (begin (define (current-directory) (let1 path (current-path) (path:toString)))))
    (larceny (import (primitives current-directory)))
    (sagittarius (import (only (sagittarius) current-directory))))

  (cond-expand
    ((or windows dos)
      (begin
        (define path-separator #\\)

        (define (path-disassemble path)
          (define len (string-length path))
          (define (read-root starting-at)
            (do ((i starting-at (+ i 1)))
                ((or (= i len)
                     (char=? #\\ (string-ref path i))
                     (char=? #\/ (string-ref path i)))
                  (values (string-copy path 0 i) i))))
          (let1-values (root start elements)
                       (cond
                         ((zero? len)
                           (values "." 0 '()))
                         ((and (> len 2)
                               (char=? #\\ (string-ref path 0))
                               (char=? #\\ (string-ref path 1)))
                           (let1-values (unc start) (read-root 2)
                             (values (string-append unc "\\") start '())))
                         (else
                           (let1-values (first start) (read-root 0)
                             (cond
                               ((string=? "." first)
                                 (values "." start '(".")))
                               ((or (string=? "" first)
                                    (eqv? #\: (string-ref
                                              first
                                              (- (string-length first) 1))))
                                 (values (string-append first "\\") start '()))
                               (else
                                 (values "." start (list first)))))))
            (do ((i start (+ i 1))) ((= i len))
              (let1 c (string-ref path i)
                (when (or (char=? #\\ c) (char=? #\/ c))
                  (unless (= i start)
                    (set! elements (cons (string-copy path start i) elements)))
                  (set! start (+ i 1)))))
            (unless (= start len)
              (set! elements (cons (string-copy path start) elements)))
            (values root (reverse elements))))))
    (else
      (begin
        (define path-separator #\/)

        (define (path-disassemble path)
          (define len (string-length path))
          (values
            (cond
              ((zero? len) ".")
              ((char=? #\/ (string-ref path 0)) "/")
              (else "."))
            (let ((start 0) (elements '()))
              (do ((i 0 (+ i 1))) ((= i len))
                (when (char=? #\/ (string-ref path i))
                  (unless (= i start)
                    (set! elements (cons (string-copy path start i) elements)))
                  (set! start (+ i 1))))
              (unless (= start len)
                (set! elements (cons (string-copy path start) elements)))
              (reverse elements)))))))

  (begin
    (define (path-assemble root elements)
      (with-output-to-string (λ ()
        (if (null? elements)
          (write-string root)
          (begin
            (unless (string=? root ".") (write-string root))
            (write-string (car elements))
            (for-each
              (λ e
                (unless (string=? "." e)
                  (write-char path-separator)
                  (write-string e)))
              (cdr elements)))))))

    (define (path-join path . suffix)
      (let1-values (root elements) (path-disassemble path)
        (path-assemble root (append elements suffix))))

    (define (path-normalize path)
      (let1-values (root elements) (path-disassemble path)
        (match-let1 (..s elements)
            (fold-right
              (λ(elem (..s elems))
                (cond
                  ((string=? "." elem) (list ..s elems))
                  ((string=? ".." elem) (list (cons ".." ..s) elems))
                  ((pair? ..s) (list (cdr ..s) elems))
                  (else (list '() (cons elem elems)))))
              '(() ())
              elements)
          (path-assemble
            (cond-expand
              ((or windows dos)
                (if (string=? "" root)
                    (let1-values (pwd-root _) (path-disassemble (current-directory))
                      pwd-root)
                    root))
              (else root))
            (if (string=? "." root)
              (append ..s elements)
              elements)))))

    (define (path-root? path)
      (let1-values (root elements) (path-disassemble path)
        (and (null? elements)
             (not (string=? "." root)))))

    (define (path-directory path)
      (path-normalize (path-join path "..")))

    (define (path-strip-directory path)
      (let1-values (root elements) (path-disassemble path)
        (if (null? elements) root (last elements))))

    (define (path-absolute? path)
      (not (path-relative? path)))

    (define (path-relative? path)
      (let1-values (root elements) (path-disassemble path)
        (string=? "." root)))

    (define (relative-path->absolute-path path)
      (assume (path-relative? path))
      (let-values (((root elements) (path-disassemble path))
                   ((pwd-root pwd-elements) (path-disassemble (current-directory))))
        (path-normalize (path-assemble pwd-root (append pwd-elements elements)))))))
