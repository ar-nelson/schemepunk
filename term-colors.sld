(define-library (schemepunk term-colors)
  (export
    make-color color? color->string color-is-reset?
    make-8-bit-color make-24-bit-color make-8-bit-color/bg make-24-bit-color/bg

    fg-black fg-red fg-yellow fg-green
    fg-blue fg-cyan fg-magenta fg-white

    fg-light-black fg-light-red fg-light-yellow fg-light-green
    fg-light-blue fg-light-cyan fg-light-magenta fg-light-white

    attr-bold attr-italic attr-underline attr-negative

    bg-black bg-red bg-yellow bg-green
    bg-blue bg-cyan bg-magenta bg-white

    black red yellow green
    blue cyan magenta white reset

    light-black gray light-red light-yellow light-green
    light-blue light-cyan light-magenta light-white

    bold-black bold-red bold-yellow bold-green
    bold-blue bold-cyan bold-magenta bold-white

    write-colored write-in-color write-color reset-color
    read-color merge-colors

    term-colors-enabled?)

  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme write)
          (scheme process-context)
          (schemepunk syntax)
          (schemepunk list))

  (begin
    (define attr-bold      "1")
    (define attr-italic    "3")
    (define attr-underline "4")
    (define attr-negative  "7")

    (define fg-black   "30")
    (define fg-red     "31")
    (define fg-green   "32")
    (define fg-yellow  "33")
    (define fg-blue    "34")
    (define fg-magenta "35")
    (define fg-cyan    "36")
    (define fg-white   "37")

    (define bg-black   "40")
    (define bg-red     "41")
    (define bg-green   "42")
    (define bg-yellow  "43")
    (define bg-blue    "44")
    (define bg-magenta "45")
    (define bg-cyan    "46")
    (define bg-white   "47")

    (define fg-light-black   "90")
    (define fg-light-red     "91")
    (define fg-light-green   "92")
    (define fg-light-yellow  "93")
    (define fg-light-blue    "94")
    (define fg-light-magenta "95")
    (define fg-light-cyan    "96")
    (define fg-light-white   "97")

    (define term-colors-enabled?
      (make-parameter
        (cond
          ((get-environment-variable "ANSI_ESCAPES_ENABLED")
           => (lambda (s) (not (equal? s "0"))))
          (else
            (member (get-environment-variable "TERM")
                    '("xterm" "xterm-color" "xterm-256color" "rxvt" "kterm"
                      "linux" "screen" "screen-256color" "vt100"
                      "rxvt-unicode-256color"))))))

    (define-record-type Color
      (%make-color params escape)
      color?
      (params color-params)
      (escape color->string))

    (define (make-color . sgr-parameters)
      (define str (open-output-string))
      (write-char #\escape str)
      (write-char #\[ str)
      (cond
        ((null? sgr-parameters) (write-char #\0 str))
        (else (write-string (car sgr-parameters) str)
              (for-each (lambda (p) (write-char #\; str) (write-string p str))
                        (cdr sgr-parameters))))
      (write-char #\m str)
      (%make-color sgr-parameters (get-output-string str)))

    (define write-color
      (case-lambda
        ((color)
          (when (term-colors-enabled?)
            (write-string (color->string color))))
        ((color port)
          (when (term-colors-enabled?)
            (write-string (color->string color) port)))))

    (define reset-color
      (case-lambda
        (() (write-color reset))
        ((port) (write-color reset port))))

    (define (color-is-reset? color)
      (match (color-params color)
        (() #t)
        (("0") #t)
        (else #f)))

    (define+ (write-in-color color text :optional (port (current-output-port)))
      (write-color color port)
      (write-string text port)
      (reset-color port))

    (define write-colored write-in-color)

    (define+ (read-color :optional (next-char read-char))
      (let loop ((c (next-char)) (n '()) (params '()))
        (match c
          ((? char-numeric?)
            (loop (next-char) (cons c n) params))
          (#\m
            (apply make-color (reverse (cons (list->string (reverse n)) params))))
          (#\;
            (loop (next-char) '() (cons (list->string (reverse n)) params)))
          (#\[
            (loop (next-char) n params))
          (#\escape
            (loop (next-char) '() '()))
          (else
            (make-color)))))

    (define (sgr-param-overrides? overrider overridee)
      (cond
        ((zero? (string-length overrider)) #f)
        ((zero? (string-length overridee)) #t)
        (else
          (case (string-ref overrider 0)
            ((#\0) #t)
            ((#\3 #\9)
              (case (string-ref overridee 0)
                ((#\3 #\9) #t)
                (else #f)))
            ((#\4)
              (eqv? #\4 (string-ref overridee 0)))
            (else #f)))))

    (define (extract-long-colors params)
      (let loop ((in params) (out '()) (fg #f) (bg #f))
        (match in
          (() (values (reverse out) fg bg))
          (("38" "2" r g b . rest)
            (loop rest out `("38" "2" ,r ,g ,b) bg))
          (("38" "5" c . rest)
            (loop rest out `("38" "5" ,c) bg))
          (("48" "2" r g b . rest)
            (loop rest out fg `("48" "2" ,r ,g ,b)))
          (("48" "5" c . rest)
            (loop rest out fg `("48" "5" ,c)))
          ((x . rest) (loop rest (cons x out) fg bg)))))

    (define (merge-colors c1 c2)
      (cond
        ((or (not c2) (null? (color-params c2))) #f)
        ((or (not c1) (null? (color-params c1))) c2)
        (else
          (let-values (((ps1 fg1 bg1) (extract-long-colors (color-params c1)))
                       ((ps2 fg2 bg2) (extract-long-colors (color-params c2))))
            (chain (fold
                     (λ(p ps) (remove (cut sgr-param-overrides? p <>) ps))
                     ps1
                     ps2)
                   (append <>
                           ps2
                           (or fg2 fg1 '())
                           (or bg2 bg1 '()))
                   (apply make-color))))))

    (define (make-8-bit-color r g b)
      (assume (integer? r))
      (assume (integer? g))
      (assume (integer? b))
      (assume (<= 0 r 5))
      (assume (<= 0 g 5))
      (assume (<= 0 b 5))
      (make-color "38" "5" (number->string (+ (* 36 r) (* 6 g) b 16))))

    (define (make-24-bit-color r g b)
      (assume (integer? r))
      (assume (integer? g))
      (assume (integer? b))
      (assume (<= 0 r 255))
      (assume (<= 0 g 255))
      (assume (<= 0 b 255))
      (make-color "38" "2" (number->string r) (number->string g) (number->string b)))

    (define (make-8-bit-color/bg r g b)
      (assume (integer? r))
      (assume (integer? g))
      (assume (integer? b))
      (assume (<= 0 r 5))
      (assume (<= 0 g 5))
      (assume (<= 0 b 5))
      (make-color "48" "5" (number->string (+ (* 36 r) (* 6 g) b 16))))

    (define (make-24-bit-color/bg r g b)
      (assume (integer? r))
      (assume (integer? g))
      (assume (integer? b))
      (assume (<= 0 r 255))
      (assume (<= 0 g 255))
      (assume (<= 0 b 255))
      (make-color "48" "2" (number->string r) (number->string g) (number->string b)))

    (define reset   (make-color))
    (define black   (make-color fg-black))
    (define red     (make-color fg-red))
    (define green   (make-color fg-green))
    (define yellow  (make-color fg-yellow))
    (define blue    (make-color fg-blue))
    (define magenta (make-color fg-magenta))
    (define cyan    (make-color fg-cyan))
    (define white   (make-color fg-white))

    (define light-black   (make-color fg-light-black))
    (define light-red     (make-color fg-light-red))
    (define light-green   (make-color fg-light-green))
    (define light-yellow  (make-color fg-light-yellow))
    (define light-blue    (make-color fg-light-blue))
    (define light-magenta (make-color fg-light-magenta))
    (define light-cyan    (make-color fg-light-cyan))
    (define light-white   (make-color fg-light-white))

    (define gray light-black)

    (define bold-black   (make-color fg-black attr-bold))
    (define bold-red     (make-color fg-red attr-bold))
    (define bold-green   (make-color fg-green attr-bold))
    (define bold-yellow  (make-color fg-yellow attr-bold))
    (define bold-blue    (make-color fg-blue attr-bold))
    (define bold-magenta (make-color fg-magenta attr-bold))
    (define bold-cyan    (make-color fg-cyan attr-bold))
    (define bold-white   (make-color fg-white attr-bold))))
