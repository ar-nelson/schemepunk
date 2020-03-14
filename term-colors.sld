(define-library (schemepunk term-colors)
  (export
    make-color color? color->string

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

    write-colored write-color reset-color

    term-colors-enabled?)

  (import (scheme base)
          (scheme write)
          (scheme process-context)
          (schemepunk syntax))

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
           => (λ s (not (equal? s "0"))))
          (else
            (member (get-environment-variable "TERM")
                    '("xterm" "xterm-color" "xterm-256color" "rxvt" "kterm"
                      "linux" "screen" "screen-256color" "vt100"
                      "rxvt-unicode-256color"))))))

    (define-record-type Color
      (string->color escape)
      color?
      (escape color->string))

    (define (make-color . sgr-parameters)
      (define str (open-output-string))
      (write-char #\escape str)
      (write-char #\[ str)
      (cond
        ((null? sgr-parameters) (write-char #\0 str))
        (else (write-string (car sgr-parameters) str)
              (for-each (λ p (write-char #\; str) (write-string p str))
                        sgr-parameters)))
      (write-char #\m str)
      (string->color (get-output-string str)))

    (define (write-color color)
      (when (term-colors-enabled?)
        (write-string (color->string color))))

    (define (reset-color)
      (write-color reset))

    (define (write-colored color text)
      (write-color color)
      (write-string text)
      (reset-color))

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
