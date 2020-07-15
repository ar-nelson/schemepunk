(define-library (schemepunk show terminal-width)
  (export get-terminal-width)

  (import (scheme base))

  (cond-expand
    ((and chicken unix)
      (import (ioctl))
      (begin (define (get-terminal-width)
               (cadr (ioctl-winsize)))))
    (gauche
      (import (only (text console) call-with-console
                                   make-default-console
                                   query-screen-size))
      (begin (define (get-terminal-width)
               (guard (e (#t 80))
                 (let-values (((_ w) (call-with-console (make-default-console)
                                                        query-screen-size)))
                   w)))))
    (chibi
      (import (rename (chibi stty) (get-terminal-width %get-terminal-width)))
      (begin (define (get-terminal-width)
               (%get-terminal-width (current-output-port)))))
    (gerbil
      (import (std misc process)
              (scheme read))
      (begin (define (get-terminal-width)
               (or (guard (e (#t #f))
                     (read (open-input-string (run-process (list "tput" "cols")))))
                   80))))
    (else (begin (define (get-terminal-width) 80)))))
