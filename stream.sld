(define-library (schemepunk stream)
  (export stream-null stream-cons stream? stream-null? stream-pair? stream-car
          stream-cdr stream-lambda define-stream list->stream port->stream stream
          stream->list stream-append stream-concat stream-constant stream-drop
          stream-drop-while stream-filter stream-fold stream-for-each stream-from
          stream-iterate stream-length stream-let stream-map stream-match
          stream-of stream-range stream-ref stream-reverse stream-scan stream-take
          stream-take-while stream-unfold stream-unfolds stream-zip)

  (cond-expand
    (gerbil
      (import (std srfi 41)))
    (gauche
      (import (scheme base)
              (rename (except (util stream) stream)
                      (stream-take %stream-take)
                      (stream-drop %stream-drop)))
      (begin
        (define-syntax stream
          (syntax-rules ()
            ((_) stream-null)
            ((_ x y ...) (stream-cons x (stream y ...)))))
        (define (stream-take n stream)
          (%stream-take stream n))
        (define (stream-drop n stream)
          (%stream-drop stream n))))
    (else
      (import (srfi 41)))))
