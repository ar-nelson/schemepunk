(define-library (schemepunk show pretty)
  (export pretty pretty-shared pretty-simply pretty-color
          indent-size)
  (import (scheme base)
          (schemepunk syntax)
          (schemepunk show base)
          (schemepunk show span)
          (schemepunk show block)
          (schemepunk show block datum))

  (begin
    (define indent-size (make-state-variable "indent-size" 2))

    (define (pretty obj)
      (fn (width string-width indent-size)
        (span-generator->formatter
          (block->span-generator/indented
            (datum->block obj)
            width
            string-width
            indent-size))))

    (define (pretty-shared obj)
      (fn (width string-width indent-size)
        (span-generator->formatter
          (block->span-generator/indented
            (datum->block/shared obj)
            width
            string-width
            indent-size))))

    (define (pretty-simply obj)
      (fn (width string-width indent-size)
        (span-generator->formatter
          (block->span-generator/indented
            (datum->block/simple obj)
            width
            string-width
            indent-size))))

    (define (pretty-color obj)
      (fn (width string-width indent-size)
        (span-generator->formatter
          (with-default-datum-colors (λ()
            (block->span-generator/indented
              (datum->block obj)
              width
              string-width
              indent-size))))))))
