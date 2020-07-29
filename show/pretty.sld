(define-library (schemepunk show pretty)
  (export pretty pretty-shared pretty-simply pretty-color
          pretty-json pretty-json-color indent-size)
  (import (scheme base)
          (schemepunk syntax)
          (schemepunk show base)
          (schemepunk show span)
          (schemepunk show block)
          (schemepunk show block datum)
          (schemepunk show block json))

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
              indent-size))))))

    (define (pretty-json json)
      (fn (width string-width indent-size)
        (span-generator->formatter
          (block->span-generator/indented
            (json->block json)
            width
            string-width
            indent-size))))

    (define (pretty-json-color json)
      (fn (width string-width indent-size)
        (span-generator->formatter
          (with-default-json-colors (λ()
            (block->span-generator/indented
              (json->block json)
              width
              string-width
              indent-size))))))))
