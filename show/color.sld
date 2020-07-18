(define-library (schemepunk show color)
  (export as-red as-blue as-green as-cyan
          as-yellow as-magenta as-white as-black

          as-light-red as-light-blue as-light-green as-light-cyan
          as-light-yellow as-light-magenta as-light-white as-light-black as-gray

          as-bold as-italic as-underline
          as-color as-true-color

          on-red on-blue on-green on-cyan
          on-yellow on-magenta on-white on-black
          on-color on-true-color)

  (import (scheme base)
          (schemepunk syntax)
          (schemepunk generator)
          (schemepunk term-colors)
          (schemepunk show base)
          (schemepunk show span))

  (begin
    (define (color-fmt color fmts)
      (λ vars
        (gmap (λ s (span-with-color s (if (span-color s)
                                        (merge-colors color (span-color s))
                                        color)))
              ((each-in-list fmts) vars))))

    (define (as-red . fmts) (color-fmt red fmts))
    (define (as-blue . fmts) (color-fmt blue fmts))
    (define (as-green . fmts) (color-fmt green fmts))
    (define (as-cyan . fmts) (color-fmt cyan fmts))
    (define (as-yellow . fmts) (color-fmt yellow fmts))
    (define (as-magenta . fmts) (color-fmt magenta fmts))
    (define (as-white . fmts) (color-fmt white fmts))
    (define (as-black . fmts) (color-fmt black fmts))

    (define (as-light-red . fmts) (color-fmt light-red fmts))
    (define (as-light-blue . fmts) (color-fmt light-blue fmts))
    (define (as-light-green . fmts) (color-fmt light-green fmts))
    (define (as-light-cyan . fmts) (color-fmt light-cyan fmts))
    (define (as-light-yellow . fmts) (color-fmt light-yellow fmts))
    (define (as-light-magenta . fmts) (color-fmt light-magenta fmts))
    (define (as-light-white . fmts) (color-fmt light-white fmts))
    (define (as-light-black . fmts) (color-fmt light-black fmts))
    (define as-gray as-light-black)

    (define (as-bold . fmts) (color-fmt (make-color attr-bold) fmts))
    (define (as-italic . fmts) (color-fmt (make-color attr-italic) fmts))
    (define (as-underline . fmts) (color-fmt (make-color attr-underline) fmts))

    (define (as-color r g b . fmts) (color-fmt (make-8-bit-color r g b) fmts))
    (define (as-true-color r g b . fmts) (color-fmt (make-24-bit-color r g b) fmts))

    (define (on-red . fmts) (color-fmt (make-color bg-red) fmts))
    (define (on-blue . fmts) (color-fmt (make-color bg-blue) fmts))
    (define (on-green . fmts) (color-fmt (make-color bg-green) fmts))
    (define (on-cyan . fmts) (color-fmt (make-color bg-cyan) fmts))
    (define (on-yellow . fmts) (color-fmt (make-color bg-yellow) fmts))
    (define (on-magenta . fmts) (color-fmt (make-color bg-magenta) fmts))
    (define (on-white . fmts) (color-fmt (make-color bg-white) fmts))
    (define (on-black . fmts) (color-fmt (make-color bg-black) fmts))

    (define (on-color r g b . fmts) (color-fmt (make-8-bit-color/bg r g b) fmts))
    (define (on-true-color r g b . fmts) (color-fmt (make-24-bit-color/bg r g b) fmts))))

