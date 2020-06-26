(define-library (schemepunk function)
  (export identity const flip compose bind complement)
  (import (scheme base))
  (begin
    (define (identity x) x)
    (define (const x) (lambda (y) x))
    (define (flip f) (lambda (x y) (f y x)))
    (define (compose f . fs)
      (if (null? fs) f (let ((g (apply compose fs))) (lambda (x) (f (g x))))))
    (define (bind f . fs)
      (if (null? fs) f (let ((g (apply bind fs))) (lambda (x) (g (f x))))))
    (define (complement f)
      (lambda xs (not (apply f xs))))))

