(define-library (schemepunk box)
  (export box box? unbox set-box! update-box!)
  (import (scheme base))
  (cond-expand
    ((or chicken (library (srfi 111)))
      (import (srfi 111)))
    (gerbil
      (import (only (gerbil core) box box? unbox set-box!)))
    (else
      (begin
        (define-record-type Box
          (box value)
          box?
          (value unbox set-box!)))))
  (begin
    (define (update-box! bx proc)
      (set-box! bx (proc (unbox bx))))))
