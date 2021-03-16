(define-record-type Flexvector
  (%make-flexvector fv-vector fv-length)
  flexvector?
  (fv-vector vec set-vec!)
  (fv-length flexvector-length set-flexvector-length!))

(define (cap fv)
  (vector-length (vec fv)))

(define (grow! fv)
  (define old-vec (vec fv))
  (define new-vec (make-vector (quotient (* (vector-length old-vec) 3) 2)))
  (vector-copy! new-vec 0 old-vec)
  (set-vec! fv new-vec)
  new-vec)

(define make-flexvector
  (case-lambda
    ((size)
      (assume (>= size 0))
      (%make-flexvector (make-vector (max size 4)) size))
    ((size fill)
      (assume (>= size 0))
      (%make-flexvector (make-vector (max size 4) fill) size))))

(define (flexvector . xs)
  (if (null? xs)
    (%make-flexvector (make-vector 4) 0)
    (list->flexvector xs)))

(define (flexvector-ref fv index)
  (assume (flexvector? fv))
  (assume (integer? index))
  (assume (< -1 index (flexvector-length fv)))
  (vector-ref (vec fv) index))

(define (flexvector-set! fv index x)
  (assume (flexvector? fv))
  (assume (integer? index))
  (assume (< -1 index (flexvector-length fv)))
  (let ((last-value (vector-ref (vec fv) index)))
    (vector-set! (vec fv) index x)
    last-value))

; Chicken's r7rs vector-copy! is broken
; when src and dest overlap and src index < dest index.
(cond-expand
  (chicken
    (define (chicken-safe-vector-copy! dest desti src . o)
      (let1 temp (vector-copy src)
        (apply vector-copy! dest desti temp o))))
  (else
    (define chicken-safe-vector-copy! vector-copy!)))

(define flexvector-add!
  (case-lambda
    ((fv i x)
      (assume (flexvector? fv))
      (assume (integer? i))
      (let* ((len (flexvector-length fv))
             (v (if (< len (cap fv)) (vec fv) (grow! fv))))
        (assume (<= 0 i len))
        (chicken-safe-vector-copy! v (+ i 1) v i len)
        (vector-set! v i x)
        (set-flexvector-length! fv (+ len 1))
        fv))
    ((fv i . xs)
      (flexvector-add-all! fv i xs))))

(define flexvector-add-back!
  (case-lambda
    ((fv x)
      (assume (flexvector? fv))
      (let* ((len (flexvector-length fv))
             (v (if (< len (cap fv)) (vec fv) (grow! fv))))
        (vector-set! v len x)
        (set-flexvector-length! fv (+ len 1))
        fv))
    ((fv x . xs)
      (flexvector-add-back! fv x)
      (apply flexvector-add-back! fv xs))))

(define (flexvector-add-all! fv i xs)
  (assume (flexvector? fv))
  (assume (integer? i))
  (assume (list? xs))
  (let* ((len (flexvector-length fv))
         (xv (list->vector xs))
         (xvlen (vector-length xv))
         (v (let lp ((v (vec fv)))
              (if (< (+ len xvlen) (vector-length v)) v (lp (grow! fv))))))
    (assume (<= 0 i len))
    (chicken-safe-vector-copy! v (+ i xvlen) v i len)
    (vector-copy! v i xv 0 xvlen)
    (set-flexvector-length! fv (+ len xvlen))
    fv))

(define (flexvector-remove! fv i)
  (assume (flexvector? fv))
  (assume (integer? i))
  (assume (<= 0 i (- (flexvector-length fv) 1)))
  (let ((removed (flexvector-ref fv i)))
    (flexvector-remove-range! fv i (+ i 1))
    removed))

(define (flexvector-remove-range! fv start end)
  (assume (flexvector? fv))
  (let ((len (flexvector-length fv)))
    (when (< start 0) (set! start 0))
    (when (>= end len) (set! end len))
    (assume (<= start end))
    (chicken-safe-vector-copy! (vec fv) start (vec fv) end)
    (let ((new-len (- len (- end start))))
      (vector-fill! (vec fv) #f new-len len)
      (set-flexvector-length! fv new-len)))
  fv)

(define (flexvector-clear! fv)
  (assume (flexvector? fv))
  (set-vec! fv (make-vector 4))
  (set-flexvector-length! fv 0)
  fv)

(define vector->flexvector
  (case-lambda
    ((vec)
      (assume (vector? vec))
      (vector->flexvector vec 0 (vector-length vec)))
    ((vec start)
      (assume (vector? vec))
      (vector->flexvector vec start (vector-length vec)))
    ((vec start end)
      (assume (vector? vec))
      (assume (<= 0 start end (vector-length vec)))
      (let ((len (- end start)))
        (cond
          ((< len 4)
            (let ((new-vec (make-vector 4)))
              (vector-copy! new-vec 0 vec start end)
              (%make-flexvector new-vec len)))
          (else
            (%make-flexvector (vector-copy vec start end) len)))))))

(define flexvector->vector
  (case-lambda
    ((fv)
      (assume (flexvector? fv))
      (flexvector->vector fv 0 (flexvector-length fv)))
    ((fv start)
      (assume (flexvector? fv))
      (flexvector->vector fv start (flexvector-length fv)))
    ((fv start end)
      (assume (flexvector? fv))
      (assume (<= 0 start end (flexvector-length fv)))
      (vector-copy (vec fv) start end))))

(define (list->flexvector xs)
  (let* ((vec (list->vector xs))
         (len (vector-length vec)))
    (cond
      ((< len 4)
        (let ((new-vec (make-vector 4)))
          (vector-copy! new-vec 0 vec)
          (%make-flexvector new-vec len)))
      (else
        (%make-flexvector vec len)))))

(define flexvector-filter/index!
  (case-lambda
    ((pred? fv)
      (assume (flexvector? fv))
      (let ((v (vec fv)) (len (flexvector-length fv)))
        (let lp ((i 0) (j 0))
          (cond
            ((>= i len)
              (set-flexvector-length! fv j)
              fv)
            ((pred? i (vector-ref v i))
              (unless (= i j) (vector-set! v j (vector-ref v i)))
              (lp (+ i 1) (+ j 1)))
            (else
              (lp (+ i 1) j))))))
    ((pred? fv . fvs)
      (assume (flexvector? fv))
      (let ((v (vec fv)) (len (flexvector-length fv)))
        (let lp ((i 0) (j 0))
          (cond
            ((>= i len)
              (set-flexvector-length! fv j)
              fv)
            ((apply pred?
                    i
                    (vector-ref v i)
                    (map (lambda (fv) (flexvector-ref fv i)) fvs))
              (unless (= i j) (vector-set! v j (vector-ref v i)))
              (lp (+ i 1) (+ j 1)))
            (else
              (lp (+ i 1) j))))))))

(define flexvector-copy
  (case-lambda
    ((fv)
      (assume (flexvector? fv))
      (%make-flexvector (vector-copy (vec fv))
                        (flexvector-length fv)))
    ((fv start)
      (assume (flexvector? fv))
      (flexvector-copy fv start (flexvector-length fv)))
    ((fv start end)
      (assume (flexvector? fv))
      (assume (<= 0 start end (flexvector-length fv)))
      (vector->flexvector (vector-copy (vec fv) start end)))))

(define flexvector-copy!
  (case-lambda
    ((to at from)
      (assume (flexvector? from))
      (flexvector-copy! to at from 0 (flexvector-length from)))
    ((to at from start)
      (assume (flexvector? from))
      (flexvector-copy! to at from start (flexvector-length from)))
    ((to at from start end)
      (assume (flexvector? to))
      (assume (<= 0 at (flexvector-length to)))
      (assume (<= 0 start end (flexvector-length from)))
      (let* ((vf (vec from))
             (lt (+ (flexvector-length to) (- end start)))
             (vt (let lp ((v (vec to)))
                   (if (< lt (vector-length v)) v (lp (grow! to))))))
        (chicken-safe-vector-copy! vt at vf start end)
        (set-flexvector-length! to
          (max (flexvector-length to) (+ at (- end start))))))))
