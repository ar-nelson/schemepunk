(define flexvector-unfold
  (case-lambda
    ((p f g seed)
      (define fv (flexvector))
      (assume (procedure? p))
      (assume (procedure? f))
      (assume (procedure? g))
      (do ((seed seed (g seed))) ((p seed) fv)
        (flexvector-add-back! fv (f seed))))
    ((p f g . seeds)
      (define fv (flexvector))
      (assume (procedure? p))
      (assume (procedure? f))
      (assume (procedure? g))
      (do ((seeds seeds (let-values ((seeds (apply g seeds))) seeds)))
          ((apply p seeds) fv)
        (flexvector-add-back! fv (apply f seeds))))))

(define (flexvector-unfold-right . args)
  (define fv (apply flexvector-unfold args))
  (flexvector-reverse! fv)
  fv)

(define flexvector-fill!
  (case-lambda
    ((fv fill)
      (flexvector-fill! fv fill 0 (flexvector-length fv)))
    ((fv fill start)
      (flexvector-fill! fv fill start (flexvector-length fv)))
    ((fv fill start end)
      (let ((actual-end (min end (flexvector-length fv))))
        (do ((i (max 0 start) (+ i 1)))
            ((>= i actual-end))
          (flexvector-set! fv i fill))))))

(define (flexvector-reverse-copy . args)
  (define fv (apply flexvector-copy args))
  (flexvector-reverse! fv)
  fv)

(define flexvector-reverse-copy!
  (case-lambda
    ((to at from)
      (assume (flexvector? from))
      (flexvector-reverse-copy! to at from 0 (flexvector-length from)))
    ((to at from start)
      (assume (flexvector? from))
      (flexvector-reverse-copy! to at from start (flexvector-length from)))
    ((to at from start end)
      (flexvector-copy! to at from start end)
      (flexvector-reverse! to at (+ at (- end start))))))

(define (flexvector-append! fv . fvs)
  (assume (flexvector? fv))
  (assume (every flexvector? fvs))
  (for-each
    (lambda (fv2) (flexvector-copy! fv (flexvector-length fv) fv2))
    fvs)
  fv)

(define (flexvector-front fv)
  (assume (flexvector? fv))
  (assume (not (flexvector-empty? fv)))
  (flexvector-ref fv 0))

(define (flexvector-back fv)
  (assume (flexvector? fv))
  (assume (not (flexvector-empty? fv)))
  (flexvector-ref fv (- (flexvector-length fv) 1)))

(define flexvector-add-front!
  (case-lambda
    ((fv x) (flexvector-add! fv 0 x))
    ((fv . xs) (apply flexvector-add! fv 0 xs))))

(define (flexvector-remove-front! fv)
  (assume (flexvector? fv))
  (assume (not (flexvector-empty? fv)))
  (flexvector-remove! fv 0))

(define (flexvector-remove-back! fv)
  (assume (flexvector? fv))
  (assume (not (flexvector-empty? fv)))
  (flexvector-remove! fv (- (flexvector-length fv) 1)))

(define (flexvector=? eq . o)
  (cond
    ((null? o) #t)
    ((null? (cdr o)) #t)
    (else
      (and (let* ((fv1 (car o))
                  (fv2 (cadr o))
                  (len (flexvector-length fv1)))
             (and (= len (flexvector-length fv2))
                  (let lp ((i 0))
                    (or (>= i len)
                        (and (eq (flexvector-ref fv1 i) (flexvector-ref fv2 i))
                             (lp (+ i 1)))))))
           (apply flexvector=? eq (cdr o))))))

(define (flexvector-fold kons knil fv1 . o)
  (assume (procedure? kons))
  (assume (flexvector? fv1))
  (let ((len (flexvector-length fv1)))
    (if (null? o)
        (let lp ((i 0) (acc knil))
          (if (>= i len) acc (lp (+ i 1) (kons acc (flexvector-ref fv1 i)))))
        (let lp ((i 0) (acc knil))
          (if (>= i len)
              acc
              (lp (+ i 1)
                  (apply kons acc (flexvector-ref fv1 i)
                         (map (lambda (fv) (flexvector-ref fv i)) o))))))))

(define (flexvector-fold-right kons knil fv1 . o)
  (assume (procedure? kons))
  (assume (flexvector? fv1))
  (let ((len (flexvector-length fv1)))
    (if (null? o)
        (let lp ((i (- len 1)) (acc knil))
          (if (negative? i) acc (lp (- i 1) (kons acc (flexvector-ref fv1 i)))))
        (let lp ((i (- len 1)) (acc knil))
          (if (negative? i)
              acc
              (lp (- i 1)
                  (apply kons acc (flexvector-ref fv1 i)
                         (map (lambda (fv) (flexvector-ref fv i)) o))))))))

(define flexvector-for-each/index
  (case-lambda
    ((proc fv)
      (assume (procedure? proc))
      (assume (flexvector? fv))
      (let ((len (flexvector-length fv)))
        (do ((i 0 (+ i 1))) ((= i len))
          (proc i (flexvector-ref fv i)))))
    ((proc . fvs)
      (assume (procedure? proc))
      (let ((len (apply min (map flexvector-length fvs))))
        (do ((i 0 (+ i 1))) ((= i len))
          (apply proc i (map (lambda (fv) (flexvector-ref fv i)) fvs)))))))

(define flexvector-for-each
  (case-lambda
    ((proc fv)
      (assume (procedure? proc))
      (flexvector-for-each/index (lambda (i x) (proc x)) fv))
    ((proc . fvs)
      (assume (procedure? proc))
      (apply flexvector-for-each/index (lambda (i . xs) (apply proc xs)) fvs))))

(define flexvector-map/index!
  (case-lambda
    ((proc fv)
      (assume (procedure? proc))
      (assume (flexvector? fv))
      (flexvector-for-each/index
        (lambda (i x) (flexvector-set! fv i (proc i x)))
        fv)
      fv)
    ((proc fv . fvs)
      (assume (procedure? proc))
      (assume (flexvector? fv))
      (apply flexvector-for-each/index
        (lambda (i . xs) (flexvector-set! fv i (apply proc i xs)))
        fv
        fvs)
      fv)))

(define flexvector-map!
  (case-lambda
    ((proc fv)
      (assume (procedure? proc))
      (flexvector-map/index! (lambda (i x) (proc x)) fv))
    ((proc . fvs)
      (assume (procedure? proc))
      (apply flexvector-map/index! (lambda (i . xs) (apply proc xs)) fvs))))

(define (flexvector-map/index proc fv . fvs)
  (assume (flexvector? fv))
  (apply flexvector-map/index! proc (flexvector-copy fv) fvs))

(define (flexvector-map proc fv . fvs)
  (assume (flexvector? fv))
  (apply flexvector-map! proc (flexvector-copy fv) fvs))

(define (flexvector-append-map/index proc fv . fvs)
  (define out (flexvector))
  (flexvector-for-each
    (lambda (x) (flexvector-append! out x))
    (apply flexvector-map/index proc fv fvs))
  out)

(define (flexvector-append-map proc fv . fvs)
  (define out (flexvector))
  (flexvector-for-each
    (lambda (x) (flexvector-append! out x))
    (apply flexvector-map proc fv fvs))
  out)

(define flexvector-filter!
  (case-lambda
    ((pred? fv)
      (assume (procedure? pred?))
      (assume (flexvector? fv))
      (flexvector-filter/index! (lambda (i x) (pred? x)) fv))
    ((pred? . fvs)
      (assume (procedure? pred?))
      (apply flexvector-filter/index! (lambda (i . xs) (apply pred? xs)) fvs))))

(define (flexvector-filter/index proc fv . fvs)
  (assume (flexvector? fv))
  (apply flexvector-filter/index! proc (flexvector-copy fv) fvs))

(define (flexvector-filter proc fv . fvs)
  (assume (flexvector? fv))
  (apply flexvector-filter! proc (flexvector-copy fv) fvs))

(define (flexvector-index pred? fv1 . o)
  (assume (procedure? pred?))
  (assume (flexvector? fv1))
  (let ((len (flexvector-length fv1)))
    (let lp ((i 0))
      (and (< i len)
           (if (apply pred?
                      (flexvector-ref fv1 i)
                      (map (lambda (fv) (flexvector-ref fv i)) o))
               i
               (lp (+ i 1)))))))

(define (flexvector-index-right pred? fv1 . o)
  (assume (procedure? pred?))
  (assume (flexvector? fv1))
  (let ((len (flexvector-length fv1)))
    (let lp ((i (- len 1)))
      (and (>= i 0)
           (if (apply pred?
                      (flexvector-ref fv1 i)
                      (map (lambda (fv) (flexvector-ref fv i)) o))
               i
               (lp (- i 1)))))))

(define (complement f)
  (lambda args (not (apply f args))))

(define (flexvector-skip pred? fv1 . o)
  (assume (procedure? pred?))
  (assume (flexvector? fv1))
  (apply flexvector-index (complement pred?) fv1 o))

(define (flexvector-skip-right pred? fv1 . o)
  (assume (procedure? pred?))
  (assume (flexvector? fv1))
  (apply flexvector-index-right (complement pred?) fv1 o))

(define flexvector-binary-search
  (case-lambda
    ((fv value cmp)
      (flexvector-binary-search fv value cmp 0 (flexvector-length fv)))
    ((fv value cmp start)
      (flexvector-binary-search fv value cmp start (flexvector-length fv)))
    ((fv value cmp start end)
      (assume (flexvector? fv))
      (assume (procedure? cmp))
      (assume (integer? start))
      (assume (integer? end))
      (assume (<= start end))
      (let lp ((lo (max start 0))
               (hi (- (min end (flexvector-length fv)) 1)))
        (and (<= lo hi)
             (let* ((mid (quotient (+ lo hi) 2))
                    (x (flexvector-ref fv mid))
                    (y (cmp value x)))
               (cond
                 ((< y 0) (lp lo (- mid 1)))
                 ((> y 0) (lp (+ mid 1) hi))
                 (else mid))))))))

(define (flexvector-any pred? fv . o)
  (assume (procedure? pred?))
  (assume (flexvector? fv))
  (let ((len (apply min (flexvector-length fv) (map flexvector-length o))))
    (let lp ((i 0))
      (and (< i len)
           (or (apply pred?
                      (flexvector-ref fv i)
                      (map (lambda (v) (flexvector-ref v i)) o))
               (lp (+ i 1)))))))

(define (flexvector-every pred? fv . o)
  (assume (procedure? pred?))
  (assume (flexvector? fv))
  (let ((len (apply min (flexvector-length fv) (map flexvector-length o))))
    (or (zero? len)
      (let lp ((i 0))
        (let ((x (apply pred?
                        (flexvector-ref fv i)
                        (map (lambda (v) (flexvector-ref v i)) o))))
          (if (= i (- len 1))
              x
              (and x (lp (+ i 1)))))))))

(define (flexvector-swap! fv i j)
  (assume (flexvector? fv))
  (assume (integer? i))
  (assume (integer? j))
  (let ((tmp (flexvector-ref fv i)))
    (flexvector-set! fv i (flexvector-ref fv j))
    (flexvector-set! fv j tmp)))

(define (flexvector-reverse! fv . o)
  (assume (flexvector? fv))
  (let lp ((left (if (pair? o) (car o) 0))
           (right (- (if (and (pair? o) (pair? (cdr o)))
                         (cadr o)
                         (flexvector-length fv))
                     1)))
    (cond
      ((>= left right) (if #f #f))
      (else
        (flexvector-swap! fv left right)
        (lp (+ left 1) (- right 1))))))

(define (flexvector-append fv . fvs)
  (assume (flexvector? fv))
  (apply flexvector-append! (flexvector-copy fv) fvs))

(define (flexvector-concatenate ls)
  (apply flexvector-append ls))

(define (flexvector-append-subvectors . o)
  (let lp ((ls o) (vecs '()))
    (if (null? ls)
        (flexvector-concatenate (reverse vecs))
        (lp (cdr (cddr ls))
            (cons (flexvector-copy (car ls) (cadr ls) (car (cddr ls))) vecs)))))

(define (flexvector-empty? fv)
  (assume (flexvector? fv))
  (zero? (flexvector-length fv)))

(define (flexvector-count pred? fv1 . o)
  (assume (procedure? pred?))
  (assume (flexvector? fv1))
  (apply flexvector-fold
         (lambda (count . x) (+ count (if (apply pred? x) 1 0)))
         0
         fv1 o))

(define (flexvector-cumulate f knil fv)
  (assume (procedure? f))
  (assume (flexvector? fv))
  (let* ((len (flexvector-length fv))
         (res (make-vector len)))
    (let lp ((i 0) (acc knil))
      (if (>= i len)
          (vector->flexvector res)
          (let ((acc (f acc (flexvector-ref fv i))))
            (vector-set! res i acc)
            (lp (+ i 1) acc))))))

(define (flexvector-partition pred? fv)
  (assume (procedure? pred?))
  (assume (flexvector? fv))
  (let ((left (flexvector)) (right (flexvector)))
    (flexvector-for-each
      (lambda (x) (flexvector-add-back! (if (pred? x) left right) x))
      fv)
    (values left right)))

(define (flexvector->list fv)
  (assume (flexvector? fv))
  (flexvector-fold-right (lambda (x y) (cons y x)) '() fv))

(define (reverse-flexvector->list fv . o)
  (assume (flexvector? fv))
  (flexvector->list (apply flexvector-reverse-copy fv o)))

(define (reverse-list->flexvector ls)
  (assume (list? ls))
  (let ((fv (list->flexvector ls)))
    (flexvector-reverse! fv)
    fv))

(define (string->flexvector s . o)
  (assume (string? s))
  (vector->flexvector (apply string->vector s o)))

(define (flexvector->string fv . o)
  (assume (flexvector? fv))
  (vector->string (apply flexvector->vector fv o)))

(define (generator->flexvector g)
  (assume (procedure? g))
  (flexvector-unfold eof-object? (lambda (x) x) (lambda (_) (g)) (g)))

(define (flexvector->generator fv)
  (assume (flexvector? fv))
  (let ((i 0))
    (lambda ()
      (if (< i (flexvector-length fv))
        (let ((element (flexvector-ref fv i)))
          (set! i (+ i 1))
          element)
        (eof-object)))))
