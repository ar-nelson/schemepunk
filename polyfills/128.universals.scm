;; Some definitions extracted from 128.body1.scm,
;; because they're useful in all Schemes, even those that implement SRFI 128.
;;
;; make-vector-comparator, in particular, is broken in so many Schemes,
;; it makes sense to just always use a vendored version of it.
;; It's unusably buggy in Gauche, Gerbil, and Sagittarius!
;; I'm convinced no one ever tests these SRFI libraies...

(define (make-hasher)
  (let ((result (hash-salt)))
    (case-lambda
     (() result)
     ((n) (set! result (+ (modulo (* result 33) (hash-bound)) n))
          result))))

(define (make-pair-comparator car-comparator cdr-comparator)
   (make-comparator
     (make-pair-type-test car-comparator cdr-comparator)
     (make-pair=? car-comparator cdr-comparator)
     (make-pair<? car-comparator cdr-comparator)
     (make-pair-hash car-comparator cdr-comparator)))

(define (make-pair-type-test car-comparator cdr-comparator)
  (lambda (obj)
    (and (pair? obj)
         (comparator-test-type car-comparator (car obj))
         (comparator-test-type cdr-comparator (cdr obj)))))

(define (make-pair=? car-comparator cdr-comparator)
   (lambda (a b)
     (and ((comparator-equality-predicate car-comparator) (car a) (car b))
          ((comparator-equality-predicate cdr-comparator) (cdr a) (cdr b)))))

(define (make-pair<? car-comparator cdr-comparator)
   (lambda (a b)
      (if (=? car-comparator (car a) (car b))
        (<? cdr-comparator (cdr a) (cdr b))
        (<? car-comparator (car a) (car b)))))

(define (make-pair-hash car-comparator cdr-comparator)
   (lambda (obj)
     (let ((acc (make-hasher)))
       (acc (comparator-hash car-comparator (car obj)))
       (acc (comparator-hash cdr-comparator (cdr obj)))
       (acc))))

(define (make-list-comparator element-comparator type-test empty? head tail)
   (make-comparator
     (make-list-type-test element-comparator type-test empty? head tail)
     (make-list=? element-comparator type-test empty? head tail)
     (make-list<? element-comparator type-test empty? head tail)
     (make-list-hash element-comparator type-test empty? head tail)))

(define (make-list-type-test element-comparator type-test empty? head tail)
  (lambda (obj)
    (and
      (type-test obj)
      (let ((elem-type-test (comparator-type-test-predicate element-comparator)))
        (let loop ((obj obj))
          (cond
            ((empty? obj) #t)
            ((not (elem-type-test (head obj))) #f)
            (else (loop (tail obj)))))))))

(define (make-list=? element-comparator type-test empty? head tail)
  (lambda (a b)
    (let ((elem=? (comparator-equality-predicate element-comparator)))
      (let loop ((a a) (b b))
        (cond
          ((and (empty? a) (empty? b) #t))
          ((empty? a) #f)
          ((empty? b) #f)
          ((elem=? (head a) (head b)) (loop (tail a) (tail b)))
          (else #f))))))

(define (make-list<? element-comparator type-test empty? head tail)
  (lambda (a b)
    (let ((elem=? (comparator-equality-predicate element-comparator))
          (elem<? (comparator-ordering-predicate element-comparator)))
      (let loop ((a a) (b b))
        (cond
          ((and (empty? a) (empty? b) #f))
          ((empty? a) #t)
          ((empty? b) #f)
          ((elem=? (head a) (head b)) (loop (tail a) (tail b)))
          ((elem<? (head a) (head b)) #t)
          (else #f))))))

(define (make-list-hash element-comparator type-test empty? head tail)
  (lambda (obj)
    (let ((elem-hash (comparator-hash-function element-comparator))
          (acc (make-hasher)))
      (let loop ((obj obj))
        (cond
          ((empty? obj) (acc))
          (else (acc (elem-hash (head obj))) (loop (tail obj))))))))

(define (make-vector-comparator element-comparator type-test length ref)
  (make-comparator
    (make-vector-type-test element-comparator type-test length ref)
    (make-vector=? element-comparator type-test length ref)
    (make-vector<? element-comparator type-test length ref)
    (make-vector-hash element-comparator type-test length ref)))

(define (make-vector-type-test element-comparator type-test length ref)
  (lambda (obj)
    (and
      (type-test obj)
      (let ((elem-type-test (comparator-type-test-predicate element-comparator))
            (len (length obj)))
        (let loop ((n 0))
          (cond
            ((= n len) #t)
            ((not (elem-type-test (ref obj n))) #f)
            (else (loop (+ n 1)))))))))

(define (make-vector=? element-comparator type-test length ref)
   (lambda (a b)
     (and
       (= (length a) (length b))
       (let ((elem=? (comparator-equality-predicate element-comparator))
             (len (length b)))
         (let loop ((n 0))
           (cond
             ((= n len) #t)
             ((elem=? (ref a n) (ref b n)) (loop (+ n 1)))
             (else #f)))))))

(define (make-vector<? element-comparator type-test length ref)
   (lambda (a b)
     (cond
       ((< (length a) (length b)) #t)
       ((> (length a) (length b)) #f)
        (else
         (let ((elem=? (comparator-equality-predicate element-comparator))
             (elem<? (comparator-ordering-predicate element-comparator))
             (len (length a)))
         (let loop ((n 0))
           (cond
             ((= n len) #f)
             ((elem=? (ref a n) (ref b n)) (loop (+ n 1)))
             ((elem<? (ref a n) (ref b n)) #t)
             (else #f))))))))

(define (make-vector-hash element-comparator type-test length ref)
  (lambda (obj)
    (let ((elem-hash (comparator-hash-function element-comparator))
          (acc (make-hasher))
          (len (length obj)))
      (let loop ((n 0))
        (cond
          ((= n len) (acc))
          (else (acc (elem-hash (ref obj n))) (loop (+ n 1))))))))
