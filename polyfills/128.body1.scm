;;; Copyright (C) John Cowan (2015). All Rights Reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 

;;;; Main part of the SRFI 114 reference implementation

;;; "There are two ways of constructing a software design: One way is to
;;; make it so simple that there are obviously no deficiencies, and the
;;; other way is to make it so complicated that there are no *obvious*
;;; deficiencies." --Tony Hoare

;;; Syntax (because syntax must be defined before it is used, contra Dr. Hardcase)

;; Arithmetic if
(define-syntax comparator-if<=>
  (syntax-rules ()
    ((if<=> a b less equal greater)
     (comparator-if<=> (make-default-comparator) a b less equal greater))
    ((comparator-if<=> comparator a b less equal greater)
     (cond
       ((=? comparator a b) equal)
       ((<? comparator a b) less)
       (else greater)))))

;; Upper bound of hash functions is 2^25-1
(define-syntax hash-bound
  (syntax-rules ()
    ((hash-bound) 33554432)))

(define %salt% (make-parameter 16064047))

(define-syntax hash-salt
   (syntax-rules ()
     ((hash-salt) (%salt%))))

(define-syntax with-hash-salt
  (syntax-rules ()
    ((with-hash-salt new-salt hash-func obj)
     (parameterize ((%salt% new-salt)) (hash-func obj)))))

;;; Definition of comparator records with accessors and basic comparator

(define-record-type comparator
  (make-raw-comparator type-test equality ordering hash ordering? hash?)
  comparator?
  (type-test comparator-type-test-predicate)
  (equality comparator-equality-predicate)
  (ordering comparator-ordering-predicate)
  (hash comparator-hash-function)
  (ordering? comparator-ordered?)
  (hash? comparator-hashable?))

;; Public constructor
(define (make-comparator type-test equality ordering hash)
  (make-raw-comparator
    (if (eq? type-test #t) (lambda (x) #t) type-test)
    (if (eq? equality #t) (lambda (x y) (eqv? (ordering x y) 0)) equality)
    (if ordering ordering (lambda (x y) (error "ordering not supported")))
    (if hash hash (lambda (x y) (error "hashing not supported")))
    (if ordering #t #f)
    (if hash #t #f)))

;;; Invokers

;; Invoke the test type
(define (comparator-test-type comparator obj)
  ((comparator-type-test-predicate comparator) obj))

;; Invoke the test type and throw an error if it fails
(define (comparator-check-type comparator obj)
  (if (comparator-test-type comparator obj)
    #t
    (error "comparator type check failed" comparator obj)))

;; Invoke the hash function
(define (comparator-hash comparator obj)
  ((comparator-hash-function comparator) obj))

;;; Comparison predicates

;; Binary versions for internal use

(define (binary=? comparator a b)
  ((comparator-equality-predicate comparator) a b))

(define (binary<? comparator a b)
  ((comparator-ordering-predicate comparator) a b))

(define (binary>? comparator a b)
  (binary<? comparator b a))

(define (binary<=? comparator a b)
  (not (binary>? comparator a b)))

(define (binary>=? comparator a b)
  (not (binary<? comparator a b)))

;; General versions for export

(define (=? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary=? comparator a b)
	 (if (null? objs) #t (loop b (car objs) (cdr objs))))))

(define (<? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary<? comparator a b)
	 (if (null? objs) #t (loop b (car objs) (cdr objs))))))

(define (>? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary>? comparator a b)
	 (if (null? objs) #t (loop b (car objs) (cdr objs))))))

(define (<=? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary<=? comparator a b)
	 (if (null? objs) #t (loop b (car objs) (cdr objs))))))

(define (>=? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary>=? comparator a b)
	 (if (null? objs) #t (loop b (car objs) (cdr objs))))))


;;; Simple ordering and hash functions

(define (boolean<? a b)
  ;; #f < #t but not otherwise
  (and (not a) b))


(define (boolean-hash obj)
  (if obj (%salt%) 0))

(define (char-hash obj)
  (modulo (* (%salt%) (char->integer obj)) (hash-bound)))

(define (char-ci-hash obj)
  (modulo (* (%salt%) (char->integer (char-foldcase obj))) (hash-bound)))

(define (number-hash obj)
  (cond
    ((nan? obj) (%salt%))
    ((and (infinite? obj) (positive? obj)) (* 2 (%salt%)))
    ((infinite? obj) (* (%salt%) 3))
    ((real? obj) (abs (exact (round obj))))
    (else (+ (number-hash (real-part obj)) (number-hash (imag-part obj))))))

;; Lexicographic ordering of complex numbers
(define (complex<? a b)
  (if (= (real-part a) (real-part b))
    (< (imag-part a) (imag-part b))
    (< (real-part a) (real-part b))))

(define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))

(define (symbol-hash obj)
  (string-hash (symbol->string obj)))

;;; Wrapped equality predicates
;;; These comparators don't have ordering functions.

(define (make-eq-comparator)
  (make-comparator #t eq? #f default-hash))

(define (make-eqv-comparator)
  (make-comparator #t eqv? #f default-hash))

(define (make-equal-comparator)
  (make-comparator #t equal? #f default-hash))

;;; Sequence ordering and hash functions

; All of these have been moved to 128.universals.scm
