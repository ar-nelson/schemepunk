;; Partial implementation of R7RS Large sorting (SRFI 132).
;; Handles Schemes that have SRFI 95 but not 132 (mostly Kawa).
;; Does not include the (start, end) optional parameters of vector functions.
(define-library (schemepunk sort)
  (export list-sorted?       vector-sorted?
          list-sort          vector-sort
          list-stable-sort   vector-stable-sort
          list-sort!         vector-sort!
          list-stable-sort!  vector-stable-sort!
          list-merge
          list-merge!)

  (cond-expand
    ((and (not chicken) (or (library (scheme sort)) (library (srfi 132))))
      (cond-expand
        ((library (scheme sort)) (import (scheme sort)))
        ((library (srfi 132)) (import (srfi 132)))))
    (else
       (import (scheme base))
       (cond-expand
         (chicken (import (chicken sort)))
         (else (import (srfi 95))))
       (begin
         (define (list-sorted? < xs) (sorted? xs <))
         (define (list-sort < xs) (sort xs <))
         (define list-stable-sort list-sort)
         (define (list-sort! < xs) (sort! xs <))
         (define list-stable-sort! list-sort!)
         (define (list-merge < xs ys) (merge xs ys <))
         (define (list-merge! < xs ys) (merge! xs ys <))

         (define vector-sorted? list-sorted?)
         (define vector-sort list-sort)
         (define vector-stable-sort list-stable-sort)
         (define vector-sort! list-sort!)
         (define vector-stable-sort! list-stable-sort!)))))
