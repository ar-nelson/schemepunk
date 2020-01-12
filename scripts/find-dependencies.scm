#!/usr/bin/env scheme-r7rs

(import (scheme base)
        (scheme file)
        (scheme read)
        (scheme write)
        (srfi 1))

(define-syntax λ (syntax-rules () ((λ x . xs) (lambda (x) . xs))))

(define filename-of cadr)
(define dependencies-of cddr)

(define (get-import clause)
  (and (list? clause)
       (pair? clause)
       (case (car clause)
         ((only except prefix rename) (get-import (cadr clause)))
         ((std scheme srfi rnrs r6rs chibi gauche gerbil kawa) #f)
         (else clause))))

(define (library-imports lib imports)
  (cond
    ((not (pair? lib)) imports)
    ((not (pair? (car lib))) (library-imports (cdr lib) imports))
    (else (library-imports (cdr lib)
      (case (caar lib)
        ((import) (append imports (map get-import (cdar lib))))
        ((cond-expand)
           (append imports
             (append-map (λ x (library-imports (cdr x) '()))
                         (cdar lib))))
        (else imports))))))

(define (read-name-and-dependencies deps)
  (define next (read))
  (cond
    ((eof-object? next) (values #f (filter (λ x x) deps)))
    ((pair? next)
       (case (car next)
         ((define-library)
            (values (cadr next)
                    (filter (λ x x)
                            (append deps (library-imports (cddr next) '())))))
         ((import)
            (read-name-and-dependencies
              (append deps (map get-import (cdr next)))))
         (else (read-name-and-dependencies deps))))
    (else (read-name-and-dependencies deps))))

(define (read-filenames-from-stdin accum)
  (define file (read-line))
  (if (eof-object? file)
      accum
      (read-filenames-from-stdin
        (guard (e (#t (display (string-append "Error parsing file " file))
                      (newline)
                      (display e)
                      (newline)
                      accum))
          (with-input-from-file file (lambda ()
            (let-values (((name deps) (read-name-and-dependencies '())))
              (if name `((,name ,file ,@deps) ,@accum) accum))))))))

(define (tree-shake roots deps)
  (define result roots)
  (for-each (λ root
    (for-each (λ dep
      (when (not (assoc dep result))
        (let ((next (assoc dep deps)))
          (when next (set! result (cons next result))))))
      root))
    roots)
  (if (equal? result roots) result (tree-shake result deps)))

(define (topological-sort deps so-far)
  (define (no-deps entry)
    (every (λ dep (assoc dep so-far)) (dependencies-of entry)))
  (define next (find no-deps deps))
  (cond (next (topological-sort
                (remove (λ x (equal? (car x) (car next))) deps)
                (cons next so-far)))
        ((null? deps) (reverse so-far))
        (else (for-each
                (λ dep
                  (unless (assoc dep deps) (raise `(dependency-missing ,dep))))
                (append-map dependencies-of deps))
              (raise `(dependency-cycle ,deps)))))

(define (main argv)
  (define initial-deps
    (fold (lambda (filename deps)
            (let-values
              (((_ new-deps)
                  (with-input-from-file filename
                    (lambda () (read-name-and-dependencies '())))))
              (lset-union equal? deps new-deps)))
          '()
          (cdr argv)))
  (define all-deps (read-filenames-from-stdin '()))
  (define deps
    (tree-shake
      (map (λ x (or (assoc x all-deps) (raise `(dependency-missing ,x))))
           initial-deps)
      all-deps))
  (for-each
    (λ lib (display (filename-of lib)) (newline))
    (topological-sort deps '()))
  0)
