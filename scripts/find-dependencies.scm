#!/usr/bin/env scheme-r7rs

(import (scheme base)
        (scheme file)
        (scheme read)
        (scheme write)
        (srfi 1))

(define filename-of cadr)
(define dependencies-of cddr)

(define (get-import clause)
  (and (list? clause)
       (pair? clause)
       (case (car clause)
         ((only except prefix rename) (get-import (cadr clause)))
         ((std scheme srfi rnrs r6rs chibi chicken gauche gerbil kawa class) '())
         (else
           (if (file-exists? (module->filename clause))
             (list clause)
             '())))))

(define (library-imports lib imports)
  (cond
    ((not (pair? lib)) imports)
    ((not (pair? (car lib))) (library-imports (cdr lib) imports))
    (else (library-imports (cdr lib)
      (case (caar lib)
        ((import)
          (append imports (append-map get-import (cdar lib))))
        ((cond-expand)
          (append imports
            (append-map (lambda (x) (library-imports (cdr x) '()))
                        (cdar lib))))
        (else imports))))))

(define (read-dependencies)
  (delete-duplicates!
    (let loop ((deps '()))
      (define next (read))
      (cond
        ((eof-object? next) deps)
        ((pair? next)
           (case (car next)
             ((define-library)
               (append deps (library-imports (cddr next) '())))
             ((import)
               (loop (append deps (append-map get-import (cdr next)))))
             (else (loop deps))))
        (else (loop deps))))))

(define (module->filename module)
  (string-append
    (fold
      (lambda (sym str)
        (string-append str "/" (symbol->string sym)))
      "."
      module)
    ".sld"))

(define (read-transitive-dependencies filename module deps-so-far)
  (let ((my-deps (with-input-from-file filename read-dependencies)))
    (fold
      (lambda (mod deps)
        (if (assoc mod deps-so-far)
          deps
          (read-transitive-dependencies (module->filename mod) mod deps)))
      (if module
        (cons (cons module my-deps) deps-so-far)
        deps-so-far)
      my-deps)))

(define (topological-sort deps so-far)
  (define (no-deps entry)
    (every (lambda (dep) (assoc dep so-far)) (cdr entry)))
  (define next (find no-deps deps))
  (cond (next (topological-sort
                (remove (lambda (x) (equal? (car x) (car next))) deps)
                (cons next so-far)))
        ((null? deps) (reverse so-far))
        (else (raise `(dependency-cycle ,deps)))))

(define (main argv)
  (define deps
    (fold
      (lambda (filename deps)
        (read-transitive-dependencies filename #f deps))
      '()
      (cdr argv)))
  (for-each
    (lambda (lib) (display (module->filename (car lib))) (display " "))
    (delete-duplicates! (topological-sort deps '())))
  0)
