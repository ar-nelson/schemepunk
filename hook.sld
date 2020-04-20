(define-library (schemepunk hook)
  (export make-hook hook?
          list->hook list->hook!
          hook-add! hook-delete! hook-reset!
          hook->list
          hook-run)

  (import (scheme base)
          (schemepunk syntax)
          (schemepunk list))

  (cond-expand
    ((and (not chicken) (library (srfi 173)))
      (import (srfi 173)))
    (else
      (begin
        (define-record-type Hook
          (list->hook arity handlers)
          hook?
          (arity hook-arity)
          (handlers hook->list list->hook!))

        (define (make-hook arity)
          (list->hook arity '()))

        (define (hook-add! hook proc)
          (list->hook! hook (cons proc (hook->list hook))))

        (define (hook-delete! hook proc)
          (list->hook! hook (filter (λ x (not (eq? x proc)))
                                    (hook->list hook))))

        (define (hook-reset! hook)
          (list->hook! hook '()))

        (define (hook-run hook . args)
          (assume (= (length args) (hook-arity hook)))
          (for-each (cut apply <> args) (hook->list hook)))))))
