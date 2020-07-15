(define-library (schemepunk generator)
  (export generator circular-generator make-iota-generator make-range-generator
          make-coroutine-generator list->generator vector->generator
          reverse-vector->generator string->generator
          bytevector->generator
          make-for-each-generator make-unfold-generator)
  (export gcons* gappend gcombine gfilter gremove
          gtake gdrop gtake-while gdrop-while
          gflatten ggroup gmerge gmap gstate-filter
          gdelete gdelete-neighbor-dups gindex gselect gfork)
  (export generator->list generator->reverse-list
          generator->vector generator->vector!  generator->string
          generator-fold generator-map->list generator-for-each generator-find
          generator-count generator-any generator-every generator-unfold)
  (export make-accumulator count-accumulator list-accumulator
          reverse-list-accumulator vector-accumulator
          reverse-vector-accumulator vector-accumulator!
          string-accumulator bytevector-accumulator bytevector-accumulator!
          sum-accumulator product-accumulator)
  (export iterator? iterator-peek iterator-next!
          generator->iterator character-port->iterator iterator->generator)

  (import (scheme base))

  (cond-expand
    (kawa
      (import (scheme case-lambda)
              (schemepunk syntax)
              (only (schemepunk list) any)
              (only (kawa base) define-simple-class runnable try-catch this invoke-special)
              (class java.lang Runnable Thread InterruptedException)
              (class java.util.concurrent SynchronousQueue)
              (class gnu.mapping Procedure Procedure0))
      ; Kawa doesn't support call/cc, but we can still implement
      ; make-coroutine-generator with threads!
      (begin
        ; Unfinished coroutines can hang around and leak memory.
        ; The only reliable way to fix this is to add a finalize() method
        ; that kills threads when the generator is garbage collected.
        (define-simple-class CoroutineGenerator (Procedure0)
          (proc ::Procedure access: 'private)
          (thread ::Thread access: 'private)
          ((*init* (pr ::Procedure) (th ::Thread))
            (invoke-special Procedure0 (this) '*init*)
            (set! proc pr)
            (set! thread th))
          ((apply0) (proc))
          ((finalize) access: 'public
            (thread:interrupt)))

        (define (make-coroutine-generator proc)
          (define queue (SynchronousQueue))
          (define done #f)
          (define thread
            (chain (λ ()
                     (try-catch (begin (proc (cut queue:put <>))
                                       (let loop ()
                                         (queue:put (eof-object))
                                         (loop)))
                       (ex InterruptedException #f)))
                   (runnable)
                   (Thread)))
          (thread:setDaemon #t)
          (thread:start)
          (CoroutineGenerator
            (λ () (queue:take))
            thread)))
      (include "polyfills/srfi-158-impl.scm"))
    ((and (not chicken) (library (srfi 158)))
      (import (srfi 158)))
    ((and gerbil (library (std srfi 158)))
      (import (std srfi 158)))
    (else
      (import (scheme case-lambda)
              (only (schemepunk list) any))
      (include "polyfills/srfi-158-impl.scm")))

  (begin
    (define-record-type Iterator
      (make-iterator peek next)
      iterator?
      (peek iterator-peek-proc)
      (next iterator->generator))

    (define (iterator-peek iter)
      ((iterator-peek-proc iter)))

    (define (iterator-next! iter)
      ((iterator->generator iter)))

    (define (generator->iterator gen)
      (define next (gen))
      (make-iterator
        (lambda () next)
        (lambda () (let ((value next)) (set! next (gen)) value))))

    (define (character-port->iterator port)
      (make-iterator
        (lambda () (peek-char port))
        (lambda () (read-char port))))

    ;; Split one generator into two,
    ;; allowing multiple operations on the same generator.
    (define (gfork gen)
      (define left '())
      (define right '())
      (values
        (lambda ()
          (if (pair? left)
            (let ((next (car left)))
              (set! left (cdr left))
              next)
            (let ((next (gen)))
              (unless (eof-object? next)
                (set! right `(,@right ,next)))
              next)))
        (lambda ()
          (if (pair? right)
            (let ((next (car right)))
              (set! right (cdr right))
              next)
            (let ((next (gen)))
              (unless (eof-object? next)
                (set! left `(,@left ,next)))
              next)))))))
