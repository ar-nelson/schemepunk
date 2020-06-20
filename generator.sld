(define-library (schemepunk generator)
  (export generator circular-generator make-iota-generator make-range-generator
          make-coroutine-generator list->generator vector->generator
          reverse-vector->generator string->generator
          bytevector->generator
          make-for-each-generator make-unfold-generator)
  (export gcons* gappend gcombine gfilter gremove
          gtake gdrop gtake-while gdrop-while
          gflatten ggroup gmerge gmap gstate-filter
          gdelete gdelete-neighbor-dups gindex gselect)
  (export generator->list generator->reverse-list
          generator->vector generator->vector!  generator->string
          generator-fold generator-map->list generator-for-each generator-find
          generator-count generator-any generator-every generator-unfold)
  (export make-accumulator count-accumulator list-accumulator
          reverse-list-accumulator vector-accumulator
          reverse-vector-accumulator vector-accumulator!
          string-accumulator bytevector-accumulator bytevector-accumulator!
          sum-accumulator product-accumulator)

  (cond-expand
    (kawa
      (import (scheme base)
              (scheme case-lambda)
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
      (import (scheme base)
              (scheme case-lambda)
              (only (schemepunk list) any))
      (include "polyfills/srfi-158-impl.scm"))))
