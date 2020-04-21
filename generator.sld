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
              (only (schemepunk syntax) λ cut)
              (only (schemepunk list) any)
              (only (kawa base) future)
              (class java.util.concurrent SynchronousQueue))
      ; Kawa doesn't support call/cc, but we can still implement
      ; make-coroutine-generator with threads!
      (begin
        (define (make-coroutine-generator proc)
          (define queue (SynchronousQueue))
          (define done #f)
          (future (begin (proc (cut queue:put <>))
                         (queue:offer (eof-object))
                         (set! done #t)))
          (λ () (if done (eof-object) (queue:take)))))
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
