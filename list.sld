(define-library (schemepunk list)
  (export ; (scheme cxr) procedures
          caaar caadr cadar caddr cdaar cdadr cddar cdddr
          caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
          cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr

          ; (scheme list) procedures
          xcons cons* make-list list-tabulate
          list-copy circular-list iota

          proper-list? circular-list? dotted-list?
          not-pair? null-list?
          list=

          first second third fourth fifth sixth seventh eighth ninth tenth
          car+cdr
          take       drop
          take-right drop-right
          take!      drop-right!
          split-at   split-at!
          last last-pair

          length+ concatenate
          append! concatenate! reverse!
          append-reverse append-reverse!
          zip unzip1 unzip2 unzip3 unzip4 unzip5
          count

          fold       unfold       pair-fold       reduce
          fold-right unfold-right pair-fold-right reduce-right
          append-map append-map!
          map! pair-for-each filter-map map-in-order

          filter  partition  remove
          filter! partition! remove!

          find find-tail
          any every
          list-index
          take-while drop-while take-while!
          span break span! break!

          delete  delete-duplicates
          delete! delete-duplicates!

          alist-cons alist-copy
          alist-delete alist-delete!

          lset<= lset= lset-adjoin
          lset-union             lset-union!
          lset-intersection      lset-intersection!
          lset-difference        lset-difference!
          lset-xor               lset-xor!
          lset-diff+intersection lset-diff+intersection!

          ; extra procedures
          snoc
          map-with-index
          intercalate
          list-gen
          fold-in-pairs fold-right-in-pairs
          topological-sort)

  (import (scheme base)
          (scheme cxr)
          (schemepunk syntax))

  (cond-expand
    (chicken (import (srfi 1)))
    ((library (scheme list)) (import (scheme list)))
    ((library (srfi 1)) (import (srfi 1)))
    ((library (std srfi 1)) (import (std srfi 1))))

  (begin
    (define (snoc init last) (append init (list last)))

    (define (map-with-index f xs)
      (map f xs (list-tabulate (length xs) (λ(x) x))))

    (define (intercalate delim xs)
      (match xs
        ((x y . zs) `(,x ,delim ,@(intercalate delim (cons y zs))))
        (else xs)))

    (define (list-gen fn)
      (define (list-gen-loop accum)
        (fn (λ(next) (list-gen-loop (cons next accum))) accum))
      (reverse (list-gen-loop '())))

    (define (fold-right-in-pairs fn seed xs)
      (match xs
        (() seed)
        ((x y . rest) (fn x y (fold-right-in-pairs fn seed rest)))))

    (define (fold-in-pairs fn seed xs)
      (match xs
        (() seed)
        ((x y . rest) (fold-in-pairs fn (fn x y seed) rest))))

    ;; Sorts a list of dependencies in dependency order.
    ;;
    ;; dependency-list is an alist, in which the car of each element is
    ;; a dependency, and the cdr of each element is a list of its dependencies,
    ;; each of which must be the car of another element. The list must contain
    ;; no dependency cycles.
    ;;
    ;; Raises '(dependency-missing _) if a cdr contains a dependency that is not
    ;; the car of any element.
    ;;
    ;; Raises '(dependency-cycle _) if there is a dependency cycle.
    ;;
    (define (topological-sort dependency-list)
      (%topological-sort dependency-list '()))

    (define (%topological-sort deps so-far)
      (define (no-deps entry)
        (every (λ dep (member dep so-far)) (cdr entry)))
      (define next (find no-deps deps))
      (cond (next (%topological-sort
                    (remove (λ x (equal? (car x) (car next))) deps)
                    (cons (car next) so-far)))
            ((null? deps) (reverse so-far))
            (else (for-each
                    (λ dep
                      (unless (assoc dep deps) (raise `(dependency-missing ,dep))))
                    (append-map cdr deps))
                  (raise `(dependency-cycle ,deps)))))))
