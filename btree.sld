(define-library (schemepunk btree)
  (export btree btree?
          alist->btree btree->alist
          btree-key-comparator
          btree-empty? btree-copy
          btree-ref btree-set btree-set!
          btree-fold btree-fold-right
          btree-map/monotone btree-map/monotone!
          btree-delete btree-delete! btree-pop btree-pop!
          btree-subset? btree=? btree<? btree-hash
          make-btree-comparator btree-comparator)

  (import (scheme base)
          (schemepunk syntax)
          (schemepunk function)
          (schemepunk list)
          (schemepunk comparator))

  (cond-expand
    ((and chicken debug) (import (only (srfi 99) define-record-type)))
    (else))

  (begin
    (define-record-type Btree
      (make-btree key-comparator max-size root)
      btree?
      (key-comparator btree-key-comparator set-btree-key-comparator!)
      (max-size btree-max-size)
      (root btree-root set-btree-root!))

    (define-record-type Node
      (make-node elements children size)
      node?
      (elements node-elements)
      (children node-children)
      (size node-size set-node-size!))

    (define (btree comparator max-size)
      (make-btree comparator max-size
        (make-node (make-vector max-size)
                   #f
                   0)))

    (define (vector-insert! vec i elem)
      (cond-expand
        (chicken
          ; Chicken's r7rs vector-copy! is broken
          ; when src and dest overlap and src index < dest index.
          (let1 temp (vector-copy vec)
            (vector-copy! vec (+ i 1) temp i (- (vector-length vec) 1))))
        (else
          (vector-copy! vec (+ i 1) vec i (- (vector-length vec) 1))))
      (vector-set! vec i elem))

    (define (vector-remove! vec i)
      (vector-copy! vec i vec (+ i 1))
      (vector-set! vec (- (vector-length vec) 1) #f))

    (define (find>= vec key <? size)
      (let loop ((i 0))
        (if (or (= i size) (isnt (car (vector-ref vec i)) <? key))
          i
          (loop (+ i 1)))))

    (define (insert-element! elements pair <? size)
      (define i (find>= elements (car pair) <? size))
      (if (= i size)
        (vector-set! elements i pair)
        (vector-insert! elements i pair))
      i)

    (define-syntax node-let
      (syntax-rules ()
        ((_ node elements children size . body)
           (let ((elements (node-elements node))
                 (children (node-children node))
                 (size (node-size node)))
             . body))))

    (define (node-copy node)
      (make-node (vector-copy (node-elements node))
                 (and (node-children node) (vector-copy (node-children node)))
                 (node-size node)))

    (define (node-split node inserted-left inserted inserted-right <? max-size)
      (let* ((elements (node-elements node))
             (children (node-children node))
             (half (quotient (+ max-size 1) 2))
             (overflow (make-vector (+ max-size 1)))
             (_ (vector-copy! overflow 0 elements 0))
             (i (insert-element! overflow inserted <? max-size))
             (left-elements (make-vector max-size))
             (right-elements (make-vector max-size))
             (left-children (and children (make-vector (+ max-size 1))))
             (right-children (and children (make-vector (+ max-size 1))))
             (left (make-node left-elements left-children half))
             (right (make-node right-elements right-children (- max-size half)))
             (median (vector-ref overflow half)))
        (vector-copy! left-elements 0 overflow 0 half)
        (vector-copy! right-elements 0 overflow (+ half 1))
        (when children
          (let1 overflow-children (make-vector (+ max-size 2))
            (vector-copy! overflow-children 0 children 0)
            (vector-set! overflow-children i inserted-left)
            (vector-insert! overflow-children (+ i 1) inserted-right)
            (vector-copy! left-children 0 overflow-children 0 (+ half 1))
            (vector-copy! right-children 0 overflow-children (+ half 1))))
        (list left median right)))

    (define (node-insert node pair =? <? max-size)
      (node-let node elements children size
        (define key (car pair))
        (define i (find>= elements key <? size))
        (cond
          ((and (is i < size) (is key =? (car (vector-ref elements i))))
            (let1 new-elements (vector-copy elements)
              (vector-set! new-elements i pair)
              (make-node new-elements (node-children node) size)))
          ((not children)
            (if (= size max-size)
              (node-split node #f pair #f <? max-size)
              (let1 new-elements (vector-copy elements)
                (insert-element! new-elements pair <? size)
                (make-node new-elements #f (+ size 1)))))
          (else
            (match (node-insert (vector-ref children i) pair =? <? max-size)
              ((left median right)
                (if (= size max-size)
                  (node-split node left median right <? max-size)
                  (let* ((new-elements (vector-copy elements))
                         (new-children (vector-copy children))
                         (j (insert-element! new-elements median <? size)))
                    (vector-set! new-children j left)
                    (vector-insert! new-children (+ j 1) right)
                    (make-node new-elements new-children (+ size 1)))))
              (child
                (let1 new-children (vector-copy children)
                  (vector-set! new-children i child)
                  (make-node elements new-children size))))))))

    (define (node-insert! node pair =? <? max-size)
      (node-let node elements children size
        (define key (car pair))
        (define i (find>= elements key <? size))
        (cond
          ((and (is i < size) (is key =? (car (vector-ref elements i))))
            (vector-set! elements i pair)
            #f)
          ((not children)
            (if (= size max-size)
              (node-split node #f pair #f <? max-size)
              (begin (insert-element! elements pair <? size)
                     (set-node-size! node (+ size 1))
                     #f)))
          (else
            (match (node-insert! (vector-ref children i) pair =? <? max-size)
              ((left median right)
                (if (= size max-size)
                  (node-split node left median right <? max-size)
                  (let1 j (insert-element! elements median <? size)
                    (vector-set! children j left)
                    (vector-insert! children (+ j 1) right)
                    (set-node-size! node (+ size 1))
                    #f)))
              (else #f))))))

    (define (btree-set btree key value)
      (define comparator (btree-key-comparator btree))
      (define max-size (btree-max-size btree))
      (define result
        (node-insert (btree-root btree)
                     (cons key value)
                     (comparator-equality-predicate comparator)
                     (comparator-ordering-predicate comparator)
                     max-size))
      (make-btree comparator max-size
        (match result
          ((left median right)
             (let ((elements (make-vector max-size))
                   (children (make-vector (+ max-size 1))))
               (vector-set! elements 0 median)
               (vector-set! children 0 left)
               (vector-set! children 1 right)
               (make-node elements children 1)))
          (new-root new-root))))

    (define (btree-set! btree key value)
      (define comparator (btree-key-comparator btree))
      (define max-size (btree-max-size btree))
      (define result
        (node-insert! (btree-root btree)
                      (cons key value)
                      (comparator-equality-predicate comparator)
                      (comparator-ordering-predicate comparator)
                      max-size))
      (match result
        ((left median right)
           (let ((elements (make-vector max-size))
                 (children (make-vector (+ max-size 1))))
             (vector-set! elements 0 median)
             (vector-set! children 0 left)
             (vector-set! children 1 right)
             (set-btree-root! btree (make-node elements children 1))))
        (else #f))
      btree)

    (define (node-get node key failure =? <?)
      (node-let node elements children size
        (define i (find>= elements key <? size))
        (define pair (and (is i < size) (vector-ref elements i)))
        (cond
          ((and pair (is (car pair) =? key)) (cdr pair))
          ((not children) (failure))
          (else (node-get (vector-ref children i) key failure =? <?)))))

    (define+ (btree-ref btree key
                        :optional
                        (failure (λ() (error "btree: key not found" key))))
      (define comparator (btree-key-comparator btree))
      (node-get (btree-root btree)
                key
                failure
                (comparator-equality-predicate comparator)
                (comparator-ordering-predicate comparator)))

    (define (node-delete node key =? <? min-size)
      (node-let node elements children size
        (define i (find>= elements key <? size))
        (define pair (and (< i size) (vector-ref elements i)))
        (cond
          ((not children)
            (if (and pair (is (car pair) =? key))
              (let1 new-elements (vector-copy elements)
                (vector-remove! new-elements i)
                (values pair (make-node new-elements #f (- size 1))))
              (values #f node)))
          ((and pair (is (car pair) =? key))
            (values pair
              (let1-values (separator right) (chain (vector-ref children (+ i 1))
                                                    (node-pop-smallest _ min-size))
                (node-balance node (+ i 1) separator right min-size))))
          (else
            (let1-values (deleted new-child)
                         (node-delete (vector-ref children i) key =? <? min-size)
              (values deleted (if deleted
                                (node-balance node i #f new-child min-size)
                                node)))))))

    (define (node-delete! node key =? <? min-size)
      (node-let node elements children size
        (define i (find>= elements key <? size))
        (define pair (and (< i size) (vector-ref elements i)))
        (if (and pair (=? (car pair) key))
          (begin
            (if children
              (begin
                (vector-set! elements i
                  (node-pop-smallest! (vector-ref children (+ i 1)) min-size))
                (node-balance! node (+ i 1) min-size))
              (begin (vector-remove! elements i)
                     (set-node-size! node (- size 1))))
            pair)
          (and-let*
            ((children)
             (deleted (node-delete! (vector-ref children i) key =? <? min-size)))
            (node-balance! node i min-size)
            deleted))))

    (define (btree-delete btree key)
      (define comparator (btree-key-comparator btree))
      (define-values (deleted new-root)
        (node-delete (btree-root btree)
                     key
                     (comparator-equality-predicate comparator)
                     (comparator-ordering-predicate comparator)
                     (quotient (btree-max-size btree) 2)))
      (if deleted
        (make-btree comparator
                    (btree-max-size btree)
                    (if (and (zero? (node-size new-root))
                             (node-children new-root))
                      (vector-ref (node-children new-root) 0)
                      new-root))
        btree))

    (define (btree-delete! btree key)
      (define root (btree-root btree))
      (define comparator (btree-key-comparator btree))
      (node-delete! root
                    key
                    (comparator-equality-predicate comparator)
                    (comparator-ordering-predicate comparator)
                    (quotient (btree-max-size btree) 2))
      (when (and (zero? (node-size root)) (node-children root))
        (set-btree-root! btree (vector-ref (node-children root) 0)))
      btree)

    (define (node-pop-smallest node min-size)
      (node-let node elements children size
        (cond
          (children
            (let1-values (popped new-left) (chain (vector-ref children 0)
                                                  (node-pop-smallest _ min-size))
              (values popped (node-balance node 0 #f new-left min-size))))
          ((zero? size)
            (values #f node))
          (else
            (let1 new-elements (make-vector (vector-length elements))
              (vector-copy! new-elements 0 elements 1)
              (values (vector-ref elements 0)
                      (make-node new-elements #f (- size 1))))))))

    (define (node-pop-smallest! node min-size)
      (node-let node elements children size
        (cond
          (children
            (let1 popped (node-pop-smallest! (vector-ref children 0) min-size)
              (node-balance! node 0 min-size)
              popped))
          ((zero? size) #f)
          (else
            (let1 popped (vector-ref elements 0)
              (vector-remove! elements 0)
              (set-node-size! node (- size 1))
              popped)))))

    (define (btree-pop btree)
      (define-values (popped new-root)
        (node-pop-smallest (btree-root btree)
                           (quotient (btree-max-size btree) 2)))
      (values popped (make-btree (btree-key-comparator btree)
                                 (btree-max-size btree)
                                 (if (and (zero? (node-size new-root))
                                          (node-children new-root))
                                   (vector-ref (node-children new-root) 0)
                                   new-root))))

    (define (btree-pop! btree)
      (define root (btree-root btree))
      (define popped
        (node-pop-smallest! root (quotient (btree-max-size btree) 2)))
      (when (and (zero? (node-size root)) (node-children root))
        (set-btree-root! btree (vector-ref (node-children root) 0)))
      popped)

    (define (node-balance node i separator-before new-child min-size)
      (define new-parent (node-copy node))
      (node-let new-parent p-el p-ch p-sz
        (node-let new-child c-el c-ch c-sz
          (vector-set! p-ch i new-child)
          (when separator-before
            (vector-set! p-el (- i 1) separator-before))
          (when (is c-sz < min-size)
            (let ((left (and (is i > 0) (vector-ref p-ch (- i 1))))
                  (right (and (is i < p-sz) (vector-ref p-ch (+ i 1)))))
              (cond
                ((and right (is (node-size right) > min-size))
                  (vector-set! p-ch (+ i 1) (node-copy right))
                  (rotate-right! new-parent i))
                ((and left (is (node-size left) > min-size))
                  (vector-set! p-ch (- i 1) (node-copy left))
                  (rotate-left! new-parent i))
                (right
                  (merge-right! new-parent i))
                (else
                  (merge-left! new-parent i)))))
          new-parent)))

    (define (node-balance! node i min-size)
      (node-let node p-el p-ch p-sz
        (node-let (vector-ref p-ch i) c-el c-ch c-sz
          (when (is c-sz < min-size)
            (let ((left (and (is i > 0) (vector-ref p-ch (- i 1))))
                  (right (and (is i < p-sz) (vector-ref p-ch (+ i 1)))))
              (cond
                ((and right (is (node-size right) > min-size)) (rotate-right! node i))
                ((and left (is (node-size left) > min-size)) (rotate-left! node i))
                (right (merge-right! node i))
                (else (merge-left! node i))))))))

    (define (rotate-right! parent i)
      (node-let parent p-el p-ch _
        (let ((center (vector-ref p-ch i)) (right (vector-ref p-ch (+ i 1))))
          (node-let center c-el c-ch c-sz
            (node-let right r-el r-ch r-sz
              (vector-set! c-el c-sz (vector-ref p-el i))
              (vector-set! p-el i (vector-ref r-el 0))
              (vector-remove! r-el 0)
              (when c-ch
                (vector-set! c-ch (+ c-sz 1) (vector-ref r-ch 0))
                (vector-remove! r-ch 0))
              (set-node-size! center (+ c-sz 1))
              (set-node-size! right (- r-sz 1)))))))

    (define (rotate-left! parent i)
      (node-let parent p-el p-ch _
        (let ((center (vector-ref p-ch i)) (left (vector-ref p-ch (- i 1))))
          (node-let center c-el c-ch c-sz
            (node-let left l-el l-ch l-sz
              (vector-insert! c-el 0 (vector-ref p-el (- i 1)))
              (vector-set! p-el (- i 1) (vector-ref l-el (- l-sz 1)))
              (vector-set! l-el (- l-sz 1) #f)
              (when c-ch
                (vector-insert! c-ch 0 (vector-ref l-ch l-sz))
                (vector-set! l-ch l-sz #f))
              (set-node-size! center (+ c-sz 1))
              (set-node-size! left (- l-sz 1)))))))

    (define (merge-right! parent i)
      (node-let parent p-el p-ch p-sz
        (let ((center (vector-ref p-ch i)) (right (vector-ref p-ch (+ i 1))))
          (node-let center c-el c-ch c-sz
            (node-let right r-el r-ch r-sz
              (vector-set! c-el c-sz (vector-ref p-el i))
              (vector-copy! c-el (+ c-sz 1) r-el 0 r-sz)
              (when c-ch
                (vector-copy! c-ch (+ c-sz 1) r-ch 0 (+ r-sz 1)))
              (set-node-size! center (+ c-sz 1 r-sz))
              (vector-remove! p-el i)
              (vector-remove! p-ch (+ i 1))
              (set-node-size! parent (- p-sz 1)))))))

    (define (merge-left! parent i)
      (node-let parent p-el p-ch p-sz
        (let ((center (vector-ref p-ch i)) (left (vector-ref p-ch (- i 1))))
          (node-let center c-el c-ch c-sz
            (node-let left l-el l-ch l-sz
              (vector-copy! c-el (+ l-sz 1) c-el 0 c-sz)
              (vector-set! c-el l-sz (vector-ref p-el (- i 1)))
              (vector-copy! c-el 0 l-el 0 l-sz)
              (when c-ch
                (vector-copy! c-ch (+ l-sz 1) c-ch 0 (+ c-sz 1))
                (vector-copy! c-ch 0 l-ch 0 (+ l-sz 1)))
              (set-node-size! center (+ l-sz 1 c-sz))
              (vector-remove! p-el (- i 1))
              (vector-remove! p-ch (- i 1))
              (set-node-size! parent (- p-sz 1)))))))

    (define (node-fold kons knil node)
      (node-let node elements children size
        (if children
          (let loop ((i 0) (accum (node-fold kons knil (vector-ref children 0))))
            (if (= i size)
              accum
              (loop (+ i 1) (node-fold kons
                                       (kons (vector-ref elements i) accum)
                                       (vector-ref children (+ i 1))))))
          (let loop ((i 0) (accum knil))
            (if (= i size)
              accum
              (loop (+ i 1) (kons (vector-ref elements i) accum)))))))

    (define (node-fold-right kons knil node)
      (node-let node elements children size
        (if children
          (let loop ((i (- size 1))
                     (accum (node-fold-right kons knil (vector-ref children size))))
            (if (negative? i)
              accum
              (loop (- i 1) (node-fold-right kons
                                             (kons (vector-ref elements i) accum)
                                             (vector-ref children i)))))
          (let loop ((i (- size 1)) (accum knil))
            (if (negative? i)
              accum
              (loop (- i 1) (kons (vector-ref elements i) accum)))))))

    (define (btree-fold kons knil btree)
      (node-fold kons knil (btree-root btree)))

    (define (btree-fold-right kons knil btree)
      (node-fold-right kons knil (btree-root btree)))

    (define (node-map/monotone fn node)
      (let ((elements (vector-copy (node-elements node)))
            (children (chain-and (node-children node) (vector-copy _)))
            (size (node-size node)))
        (do ((i 0 (+ i 1))) ((>= i size))
          (when children
            (vector-set! children i (node-map/monotone fn (vector-ref children i))))
          (match (vector-ref elements i)
            ((k . v)
              (let1-values (k2 v2) (fn k v)
                (vector-set! elements i (cons k2 v2))))
            (else #f)))
        (when children
          (vector-set! children size (node-map/monotone fn (vector-ref children size))))
        (make-node elements children size)))

    (define (node-map/monotone! fn node)
      (node-let node elements children size
        (do ((i 0 (+ i 1))) ((>= i size))
          (when children
            (node-map/monotone! fn (vector-ref children i)))
          (match (vector-ref elements i)
            ((k . v)
              (let1-values (k2 v2) (fn k v)
                (vector-set! elements i (cons k2 v2))))
            (else #f)))
        (when children
          (node-map/monotone! fn (vector-ref children size)))))

    (define (btree-map/monotone fn key-comparator btree)
      (make-btree key-comparator
                  (btree-max-size btree)
                  (node-map/monotone fn (btree-root btree))))

    (define (btree-map/monotone! fn key-comparator btree)
      (set-btree-key-comparator! btree key-comparator)
      (node-map/monotone! fn (btree-root btree))
      btree)

    (define (btree-copy btree)
      (btree-map/monotone (λ(k v) (values k v))
                          (btree-key-comparator btree)
                          btree))

    (define btree-empty? (compose zero? node-size btree-root))

    (define (btree->alist btree)
      (btree-fold-right cons '() btree))

    (define (alist->btree alist comparator max-size)
      (fold
        (λ ((k . v) bt) (btree-set! bt k v))
        (btree comparator max-size)
        alist))

    (define (btree-subset? value-comparator x y)
      (call/cc
        (λ return
          (btree-fold (λ((k . v) _)
                        (let1 v2 (btree-ref y k (λ () (return #f)))
                          (or (=? value-comparator v v2) (return #f))))
                      #t
                      x))))

    (define (btree=? value-comparator x y)
      (and (eq? (btree-key-comparator x)
                (btree-key-comparator y))
           (btree-subset? value-comparator x y)
           (btree-subset? value-comparator y x)))

    (define (btree<? value-comparator x y)
      (define key-comparator (btree-key-comparator x))
      (assume (eq? (btree-key-comparator x)
                   (btree-key-comparator y))
              "btree<?: different key comparators")
      (let loop ((x x) (y y))
        (let*-values (((xe x2) (btree-pop x))
                      ((ye y2) (btree-pop y)))
          (cond
            ((not ye) #f)
            ((not xe) #t)
            ((<? key-comparator (car xe) (car ye)) #t)
            ((not (=? key-comparator (car xe) (car ye))) #f)
            ((<? value-comparator (cdr xe) (cdr ye)) #t)
            ((=? value-comparator (cdr xe) (cdr ye)) (loop x2 y2))
            (else #f)))))

    (define (btree-hash value-comparator btree)
      (define key-comparator (btree-key-comparator btree))
      (btree-fold (λ((k . v) h)
                    (chain (modulo (* h 33) (hash-bound))
                           (+ _ (comparator-hash key-comparator k))
                           (* _ 33)
                           (modulo _ (hash-bound))
                           (+ _ (comparator-hash value-comparator v))))
                  (hash-salt)
                  btree))

    (define (make-btree-comparator value-comparator)
      (make-comparator
        btree?
        (cut btree=? value-comparator <> <>)
        (cut btree<? value-comparator <> <>)
        (hash-lambda (x) (btree-hash value-comparator x))))

    (define btree-comparator (make-btree-comparator (make-default-comparator)))

    (comparator-register-default! btree-comparator)))
