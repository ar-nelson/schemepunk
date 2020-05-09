(define-library (schemepunk datalog)
  (export make-var var? var->symbol wildcard-var
          with-new-datalog-db with-new-datalog-facts
          with-extended-datalog-db with-extended-datalog-facts
          declare-datalog-rule declare-datalog-fact retract-datalog-fact
          define-datalog let-datalog query-datalog ?-)

  (import (scheme base)
          (scheme case-lambda)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk vector)
          (schemepunk comparator)
          (schemepunk set)
          (schemepunk multimap))

  (cond-expand
    (gerbil
      (import (only (gerbil core) syntax underscore? identifier?))
      (include "datalog/macros-gerbil.scm"))
    (else
      (include "datalog/macros.scm")))

  (begin
    (define predicate-comparator
      (make-pair-comparator symbol-comparator number-comparator))

    (define identity-comparator
      (make-comparator
        (λ _ #t)
        eq?
        identity<?
        identity-hash))

    (define default-value-comparator
      (make-sum-comparator symbol-comparator
                           number-comparator
                           string-comparator
                           identity-comparator))

    (define (tuple-comparator element-comparator)
      (make-vector-comparator element-comparator vector? vector-length vector-ref))

    (define (fact-comparator element-comparator)
      (make-pair-comparator predicate-comparator (tuple-comparator element-comparator)))

    (define-record-type Var
      (make-var name)
      var?
      (name var->symbol))

    (define wildcard-var (make-var '_))

    (define var->string (λ-> var->symbol symbol->string))

    (define-record-type Rule
      (make-rule head body negatives guards)
      rule?
      (head rule-head)
      (body rule-body)
      (negatives rule-negatives)
      (guards rule-guards))

    (define (atom->string a)
      (define (term->string t)
        (if (var? t) (var->string t) (format #f "~s" t)))
      (format #f "~a(~a)"
        (caar a)
        (->> (cdr a) vector->list (map term->string) (string-join ", "))))

    (define (rule->string r)
      (format #f "~a :- ~a"
        (atom->string (rule-head r))
        (string-join ", "
          (vector->list
            (vector-append
              (vector-map atom->string (rule-body r))
              (vector-map (λ->> atom->string (string-append "¬")) (rule-negatives r)))))))

    (define db-comparator
      (make-parameter default-value-comparator))
    (define db-tuple-comparator
      (make-parameter (tuple-comparator (db-comparator))))

    (define (make-fact-map)
      (multimap predicate-comparator (db-tuple-comparator)))

    (define (make-fact-dependency-map)
      (multimap (fact-comparator (db-comparator)) (fact-comparator (db-comparator))))

    (define db-new-rules
      (make-parameter (list '())))
    (define db-stratified-rules
      (make-parameter (list '(#()))))
    (define db-new-facts
      (make-parameter (make-fact-map)))
    (define db-facts
      (make-parameter (make-fact-map)))
    (define db-derived-facts
      (make-parameter (make-fact-map)))
    (define db-fact-dependencies
      (make-parameter (make-fact-dependency-map)))
    (define db-rule-dependencies
      (make-parameter (multimap predicate-comparator predicate-comparator)))
    (define db-rule-negative-dependencies
      (make-parameter (multimap predicate-comparator predicate-comparator)))

    (define (db-facts-stable?)
      (multimap-empty? (db-new-facts)))

    (define (db-rules-stable?)
      (null? (car (db-new-rules))))

    (define with-new-datalog-db
      (case-lambda
        ((thunk)
          (parameterize ((db-new-rules (list '()))
                         (db-stratified-rules (list '(#())))
                         (db-new-facts (make-fact-map))
                         (db-facts (make-fact-map))
                         (db-derived-facts (make-fact-map))
                         (db-fact-dependencies (make-fact-dependency-map))
                         (db-rule-dependencies
                           (multimap predicate-comparator predicate-comparator))
                         (db-rule-negative-dependencies
                           (multimap predicate-comparator predicate-comparator)))
            (thunk)))
        ((comparator thunk)
           (parameterize ((db-comparator comparator)
                          (db-tuple-comparator (tuple-comparator comparator)))
             (with-new-datalog-db thunk)))))

    (define with-new-datalog-facts
      (case-lambda
        ((thunk)
          (parameterize ((db-new-facts (make-fact-map))
                         (db-facts (make-fact-map))
                         (db-derived-facts (make-fact-map))
                         (db-fact-dependencies (make-fact-dependency-map)))
            (thunk)))
        ((comparator thunk)
           (parameterize ((db-comparator comparator)
                          (db-tuple-comparator (tuple-comparator comparator)))
             (with-new-datalog-facts thunk)))))

    (define (with-extended-datalog-db thunk)
      (parameterize
        ((db-new-rules (list (car (db-new-rules))))
         (db-stratified-rules (list (car (db-stratified-rules))))
         (db-new-facts (multimap-copy (db-new-facts)))
         (db-facts (multimap-copy (db-facts)))
         (db-derived-facts (multimap-copy (db-derived-facts)))
         (db-fact-dependencies (multimap-copy (db-fact-dependencies)))
         (db-rule-dependencies (multimap-copy (db-rule-dependencies)))
         (db-rule-negative-dependencies (multimap-copy (db-rule-negative-dependencies))))
        (thunk)))

    (define (with-extended-datalog-facts thunk)
      (parameterize ((db-new-facts (multimap-copy (db-new-facts)))
                     (db-facts (multimap-copy (db-facts)))
                     (db-derived-facts (multimap-copy (db-derived-facts)))
                     (db-fact-dependencies (multimap-copy (db-fact-dependencies))))
        (thunk)))

    (define-syntax let-datalog
      (syntax-rules ()
        ((_ (definitions ...) . body)
          (with-extended-datalog-db (λ ()
            (define-datalog definitions ...)
            . body)))))

    (define (!=? x y)
      (not (=? (db-comparator) x y)))

    (define (match-vars params tuple vars)
      (vector-fold
        (λ(vars p x)
          (cond
            ((not vars) #f)
            ((eq? wildcard-var p) vars)
            ((var? p) (match (assq p vars)
                        ((_ . y) (and (eqv? x y) vars))
                        (else `((,p . ,x) ,@vars))))
            ((eqv? p x) vars)
            (else #f)))
        vars
        params
        tuple))

    (define (var-subst params vars)
      (vector-map (λ p (if (var? p) (cdr (assq p vars)) p)) params))

    (define (with-arity predicate params)
      (cons predicate (vector-length params)))

    (define (declare-datalog-rule head body negatives guards)
      (define rule
        (make-rule (cons (with-arity (car head) (cdr head)) (cdr head))
                   (vector-map (λ((pr . pa)) (cons (with-arity pr pa) pa)) body)
                   (vector-map (λ((pr . pa)) (cons (with-arity pr pa) pa)) negatives)
                   guards))
      (set-car! (db-new-rules) (cons rule (car (db-new-rules)))))

    (define (declare-datalog-fact predicate params)
      (multimap-adjoin! (db-new-facts) (with-arity predicate params) params))

    (define (retract-datalog-fact predicate params)
      (error "Retraction is not yet implemented"))

    (define query-datalog
      (case-lambda
        ((predicate params var-names)
          (unless (db-rules-stable?) (update-rules!))
          (unless (db-facts-stable?) (update-facts!))
          (->> (with-arity predicate params)
               (multimap-ref (db-derived-facts))
               set->list
               (map (cut match-vars params <> '()))
               (filter (λ x x))
               (map (cute map (if var-names
                                (λ((k . v)) (cons (cdr (assq k var-names)) v))
                                (λ x x)) <>))))
        ((predicate params)
          (query-datalog predicate params #f))))

    (define (update-rules!)
      (define rules
        (fold vector-append
              (list->vector (car (db-new-rules)))
              (car (db-stratified-rules))))
      (update-rule-dependencies! (db-rule-dependencies)
                                 (db-rule-negative-dependencies)
                                 rules)
      (set-car! (db-stratified-rules)
                (stratify (db-rule-dependencies)
                          (db-rule-negative-dependencies)
                          rules))
      (for-each
        (λ rule
          (vector-for-each
            (λ atom (->> (car atom)
                         (multimap-ref (db-derived-facts))
                         (multimap-adjoin-set! (db-new-facts) (car atom))))
            (vector-append (rule-body rule) (rule-negatives rule))))
        (car (db-new-rules)))
      (set-car! (db-new-rules) '()))

    (define (update-facts!)
      (define new-facts (db-new-facts))
      (multimap-union! (db-facts) new-facts)
      (multimap-union! (db-derived-facts) new-facts)
      (for-each
        (λ rules
          (->> (semi-naive-update! rules new-facts new-facts)
               (multimap-union! new-facts)))
        (car (db-stratified-rules)))
      (multimap-clear! new-facts))

    (define (semi-naive-update! rules recent-facts all-new-facts)
      (define new-facts (make-fact-map))
      (vector-for-each
        (λ rule
          (when (can-update? rule recent-facts)
            (generate-facts-from-rule! rule new-facts)))
        rules)
      (cond
        ((multimap-empty? new-facts) all-new-facts)
        (else
          (multimap-union! (db-derived-facts) new-facts)
          (->> (multimap-union all-new-facts new-facts)
               (semi-naive-update! rules new-facts)))))

    (define (can-update? rule facts)
      (->> (rule-body rule)
           (vector-map car)
           (vector-any (cut multimap-contains-key? facts <>))))

    (define (generate-facts-from-rule! rule out)
      (define (var=? a b) (and (eq? (car a) (car b)) (eq? (cdr a) (cdr b))))
      (define predicate (car (rule-head rule)))
      (define params (cdr (rule-head rule)))
      (define existing (multimap-ref (db-derived-facts) predicate))
      (->> (rule-body rule)
           (vector-fold
             (λ(var-sets (apred . aparams))
               (-> (append-map
                     (λ vars
                       (append-map
                         (λ tuple
                           (match (match-vars aparams tuple vars)
                             (#f '())
                             (vars+ (list vars+))))
                         (set->list (multimap-ref (db-derived-facts) apred))))
                     var-sets)
                   (delete-duplicates! (cut lset= var=? <> <>))))
             '(()))
           (filter
             (λ vars
               (vector-every
                 (λ((fn . args)) (apply fn (vector->list (var-subst args vars))))
                 (rule-guards rule))))
           (remove
             (λ vars
               (vector-any
                 (λ((apred . aparams))
                   (multimap-contains? (db-derived-facts) apred (var-subst aparams vars)))
                 (rule-negatives rule))))
           (map (λ->> (var-subst params)))
           (for-each (λ tuple
             (unless (set-contains? existing tuple)
               (multimap-adjoin! out predicate tuple))))))

    (define (update-rule-dependencies! deps neg-deps rules)
      ; Direct links
      (vector-for-each
        (λ rule
           (let1 p (car (rule-head rule))
             (vector-for-each (λ->> car (multimap-adjoin! deps p)) (rule-body rule))
             (vector-for-each
               (λ x (multimap-adjoin! deps p (car x))
                    (multimap-adjoin! neg-deps p (car x)))
               (rule-negatives rule))))
        rules)

      ; Transitive closure
      (for-each
        (λ a (set-for-each
               (λ b (set-for-each
                      (λ c (unless (multimap-contains? deps a c)
                             (multimap-adjoin! deps a c)
                             (when (or (multimap-contains? neg-deps a b)
                                       (multimap-contains? neg-deps b c))
                               (multimap-adjoin! neg-deps a c))))
                      (multimap-ref deps b)))
               (multimap-ref deps a)))
        (multimap-keys deps))

      ; Detect cycles
      (as-> (multimap-keys neg-deps) xs
        (filter (λ x (multimap-contains? neg-deps x x)) xs)
        (map (λ((name . arity)) (format #f "~a/~a" name arity)) xs)
        (unless (null? xs)
          (error "Cycle in Datalog negative dependency" xs))))

    (define (stratify deps neg-deps rules)
      (if (vector-empty? rules) '(#())
        (let loop ((rules rules))
          (if (vector-empty? rules) '()
            (let1-values (split stratum-start)
              (vector-partition
                (λ->> rule-head
                      car
                      (multimap-ref neg-deps)
                      (set-any? (λ dep (vector-any (λ-> rule-head car (equal? dep)) rules))))
                rules)
              (if (< stratum-start (vector-length rules))
                (cons (vector-copy split stratum-start)
                      (loop (vector-copy split 0 stratum-start)))
                (error "Datalog stratification failed" rules)))))))))
