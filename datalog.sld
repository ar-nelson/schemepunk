(define-library (schemepunk datalog)
  (export datalog-db
          datalog-db?
          make-datalog-db
          extend-datalog-db
          var?
          var->symbol
          var->string
          predicate?
          predicate-name
          predicate-arity
          predicate-comparator
          var-comparator
          tuple-comparator
          get-predicate
          predicate->string
          atom->string
          rule->string
          ?-
          let-datalog)

  (import (scheme base)
          (scheme char)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk vector)
          (schemepunk comparator)
          (schemepunk set)
          (schemepunk hash-table)
          (schemepunk multimap))

  (begin
    (define-record-type Datalog-Database
      (make-db predicates strata facts derived-facts deps neg-deps)
      datalog-db?
      (predicates db-predicates)
      (strata db-stratified-rules)
      (facts db-facts)
      (derived-facts db-derived-facts)
      (deps db-dependencies)
      (neg-deps db-negative-dependencies))

    (define (make-datalog-db)
      (make-db
        (make-hash-table (make-default-comparator))
        '()
        (make-fact-store)
        (make-fact-store)
        (make-multimap predicate-comparator predicate-comparator)
        (make-multimap predicate-comparator predicate-comparator)))

    (define-record-type Var
      (make-var name)
      var?
      (name var->symbol))

    (define-record-type Predicate
      (make-predicate name arity)
      predicate?
      (name predicate-name)
      (arity predicate-arity))

    (define-record-type Rule
      (make-rule head body negatives guards)
      rule?
      (head rule-head)
      (body rule-body)
      (negatives rule-negatives)
      (guards rule-guards))

    (define (symbol<? x y)
      (string<? (symbol->string x) (symbol->string y)))

    (define var-comparator
      (make-comparator var? eq?
        (λ(x y) (symbol<? (var->symbol x) (var->symbol y)))
        (hash-lambda (x) (symbol-hash (var->symbol x)))))

    (define predicate-comparator
      (make-comparator predicate? eq?
        (λ(x y) (or (< (predicate-arity x) (predicate-arity y))
                    (and (= (predicate-arity x) (predicate-arity y))
                         (symbol<? (predicate-name x) (predicate-name y)))))
        (hash-lambda (x) (+ (symbol-hash (predicate-name x)) (predicate-arity x)))))

    (define value-comparator
      (let1 default (make-default-comparator)
        (make-comparator
          (λ _ #t)
          eqv?
          (comparator-ordering-predicate default)
          (comparator-hash-function default))))

    (define tuple-comparator
      (make-vector-comparator value-comparator vector? vector-length vector-ref))

    (define (make-fact-store)
      (make-multimap predicate-comparator tuple-comparator))

    (define (fact-store-query store predicate params)
      (->> (multimap-ref store predicate)
           set->list
           (map (λ t (match-vars params t '())))
           (filter (λ x x))))

    (define var->string (λ-> var->symbol symbol->string))

    (define (predicate->string p)
      (string-append
        (symbol->string (predicate-name p))
        "/"
        (number->string predicate-arity p)))

    (define (atom->string a)
      (define (term->string t)
        (cond
          ((var? t) (var->string t))
          ((symbol? t) (symbol->string t))
          ((number? t) (number->string t))
          (else t)))
      (string-append
        (symbol->string (predicate-name (car a)))
        "("
        (string-join ", " (map term->string (vector->list (cdr a))))
        ")"))

    (define (rule->string r)
      (string-append
        (atom->string (rule-head r))
        " :- "
        (string-join ", "
          (append
            (map atom->string (rule-body r))
            (map (λ->> atom->string (string-append "¬")) (rule-negatives r))))))

    (define (match-vars params tuple vars)
      (vector-fold
        (λ(vars p x)
          (and vars
               (if (var? p)
                   (match (assq p vars)
                     ((_ . y) (and (eq? x y) vars))
                     (else (cons (cons p x) vars)))
                   (and (eq? p x) vars))))
        vars
        params tuple))

    (define (var-subst params vars)
      (vector-map (λ p (if (var? p) (cdr (assq p vars)) p)) params))

    (define (tuple=? x y) (vector= eq? x y))

    (define (semi-naive-update rules facts recent-facts)
      (define new-facts (make-fact-store))
      (for-each
        (λ rule
          (when (can-update? rule recent-facts)
            (generate-facts-from-rule rule facts new-facts)))
        rules)
      (if (zero? (multimap-key-count new-facts))
          facts
          (begin (multimap-union! facts new-facts)
                 (semi-naive-update rules facts new-facts))))

    (define (can-update? rule facts)
      (->> (rule-body rule)
           (map car)
           (any (λ->> (multimap-ref facts) set-empty? not))))

    (define (generate-facts-from-rule rule facts out)
      (define (var=? a b) (and (eq? (car a) (car b)) (eq? (cdr a) (cdr b))))
      (define predicate (car (rule-head rule)))
      (define params (cdr (rule-head rule)))
      (define existing (multimap-ref facts predicate))
      (->> (rule-body rule)
           (fold
             (λ(atom var-sets)
               (-> (append-map
                     (λ vars
                       (append-map
                         (λ tuple
                           (match (match-vars (cdr atom) tuple vars)
                             ((? not) '())
                             (vars+ (list vars+))))
                         (set->list (multimap-ref facts (car atom)))))
                     var-sets)
                   (delete-duplicates! (λ(x y) (lset= var=? x y)))))
             '(()))
           (filter
             (λ vars
               (every
                 (λ g (apply (car g) (vector->list (var-subst (cdr g) vars))))
                 (rule-guards rule))))
           (remove
             (λ vars
               (any
                 (λ c (multimap-contains? facts (car c) (var-subst (cdr c) vars)))
                 (rule-negatives rule))))
           (map (λ->> (var-subst params)))
           (for-each (λ tuple
             (unless (set-contains? existing tuple)
               (multimap-add! out predicate tuple))))))

    (define (extend-datalog-db db new-rules new-facts)
      (let-values (((facts) (multimap-copy (db-facts db)))
                   ((deps neg-deps) (add-deps (db-dependencies db)
                                              (db-negative-dependencies db)
                                              new-rules)))
        (for-each
          (λ-> (match ((predicate . tuple)
                 (multimap-add! facts predicate tuple))))
          new-facts)
        (let* ((strata (->> (db-stratified-rules db)
                            (fold append new-rules)
                            (stratify deps neg-deps)))
               (derived-facts
                 (fold
                   (λ(rules initial)
                     (let1 derived (multimap-copy initial)
                       (semi-naive-update rules derived initial)
                       derived))
                   facts
                   strata)))
          (make-db (db-predicates db) strata facts derived-facts deps neg-deps))))

    (define (add-deps original-deps original-neg-deps rules)
      (if (null? rules)
        (values original-deps original-neg-deps)
        (let ((deps (multimap-copy original-deps))
              (neg-deps (multimap-copy original-neg-deps)))

          ; Direct links
          (for-each
            (λ rule
               (let1 p (car (rule-head rule))
                 (for-each (λ->> car (multimap-add! deps p)) (rule-body rule))
                 (for-each
                   (λ x (multimap-add! deps p (car x))
                        (multimap-add! neg-deps p (car x)))
                   (rule-negatives rule))))
            rules)

          ; Transitive closure
          (for-each
            (λ a (set-for-each
                   (λ b (set-for-each
                          (λ c (unless (multimap-contains? deps a c)
                                 (multimap-add! deps a c)
                                 (when (or (multimap-contains? neg-deps a b)
                                           (multimap-contains? neg-deps b c))
                                   (multimap-add! neg-deps a c))))
                          (multimap-ref deps b)))
                   (multimap-ref deps a)))
            (multimap-keys deps))

          ; Detect cycles
          (as-> (multimap-keys neg-deps) xs
            (filter (λ x (multimap-contains? neg-deps x x)) xs)
            (map predicate->string xs)
            (unless (null? xs)
              (error "Cycle in Datalog negative dependency" xs)))

          (values deps neg-deps))))

    (define (stratify deps neg-deps rules)
      (if (null? rules) '()
        (let-values
          (((remaining stratum)
              (partition
                (λ->> rule-head
                      car
                      (multimap-ref neg-deps)
                      (set-any? (λ dep (any (λ-> rule-head car (eq? dep)) rules))))
                rules)))
          (if (null? stratum)
            (if (null? remaining)
                '()
                (error "Datalog stratification failed" remaining))
            (cons stratum (stratify deps neg-deps remaining))))))

    (define (get-predicate db name arity)
      (let* ((predicates (db-predicates db))
             (predicate-id (cons name arity)))
        (hash-table-ref predicates predicate-id
          (λ() (let1 predicate (make-predicate name arity)
                 (hash-table-set! predicates predicate-id predicate)
                 predicate)))))

    (define (not-eq? x y)
      (not (eq? x y)))

    (define datalog-db (make-parameter (make-datalog-db)))

    (define-syntax build-atom
      (syntax-rules ()
        ((build-atom pred params ...)
           (cons (get-predicate (datalog-db) 'pred (length '(params ...)))
                 (vector params ...)))))

    (define-syntax ?-
      (syntax-rules ()
        ((?- (query ...))
           (let-values (((pred params) (car+cdr (build-atom query ...))))
             (fact-store-query (db-derived-facts (datalog-db)) pred params)))))

    (define-syntax build-rule
      (syntax-rules (is not ¬ != ≠)
        ((build-rule () head body negatives guards)
           (make-rule head (list . body) (list . negatives) (list . guards)))
        ((build-rule ((is pred? params ...) . rest) head body negatives guards)
           (build-rule rest head body negatives ((cons pred? (vector params ...)) . guards)))
        ((build-rule ((not pred params ...) . rest) head body negatives guards)
           (build-rule rest head body ((build-atom pred params ...) . negatives) guards))
        ((build-rule ((¬ pred params ...) . rest) head body negatives guards)
           (build-rule rest head body ((build-atom pred params ...) . negatives) guards))
        ((build-rule ((!= x y) . rest) head body negatives guards)
           (build-rule rest head body negatives ((cons not-eq? (vector x y)) . guards)))
        ((build-rule ((≠ x y) . rest) head body negatives guards)
           (build-rule rest head body negatives ((cons not-eq? (vector x y)) . guards)))
        ((build-rule ((pred params ...) . rest) head body negatives guards)
           (build-rule rest head ((build-atom pred params ...) . body) negatives guards))))

    (define-syntax build-defs
      (syntax-rules (:-)
        ((build-defs () rules facts)
           (values (list . rules) (list . facts)))
        ((build-defs ((:- head . body) . rest) rules facts)
           (build-defs rest
                       ((build-rule body (build-atom . head) () () ()) . rules)
                       facts))
        ((build-defs ((predicate params ...) . rest) rules facts)
           (build-defs rest
                       rules
                       ((build-atom predicate params ...) . facts)))))

    (define-syntax let-datalog
      (syntax-rules ()
        ((let-datalog (var ...) (clause ...) body ...)
           (let ((var (make-var 'var)) ...)
             (let-values (((rules facts) (build-defs (clause ...) () ())))
               (parameterize
                 ((datalog-db (extend-datalog-db (datalog-db) rules facts)))
                 body ...))))))))
