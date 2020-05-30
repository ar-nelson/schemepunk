(define-library (schemepunk datalog)
  (export make-var var? var->symbol wildcard-var
          with-new-datalog-db with-new-datalog-facts
          with-extended-datalog-db with-extended-datalog-facts
          declare-datalog-rule declare-datalog-fact retract-datalog-fact
          define-datalog let-datalog query-datalog ?-)

  (cond-expand
    ((or gambit chicken)
      (export %definition %head %head+var %body %resume-body %destruct
              %atom %atom/query %atom/not %atom/? %atom/= %atom+var %atom+constant
              %vector-reverse! %!=?))
    (else))

  (import (scheme base)
          (scheme case-lambda)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk vector)
          (schemepunk comparator)
          (schemepunk set)
          (schemepunk mapping)
          (schemepunk multimap))

  (begin
    (define predicate-comparator
      (make-pair-comparator symbol-comparator number-comparator))

    (define identity-comparator
      (make-comparator (λ _ #t) eq? identity<? identity-hash))

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

    (define (var<? x y)
      (and (not (eq? x y))
        (if (symbol=? (var->symbol x) (var->symbol y))
          (identity<? x y)
          (<? symbol-comparator (var->symbol x) (var->symbol y)))))

    (define var-comparator
      (make-comparator var? eq? var<? identity-hash))

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
    (define db-var-map-comparator
      (make-parameter (make-mapping-comparator (db-comparator))))

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
    (define db-fact-dependents
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
                         (db-fact-dependents (make-fact-dependency-map))
                         (db-rule-dependencies
                           (multimap predicate-comparator predicate-comparator))
                         (db-rule-negative-dependencies
                           (multimap predicate-comparator predicate-comparator)))
            (thunk)))
        ((comparator thunk)
           (parameterize ((db-comparator comparator)
                          (db-tuple-comparator (tuple-comparator comparator))
                          (db-var-map-comparator (make-mapping-comparator comparator)))
             (with-new-datalog-db thunk)))))

    (define with-new-datalog-facts
      (case-lambda
        ((thunk)
          (parameterize ((db-new-facts (make-fact-map))
                         (db-facts (make-fact-map))
                         (db-derived-facts (make-fact-map))
                         (db-fact-dependencies (make-fact-dependency-map))
                         (db-fact-dependents (make-fact-dependency-map)))
            (thunk)))
        ((comparator thunk)
           (parameterize ((db-comparator comparator)
                          (db-tuple-comparator (tuple-comparator comparator))
                          (db-var-map-comparator (make-mapping-comparator comparator)))
             (with-new-datalog-facts thunk)))))

    (define (with-extended-datalog-db thunk)
      (parameterize
        ((db-new-rules (list (car (db-new-rules))))
         (db-stratified-rules (list (car (db-stratified-rules))))
         (db-new-facts (multimap-copy (db-new-facts)))
         (db-facts (multimap-copy (db-facts)))
         (db-derived-facts (multimap-copy (db-derived-facts)))
         (db-fact-dependencies (multimap-copy (db-fact-dependencies)))
         (db-fact-dependents (multimap-copy (db-fact-dependents)))
         (db-rule-dependencies (multimap-copy (db-rule-dependencies)))
         (db-rule-negative-dependencies (multimap-copy (db-rule-negative-dependencies))))
        (thunk)))

    (define (with-extended-datalog-facts thunk)
      (parameterize ((db-new-facts (multimap-copy (db-new-facts)))
                     (db-facts (multimap-copy (db-facts)))
                     (db-derived-facts (multimap-copy (db-derived-facts)))
                     (db-fact-dependencies (multimap-copy (db-fact-dependencies)))
                     (db-fact-dependents (multimap-copy (db-fact-dependents))))
        (thunk)))

    (define-syntax let-datalog
      (syntax-rules ()
        ((_ (definitions ...) . body)
          (with-extended-datalog-db (λ ()
            (define-datalog definitions ...)
            . body)))))

    (define (%!=? x y)
      (not (=? (db-comparator) x y)))

    (define (match-vars params tuple vars)
      (vector-fold
        (λ(vars p x)
          (cond
            ((not vars) #f)
            ((eq? wildcard-var p) vars)
            ((var? p) (mapping-ref vars p
                        (λ () (mapping-set vars p x))
                        (λ y (and (eqv? x y) vars))))
            ((eqv? p x) vars)
            (else #f)))
        vars
        params
        tuple))

    (define (var-subst params vars)
      (vector-map (λ p (if (var? p) (mapping-ref vars p) p)) params))

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
               (map (cute match-vars params <> (mapping var-comparator)))
               (filter (λ x x))
               (map mapping->alist)
               (map (cute map
                          (if var-names
                            (λ((k . v)) (cons (cdr (assq k var-names)) v))
                            (λ x x))
                          <>))))
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
      (define predicate (car (rule-head rule)))
      (define params (cdr (rule-head rule)))
      (define existing (multimap-ref (db-derived-facts) predicate))
      (define fact-cmp (multimap-value-comparator (db-fact-dependencies)))
      (define var-map-deps (multimap (db-var-map-comparator) fact-cmp))
      (->>
        (vector-fold
          (λ(var-maps (apred . aparams))
            (define tuples (multimap-ref (db-derived-facts) apred))
            (define new-var-maps (set (db-var-map-comparator)))
            (set-for-each
              (λ vars
                (set-for-each
                  (λ tuple
                    (and-let* ((matched (match-vars aparams tuple vars)))
                      (set-adjoin! new-var-maps matched)
                      (when (multimap-contains? (db-facts) apred tuple)
                        (multimap-adjoin! var-map-deps matched (cons apred tuple))
                        (multimap-adjoin-set! var-map-deps
                                              matched
                                              (multimap-ref (db-fact-dependencies)
                                                            (cons apred tuple))))))
                  tuples))
              var-maps)
            new-var-maps)
          (set (db-var-map-comparator) (mapping var-comparator))
          (rule-body rule))
        (set-for-each
          (λ vars
            (and
              (vector-every
                (λ((fn . args)) (apply fn (vector->list (var-subst args vars))))
                (rule-guards rule))
              (vector-every
                (λ((apred . aparams))
                  (not (multimap-contains? (db-derived-facts)
                                           apred
                                           (var-subst aparams vars))))
                (rule-negatives rule))
              (let1 tuple (var-subst params vars)
                (unless (set-contains? existing tuple)
                  (let1 deps (multimap-ref var-map-deps vars)
                    (multimap-adjoin-set! (db-fact-dependencies) (cons predicate tuple) deps)
                    (set-for-each
                      (cute multimap-adjoin! (db-fact-dependents) <> (cons predicate tuple))
                      deps))
                  (multimap-adjoin! out predicate tuple))))))))

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
                (error "Datalog stratification failed" rules)))))))

    (define %vector-reverse! vector-reverse!)

    (define-syntax define-datalog
      (syntax-rules ()
        ((_ definition ...)
          (begin (%definition . definition) ...))))

    (define-syntax %definition
      (syntax-rules (:-)
        ((_ :- (predicate . params) . body)
          (%head predicate params () () body))
        ((_ predicate . params)
          (declare-datalog-fact 'predicate (vector . params)))))

    (define-syntax ?-
      (syntax-rules ()
        ((_ (predicate . params))
          (match (%atom/query params () () predicate)
            (() #f)
            (result-set result-set)))))

    (define-syntax %head
      (syntax-rules ()
        ((_ predicate () params vars body)
          (%body body (cons 'predicate (vector . params)) vars () () ()))
        ((_ predicate (param . rest) (params ...) vars body)
          (syntax-symbol-case param
            (underscore (syntax-error "Datalog rule head cannot contain wildcard variable _"))
            (identifier (%head+var param predicate rest (params ... param) () vars body))
            (else (%head predicate rest (params ... param) vars body))))))

    (define-syntax %head+var
      (syntax-rules ()
        ((_ var predicate i o vars () body)
          (%head predicate i o
            ((var (syntax-error "Datalog variable occurs only in head:" var)) . vars)
            body))
        ((_ var predicate i o (before ...) ((other-var val) . after) body)
          (let ()
            (define-syntax same-var?
              (syntax-rules (var)
                ((_ var) (%head predicate i o (before ... (var val) . after) body))
                ((_ _) (%head+var var predicate i o (before ... (other-var val)) after body))))
            (same-var? other-var)))))

    (define-syntax %body
      (syntax-rules (? not = !=)
        ((_ () head vars deps neg-deps ())
          (let vars
            (declare-datalog-rule head (vector . deps) (vector . neg-deps) #())))
        ((_ () head vars deps neg-deps guards)
          (let vars
            (define g (vector . guards))
            (%vector-reverse! g)
            (declare-datalog-rule head (vector . deps) (vector . neg-deps) g)))
        ((_ ((? predicate . params) . rest) head vars deps neg-deps guards)
          (%atom/? params () vars guards predicate head rest deps neg-deps))
        ((_ ((not (predicate . params)) . rest) head vars deps neg-deps guards)
          (%atom/not params () vars guards predicate head rest deps neg-deps))
        ((_ ((= lhs rhs) . rest) head vars deps neg-deps guards)
          (syntax-symbol-case lhs
            (underscore (%body rest head vars deps neg-deps guards))
            (identifier
              (%atom+var lhs
                         (syntax-error "Datalog variable occurs only in = clause:" lhs)
                         %atom/=
                         lhs
                         rhs
                         ()
                         vars
                         guards
                         head
                         rest
                         deps
                         neg-deps))
            (else (syntax-error "First argument of datalog = must be a variable" lhs))))
        ((_ ((!= lhs rhs) . rest) head vars deps neg-deps guards)
          (%atom/? (lhs rhs) () vars guards %!=? head rest deps neg-deps))
        ((_ ((predicate . params) . rest) head vars deps neg-deps guards)
          (%atom params () vars guards predicate head rest deps neg-deps))))

    (define-syntax %atom
      (syntax-rules (quote)
        ((_ () params vars guards predicate head body deps neg-deps)
          (%body body head vars ((cons 'predicate (vector . params)) . deps) neg-deps guards))
        ((_ ('param . rest) (params ...) . state)
          (%atom rest (params ... 'param) . state))
        ((_ ((hd . tl) . rest) (params ...) vars guards . state)
          (%atom+var list-match
                     (make-var 'list-match)
                     %destruct
                     ((list-match (car list-match) hd) (list-match (cdr list-match) tl))
                     (%atom rest (params ... list-match))
                     vars
                     ()
                     ((cons pair? (vector list-match)) . guards)
                     . state))
        ((_ (param . rest) (params ...) . state)
          (syntax-symbol-case param
            (underscore (%atom rest (params ... wildcard-var) . state))
            (identifier
              (%atom+var param (make-var 'param) %atom rest (params ... param) () . state))
            (else (%atom rest () (params ... param) . state))))))

    (define-syntax %atom/query
      (syntax-rules (quote)
        ((_ () params ((var val) ...) predicate)
          (let ((var val) ...)
            (query-datalog 'predicate (vector . params) `((,var . var) ...))))
        ((_ ('param . rest) (params ...) . state)
          (%atom/query rest (params ... 'param) . state))
        ((_ (param . rest) (params ...) . state)
          (syntax-symbol-case param
            (underscore (%atom/query rest (params ... wildcard-var) . state))
            (identifier
              (%atom+var param (make-var 'param) %atom/query rest (params ... param) () . state))
            (else (%atom/query rest () (params ... param) . state))))))

    (define-syntax %atom/not
      (syntax-rules (quote)
        ((_ () params vars guards predicate head body deps neg-deps)
          (%body body head vars deps ((cons 'predicate (vector . params)) . neg-deps) guards))
        ((_ ('param . rest) (params ...) . state)
          (%atom/not rest (params ... 'param) . state))
        ((_ (param . rest) (params ...) . state)
          (syntax-symbol-case param
            (underscore (%atom/not rest (params ... wildcard-var) . state))
            (identifier
              (%atom+var param
                         (syntax-error "Datalog variable occurs only in negative:" param)
                         %atom/not
                         rest
                         (params ... param)
                         ()
                         . state))
            (else (%atom/not rest (params ... param) . state))))))

    (define-syntax %atom/?
      (syntax-rules (quote)
        ((_ () params vars guards predicate head body deps neg-deps)
          (%body body head vars deps neg-deps ((cons predicate (vector . params)) . guards)))
        ((_ ('param . rest) (params ...) . state)
          (%atom/? rest (params ... 'param) . state))
        ((_ (param . rest) (params ...) . state)
          (syntax-symbol-case param
            (underscore (syntax-error "Datalog ? clause cannot contain wildcard variable _"))
            (identifier
              (%atom+var param
                         (syntax-error "Datalog variable occurs only in ? clause:" param)
                         %atom/?
                         rest
                         (params ... param)
                         ()
                         . state))
            (else (%atom/? rest (params ... param) . state))))))

    (define-syntax %resume-body
      (syntax-rules ()
        ((_ vars guards head body deps neg-deps)
          (%body body head vars deps neg-deps guards))))

    (define-syntax %atom/=
      (syntax-rules (quote)
        ((_ lhs 'rhs . state)
          (%atom+constant lhs 'rhs () . state))
        ((_ lhs (hd . tl) vars guards . state)
          (%destruct ((lhs (car lhs) hd) (lhs (cdr lhs) tl))
                     (%resume-body)
                     vars
                     ((cons pair? (vector lhs)) . guards)
                     . state))
        ((_ lhs rhs . state)
          (syntax-symbol-case lhs
            (underscore (%resume-body . state))
            (identifier (syntax-error "Datalog variable equality is not supported" (= lhs rhs)))
            (else (%atom+constant lhs 'rhs () . state))))))

    (define-syntax %atom+var
      (syntax-rules ()
        ((_ var val continue i o vars () . state)
          (continue i o ((var val) . vars) . state))
        ((_ var val continue i o (before ...) (binding . after) . state)
          (let ()
            (define-syntax same-var?
              (syntax-rules (var syntax-error)
                ((_ (var (syntax-error . _)))
                  (continue i o (before ... (var val) . after) . state))
                ((_ (var val2))
                  (continue i o (before ... (var val2) . after) . state))
                ((_ _)
                  (%atom+var var val continue i o (before ... binding) after . state))))
            (same-var? binding)))))

    (define-syntax %atom+constant
      (syntax-rules ()
        ((_ var val vars () . state)
          (%resume-body ((var val) . vars) . state))
        ((_ var val (before ...) (binding . after) . state)
          (let ()
            (define-syntax same-var?
              (syntax-rules (var quote)
                ((_ (var '_))
                  (syntax-error "Datalog variable is = to multiple constants" var))
                ((_ (var _))
                  (%resume-body (before ...  (var val) . after) . state))
                ((_ _)
                  (%atom+constant var val (before ... binding) after . state))))
            (same-var? binding)))))

    (define-syntax %destruct
      (syntax-rules (quote)
        ((_ () (continue before-vars ...) . state)
          (continue before-vars ... . state))
        ((_ ((x subject ()) . rest) params vars guards . state)
          (%destruct rest
                     params
                     vars
                     ((cons (lambda (x) (null? subject)) (vector x)) . guards)
                     . state))
        ((_ ((x subject 'param) . rest) params vars guards . state)
          (%destruct rest
                     params
                     vars
                     ((cons (lambda (x) (equal? 'param subject)) (vector x)) . guards)
                     . state))
        ((_ ((x subject (hd . tl)) . rest) params vars guards . state)
          (%destruct ((x (car subject) hd) (x (cdr subject) tl) . rest)
                     params
                     vars
                     ((cons (lambda (x) (pair? subject)) (vector x)) . guards)
                     . state))
        ((_ ((x subject param) . rest) params vars guards . state)
          (syntax-symbol-case param
            (underscore (%destruct rest params vars guards . state))
            (identifier
              (%atom+var param
                         (syntax-error "Datalog variable only occurs in destructuring:" param)
                         %destruct
                         rest
                         params
                         ()
                         vars
                         ((cons (lambda (x y) (equal? subject y)) (vector x param)) . guards)
                         . state))
            (else
              (%destruct rest
                         params
                         vars
                         ((cons (lambda (x) (equal? subject param)) (vector x)) . guards)
                         . state))))))))
