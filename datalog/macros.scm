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
      (let-syntax
        ((sym? (syntax-rules ()
                 ((_ param) param)
                 ((_ _) (%head predicate rest (params ... param) vars body)))))
        (sym? (%head+var param predicate rest (params ... param) () vars body))))))

(define-syntax %head+var
  (syntax-rules ()
    ((_ var predicate in-params out-params vars () body)
      (%head predicate in-params out-params
        ((var (syntax-error "Datalog variable occurs only in head:" var)) . vars)
        body))
    ((_ var predicate in-params out-params (before ...) ((other-var val) . after) body)
      (let-syntax
        ((same-var?
           (syntax-rules (var)
             ((_ var) (%head predicate in-params out-params (before ... (var val) . after) body))
             ((_ _) (%head+var var predicate in-params out-params (before ... (other-var val)) after body)))))
        (same-var? other-var)))))

(define-syntax %body
  (syntax-rules (? not = !=)
    ((_ () head vars deps neg-deps ())
      (let vars
        (declare-datalog-rule head (vector . deps) (vector . neg-deps) #())))
    ((_ () head vars deps neg-deps guards)
      (let vars
        (define g (vector . guards))
        (vector-reverse! g)
        (declare-datalog-rule head (vector . deps) (vector . neg-deps) g)))
    ((_ ((? predicate . params) . rest) head vars deps neg-deps guards)
      (%atom/? params () vars guards predicate head rest deps neg-deps))
    ((_ ((not (predicate . params)) . rest) head vars deps neg-deps guards)
      (%atom/not params () vars guards predicate head rest deps neg-deps))
    ((_ ((= lhs rhs) . rest) head vars deps neg-deps guards)
      (let-syntax
        ((sym? (syntax-rules ()
                 ((_ lhs) lhs)
                 ((_ _) (syntax-error "First argument of datalog = must be a variable" lhs)))))
        (sym? (%atom+var lhs
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
                         neg-deps))))
    ((_ ((!= lhs rhs) . rest) head vars deps neg-deps guards)
      (%atom/? (lhs rhs) () vars guards !=? head rest deps neg-deps))
    ((_ ((predicate . params) . rest) head vars deps neg-deps guards)
      (%atom params () vars guards predicate head rest deps neg-deps))))

(define-syntax %atom
  (syntax-rules (quote _)
    ((_ () params vars guards predicate head body deps neg-deps)
      (%body body head vars ((cons 'predicate (vector . params)) . deps) neg-deps guards))
    ((_ (_ . rest) (params ...) . state)
      (%atom rest (params ... wildcard-var) . state))
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
      (let-syntax
        ((sym? (syntax-rules ()
                 ((_ param) param)
                 ((_ _) (%atom rest () (params ... param) . state)))))
        (sym? (%atom+var param
                         (make-var 'param)
                         %atom
                         rest
                         (params ... param)
                         ()
                         . state))))))

(define-syntax %atom/query
  (syntax-rules (quote _)
    ((_ () params ((var val) ...) predicate)
      (let ((var val) ...)
        (query-datalog 'predicate (vector . params) `((,var . var) ...))))
    ((_ (_ . rest) (params ...) . state)
      (%atom/query rest (params ... wildcard-var) . state))
    ((_ ('param . rest) (params ...) . state)
      (%atom/query rest (params ... 'param) . state))
    ((_ (param . rest) (params ...) . state)
      (let-syntax
        ((sym? (syntax-rules ()
                 ((_ param) param)
                 ((_ _) (%atom/query rest () (params ... param) . state)))))
        (sym? (%atom+var param
                         (make-var 'param)
                         %atom/query
                         rest
                         (params ... param)
                         ()
                         . state))))))

(define-syntax %atom/not
  (syntax-rules (quote _)
    ((_ () params vars guards predicate head body deps neg-deps)
      (%body body head vars deps ((cons 'predicate (vector . params)) . neg-deps) guards))
    ((_ (_ . rest) (params ...) . state)
      (%atom/not rest (params ... wildcard-var) . state))
    ((_ ('param . rest) (params ...) . state)
      (%atom/not rest (params ... 'param) . state))
    ((_ (param . rest) (params ...) . state)
      (let-syntax
        ((sym? (syntax-rules ()
                 ((_ param) param)
                 ((_ _) (%atom/not rest (params ... param) . state)))))
        (sym? (%atom+var param
                         (syntax-error "Datalog variable occurs only in negative:" param)
                         %atom/not
                         rest
                         (params ... param)
                         ()
                         . state))))))

(define-syntax %atom/?
  (syntax-rules (quote _)
    ((_ () params vars guards predicate head body deps neg-deps)
      (%body body head vars deps neg-deps ((cons predicate (vector . params)) . guards)))
    ((_ (_ . rest) . state)
      (syntax-error "Datalog ? clause cannot contain wildcard variable _"))
    ((_ ('param . rest) (params ...) . state)
      (%atom/? rest (params ... 'param) . state))
    ((_ (param . rest) (params ...) . state)
      (let-syntax
        ((sym? (syntax-rules ()
                 ((_ param) param)
                 ((_ _) (%atom/? rest (params ... param) . state)))))
        (sym? (%atom+var param
                         (syntax-error "Datalog variable occurs only in ? clause:" param)
                         %atom/?
                         rest
                         (params ... param)
                         ()
                         . state))))))

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
      (let-syntax
        ((sym? (syntax-rules ()
                 ((_ rhs) rhs)
                 ((_ _) (%atom+constant lhs 'rhs () . state)))))
        (sym? (syntax-error "Datalog variable equality is not supported" (= lhs rhs)))))))

(define-syntax %atom+var
  (syntax-rules ()
    ((_ var val continue i o vars () . state)
      (continue i o ((var val) . vars) . state))
    ((_ var val continue i o (before ...) (binding . after) . state)
      (let-syntax
        ((same-var?
           (syntax-rules (var syntax-error)
             ((_ (var (syntax-error . _)))
               (continue i o (before ... (var val) . after) . state))
             ((_ (var val2))
               (continue i o (before ... (var val2) . after) . state))
             ((_ _)
               (%atom+var var val continue i o (before ... binding) after . state)))))
        (same-var? binding)))))

(define-syntax %atom+constant
  (syntax-rules ()
    ((_ var val vars () . state)
      (%resume-body ((var val) . vars) . state))
    ((_ var val (before ...) (binding . after) . state)
      (let-syntax
        ((same-var?
           (syntax-rules (var quote)
             ((_ (var '_))
               (syntax-error "Datalog variable is = to multiple constants" var))
             ((_ (var _))
               (%resume-body (before ...  (var val) . after) . state))
             ((_ _)
               (%atom+constant var val (before ... binding) after . state)))))
        (same-var? binding)))))

(define-syntax %destruct
  (syntax-rules (quote _)
    ((_ () (continue before-vars ...) . state)
      (continue before-vars ... . state))
    ((_ ((x subject _) . rest) . state)
      (%destruct rest . state))
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
      (let-syntax
        ((sym? (syntax-rules ()
                 ((_ param) param)
                 ((_ _) (%destruct rest
                                   params
                                   vars
                                   ((cons (lambda (x) (equal? subject param)) (vector x)) . guards)
                                   . state)))))
        (sym? (%atom+var param
                         (syntax-error "Datalog variable only occurs in destructuring:" param)
                         %destruct
                         rest
                         params
                         ()
                         vars
                         ((cons (lambda (x y) (equal? subject y)) (vector x param)) . guards)
                         . state))))))
