(define-library (schemepunk show base)
  (export show each each-in-list
          displayed written written-shared written-simply
          escaped maybe-escaped
          numeric numeric/comma numeric/si numeric/fitted
          nl fl space-to tab-to nothing
          joined joined/prefix joined/suffix
          joined/last joined/dot joined/range
          padded padded/right padded/both
          trimmed trimmed/right trimmed/both
          trimmed/lazy fitted fitted/right fitted/both
          fn with with! forked call-with-output
          make-state-variable port row col width output writer
          string-width substring/width pad-char ellipsis
          radix precision decimal-sep decimal-align
          sign-rule comma-rule comma-sep word-separator?
          ambiguous-is-wide?

          span-generator->formatter call-with-output-generator)

  (cond-expand
    (chicken
      (import (except (scheme base) string-length substring)
              (only (utf8) string-length substring)))
    (else
      (import (scheme base))))

  (import (scheme case-lambda)
          (scheme char)
          (scheme write)
          (schemepunk syntax)
          (schemepunk function)
          (schemepunk list)
          (schemepunk generator)
          (schemepunk comparator)
          (schemepunk mapping)
          (schemepunk box)
          (schemepunk show span)
          (schemepunk show block)
          (schemepunk show block datum)
          (schemepunk show numeric)
          (schemepunk show terminal-width))

  (begin
    (define-record-type State-Variable
      (%make-state-variable name index immutable)
      state-variable?
      (name state-variable-name)
      (index state-variable-index)
      (immutable state-variable-immutable?))

    (define state-variable-comparator
      (make-comparator
        state-variable?
        eq?
        (λ(x y) (< (state-variable-index x) (state-variable-index y)))
        state-variable-index))

    (define next-state-variable-index 0)
    (define default-state-variables (mapping state-variable-comparator))

    (define+ (make-state-variable name default :optional (immutable #f))
      (define var (%make-state-variable name next-state-variable-index immutable))
      (set! next-state-variable-index (+ 1 next-state-variable-index))
      (mapping-set! default-state-variables var (box default))
      var)

    (define string-width-default
      (case-lambda
        ((str) (string-length str))
        ((str start) (- (string-length str) start))
        ((str start end) (- end start))))

    (define+ (substring-default str start :optional (end (string-length str)))
      (substring str start end))

    (define port (make-state-variable "port" (current-output-port)))
    (define row (make-state-variable "row" 0))
    (define col (make-state-variable "col" 0))
    (define width (make-state-variable "width" 80))
    (define string-width (make-state-variable "string-width" string-width-default))
    (define substring/width (make-state-variable "substring/width" substring-default))
    (define pad-char (make-state-variable "pad-char" #\space))
    (define ellipsis (make-state-variable "ellipsis" ""))
    (define radix (make-state-variable "radix" 10))
    (define precision (make-state-variable "precision" #f))
    (define decimal-sep (make-state-variable "decimal-sep" #\.))
    (define decimal-align (make-state-variable "decimal-align" 0))
    (define sign-rule (make-state-variable "sign-rule" #f))
    (define comma-rule (make-state-variable "comma-rule" #f))
    (define comma-sep (make-state-variable "comma-sep" #\,))
    (define word-separator? (make-state-variable "word-separator?" char-whitespace?))
    (define ambiguous-is-wide? (make-state-variable "ambiguous-is-wide?" #f))

    (define (copy-vars vars)
      (mapping-map/monotone
        (λ(k v) (values k (box (unbox v))))
        state-variable-comparator
        vars))

    (define (get-var vars var)
      (unbox (mapping-ref vars var)))

    (define state-variables (make-parameter default-state-variables))

    (define (track-position vars gen)
      (let ((string-width (get-var vars string-width))
            (row (mapping-ref vars row))
            (col (mapping-ref vars col)))
        (gmap
          (λ x
            (case (span-type x)
              ((newline)
                (update-box! row
                  (cut + <> (chain (span-text x)
                                   (string->list)
                                   (count (cut eqv? <> #\newline)))))
                (set-box! col 0))
              (else
                (update-box! col
                  (cut + <> (string-width (span-text x))))))
            x)
          gen)))

    (define-syntax span-generator->formatter
      (syntax-rules ()
        ((_ gen)
          (lambda (vars) (track-position vars gen)))))

    (define (output-default str)
      (λ vars
        (chain (cute read-char (open-input-string str))
               (char-generator->span-generator <> (get-var vars word-separator?))
               (span-generator->formatter)
               (<> vars))))

    (define output (make-state-variable "output" output-default))

    (define (%with state-variable value fmt)
      (λ vars (fmt (mapping-set vars state-variable (box value)))))

    (define (%with! state-variable value)
      (if (state-variable-immutable? state-variable)
        (error "state variable is immutable" (state-variable-name state-variable))
        (set-box! (mapping-ref (state-variables) state-variable) value)))

    (define-syntax %fn
      (syntax-rules ()
        ((_ vars ((id state-var) . rest) . body)
          (let1 id (get-var vars state-var)
            (%fn vars rest . body)))
        ((_ vars (id . rest) . body)
          (let1 id (get-var vars id)
            (%fn vars rest . body)))
        ((_ vars () exprs ... fmt)
          (begin exprs ... (fmt vars)))))

    (define (%show vars output-port fmt)
      (parameterize ((state-variables (mapping-set! vars port (box output-port))))
        (generator-for-each (cut write-span <> (get-var vars port))
                            (fmt vars))))

    (define-syntax with
      (syntax-rules ()
        ((_ () . fmts) (each . fmts))
        ((_ ((var val) . rest) . fmts) (%with var val (with rest . fmts)))))

    (define-syntax with!
      (syntax-rules ()
        ((_ (var val) ...) (begin (%with! var val) ...))))

    (define-syntax fn
      (syntax-rules ()
        ((_ (bindings ...) exprs ... fmt)
           (λ vars
             (parameterize ((state-variables vars))
               (%fn vars (bindings ...) exprs ... fmt))))))

    (define (show output-dest . fmts)
      (define vars
        (chain (copy-vars default-state-variables)
               (mapping-set! <> width (box (get-terminal-width)))))
      (define fmt (each-in-list fmts))
      (case output-dest
        ((#t) (%show vars (current-output-port) fmt))
        ((#f) (let1 str (open-output-string)
                (%show vars str fmt)
                (get-output-string str)))
        (else (%show vars output-dest fmt))))

    (define (each . fmts)
      (each-in-list fmts))

    (define (each-in-list list-of-fmts)
      (define (get-fmt fmt) (if (procedure? fmt) fmt (displayed fmt)))
      (match list-of-fmts
        (() nothing)
        ((fmt) (get-fmt fmt))
        (else
          (λ vars
            (let1 gen ((get-fmt (car list-of-fmts)) vars)
              (set! list-of-fmts (cdr list-of-fmts))
              (λ ()
                (let loop ((next (gen)))
                  (if (and (eof-object? next) (pair? list-of-fmts))
                    (begin (set! gen ((get-fmt (car list-of-fmts)) vars))
                           (set! list-of-fmts (cdr list-of-fmts))
                           (loop (gen)))
                    next))))))))

    (define+ (joined mapper xs :optional (sep ""))
      (chain (map mapper xs)
             (intercalate sep)
             (each-in-list)))

    (define+ (joined/prefix mapper xs :optional (sep ""))
      (each-in-list (append-map (λ x (list sep (mapper x))) xs)))

    (define+ (joined/suffix mapper xs :optional (sep ""))
      (each-in-list (append-map (λ x (list (mapper x) sep)) xs)))

    (define+ (joined/last mapper last-mapper xs :optional (sep ""))
      (if (null? xs) nothing
        (chain (last-mapper (last xs))
               (snoc (map mapper (drop-right xs 1)))
               (intercalate sep)
               (each-in-list))))

    (define+ (joined/dot mapper dot-mapper xs :optional (sep ""))
      (let loop ((out '()) (xs xs))
        (cond
          ((null? xs)
            (chain (reverse out)
                   (intercalate sep)
                   (each-in-list)))
          ((pair? xs)
            (loop (cons (mapper (car xs)) out) (cdr xs)))
          (else
            (chain (cons (dot-mapper xs) out)
                   (reverse)
                   (intercalate sep)
                   (each-in-list))))))

    (define+ (joined/range mapper start :optional (end #f) (sep ""))
      (define last (and (number? end) (- end 1)))
      (λ vars
        (chain (make-range-generator start end)
               (gmap (λ i (chain (if (= i last)
                                   (each (mapper i))
                                   (each (mapper i) sep))
                                 (<> vars))))
               (generator-fold (flip gappend) (generator)))))

    (define (displayed obj)
      (cond
        ((string? obj)
          (λ vars (((get-var vars output) obj) vars)))
        ((char? obj)
          (λ vars (((get-var vars output) (string obj)) vars)))
        (else
          (written obj))))

    (define (written obj)
      (span-generator->formatter
        (block->span-generator (datum->block obj))))

    (define writer (make-state-variable "writer" written))

    (define (written-shared obj)
      (span-generator->formatter
        (block->span-generator (datum->block/shared obj))))

    (define (written-simply obj)
      (span-generator->formatter
        (block->span-generator (datum->block/simple obj))))

    (define (nl vars)
      (update-box! (mapping-ref vars row) (cut + <> 1))
      (set-box! (mapping-ref vars col) 0)
      (generator (newline-span)))

    (define (fl vars)
      (if (zero? (get-var vars col))
        (generator)
        (nl vars)))

    (define (space-to column)
      (assume (integer? column))
      (assume (positive? column))
      (λ vars
        (let1 current-column (get-var vars col)
          (if (< current-column column)
            (begin
              (set-box! (mapping-ref vars col) column)
              (generator
                (whitespace-span
                  (make-string (- column current-column)
                               (get-var vars pad-char)))))
            (generator)))))

    (define+ (tab-to :optional (tab-width 8))
      (assume (integer? tab-width))
      (assume (positive? tab-width))
      (λ vars
        (let* ((current-column (get-var vars col))
               (offset (remainder current-column tab-width))
               (tab (- tab-width offset)))
          (if (zero? offset)
            (generator)
            (begin
              (set-box! (mapping-ref vars col) (+ current-column tab))
              (generator
                (whitespace-span
                  (make-string tab (get-var vars pad-char)))))))))

    (define (nothing _) (generator))

    (define (escape str quote-ch esc-ch renamer)
      (with-output-to-string (λ()
        (with-input-from-string str (λ()
          (let loop ((ch (read-char)))
            (cond
              ((eof-object? ch)
                #f)
              ((eqv? quote-ch ch)
                (if esc-ch (write-char esc-ch) (write-char ch))
                (write-char ch)
                (loop (read-char)))
              ((not esc-ch)
                (write-char ch)
                (loop (read-char)))
              ((eqv? esc-ch ch)
                (write-char esc-ch)
                (write-char ch)
                (loop (read-char)))
              ((renamer ch) => (λ renamed
                (write-char esc-ch)
                (write-char renamed)
                (loop (read-char))))
              (else
                (write-char ch)
                (loop (read-char))))))))))

    (define+ (escaped str
                      :optional
                      (quote-ch #\")
                      (esc-ch #\\)
                      (renamer (const #f)))
      (assume (string? str))
      (assume (char? quote-ch))
      (assume (or (not esc-ch) (char? esc-ch)))
      (assume (procedure? renamer))
      (displayed (escape str quote-ch esc-ch renamer)))

    (define+ (maybe-escaped str
                            pred?
                            :optional
                            (quote-ch #\")
                            (esc-ch #\\)
                            (renamer (const #f)))
      (assume (string? str))
      (assume (procedure? pred?))
      (call/cc (λ return
        (string-for-each
          (λ ch
            (when (or (eqv? quote-ch ch) (eqv? esc-ch ch) (pred? ch))
              (return (each quote-ch
                            (escaped str quote-ch esc-ch renamer)
                            quote-ch))))
          str)
        (displayed str))))

    (define+ (numeric num
                      :optional
                      (radix/arg #f)
                      (precision/arg #f)
                      (sign-rule/arg #f)
                      (comma-rule/arg #f)
                      (comma-sep/arg #f)
                      (decimal-sep/arg #f))
      (assume (number? num))
      (λ vars
        (chain (numeric->string
                 num
                 (or radix/arg (get-var vars radix))
                 (or precision/arg (get-var vars precision))
                 (or sign-rule/arg (get-var vars sign-rule))
                 (or comma-rule/arg (get-var vars comma-rule))
                 (or comma-sep/arg (get-var vars comma-sep))
                 (or decimal-sep/arg (get-var vars decimal-sep))
                 (get-var vars decimal-align))
               (displayed)
               (<> vars))))

    (define+ (numeric/comma num
                            :optional
                            (comma-rule 3)
                            (radix #f)
                            (precision #f)
                            (sign-rule #f))
      (numeric num radix precision sign-rule comma-rule))

    (define+ (numeric/si num :optional (base 1000) (separator ""))
      (assume (number? num))
      (assume (or (= base 1000) (= base 1024)))
      (assume (string? separator))
      (λ vars
        (chain (numeric->string/si
                 num
                 base
                 separator
                 (get-var vars radix)
                 (get-var vars precision)
                 (get-var vars sign-rule)
                 (get-var vars comma-rule)
                 (get-var vars comma-sep)
                 (get-var vars decimal-sep)
                 (get-var vars decimal-align))
               (displayed)
               (<> vars))))

    (define (numeric/fitted width num . args)
      (assume (number? num))
      (assume (integer? width))
      (assume (positive? width))
      (call-with-output (apply numeric num args) (λ str
        (if (> (string-length str) width)
          (fn (precision decimal-sep comma-sep)
            (let ((prec (if (and (pair? args) (pair? (cdr args)))
                          (cadr args)
                          precision)))
              (if (and prec (not (zero? prec)))
                  (let* ((dec-sep
                          (or decimal-sep
                              (if (eqv? #\. comma-sep) #\, #\.)))
                         (diff (- width (+ prec
                                           (if (char? dec-sep)
                                             1
                                             (string-length dec-sep))))))
                    (each (if (positive? diff) (make-string diff #\#) "")
                          dec-sep (make-string prec #\#)))
                  (displayed (make-string width #\#)))))
          (displayed str)))))

    (define (fork-width vars width fmts)
      (let1-values (gen gen/length) (gfork ((each-in-list fmts) vars))
        (values
          gen
          (unindented-length gen/length
                             (+ width 1)
                             (get-var vars string-width)))))

    (define (padded width . fmts)
      (assume (integer? width))
      (assume (positive? width))
      (λ vars
        (let1-values (gen actual-width) (fork-width vars width fmts)
          (if (< actual-width width)
            (chain (- width actual-width)
                   (make-string <> (get-var vars pad-char))
                   (whitespace-span)
                   (generator)
                   (gappend <> gen))
            gen))))

    (define (padded/right width . fmts)
      (assume (integer? width))
      (assume (positive? width))
      (λ vars
        (let1-values (gen actual-width) (fork-width vars width fmts)
          (if (< actual-width width)
            (chain (- width actual-width)
                   (make-string <> (get-var vars pad-char))
                   (whitespace-span)
                   (generator)
                   (gappend gen <>))
            gen))))

    (define (padded/both width . fmts)
      (assume (integer? width))
      (assume (positive? width))
      (λ vars
        (let1-values (gen actual-width) (fork-width vars width fmts)
          (if (< actual-width width)
            (let* ((pad (- width actual-width))
                   (half-pad (quotient pad 2))
                   (ch (get-var vars pad-char)))
              (gappend
                (generator (whitespace-span (make-string half-pad ch)))
                gen
                (generator (whitespace-span (make-string (- pad half-pad) ch)))))
            gen))))

    (define (trimmed width . fmts)
      (assume (integer? width))
      (assume (positive? width))
      (call-with-output (each-in-list fmts)
        (λ str (λ vars
          (let* ((string-width (get-var vars string-width))
                 (substring/width (get-var vars substring/width))
                 (actual-width (string-width str)))
            ((displayed
               (if (> actual-width width)
                 (let* ((ellipsis (get-var vars ellipsis))
                        (ellipsis-width (string-width ellipsis)))
                   (string-append
                     ellipsis
                     (substring/width str
                                      (- actual-width (- width ellipsis-width))
                                      actual-width)))
                 str))
             vars))))))

    (define (trimmed/right width . fmts)
      (assume (integer? width))
      (assume (positive? width))
      (call-with-output (each-in-list fmts)
        (λ str (λ vars
          (let* ((string-width (get-var vars string-width))
                 (substring/width (get-var vars substring/width))
                 (actual-width (string-width str)))
            ((displayed
               (if (> actual-width width)
                 (let* ((ellipsis (get-var vars ellipsis))
                        (ellipsis-width (string-width ellipsis)))
                   (string-append
                     (substring/width str 0 (- width ellipsis-width))
                     ellipsis))
                 str))
             vars))))))

    (define (trimmed/both width . fmts)
      (assume (integer? width))
      (assume (positive? width))
      (call-with-output (each-in-list fmts)
        (λ str (λ vars
          (let* ((string-width (get-var vars string-width))
                 (substring/width (get-var vars substring/width))
                 (actual-width (string-width str)))
            ((displayed
               (if (> actual-width width)
                 (let* ((ellipsis (get-var vars ellipsis))
                        (ellipsis-width (string-width ellipsis))
                        (pad (- actual-width width (* -2 ellipsis-width)))
                        (half-pad (quotient pad 2)))
                   (string-append
                     ellipsis
                     (substring/width str
                                      half-pad
                                      (- actual-width (- pad half-pad)))
                     ellipsis))
                 str))
             vars))))))

    (define (trimmed/lazy width . fmts)
      (assume (integer? width))
      (assume (positive? width))
      (λ vars
        (match-let* ((string-width (get-var vars string-width))
                     (substring/width (get-var vars substring/width))
                     ((len spans)
                        (call/cc (λ return
                          (generator-fold
                            (λ(span (len spans))
                              (if (>= len width)
                                (return (list len spans))
                                (list (+ len (string-width (span-text span)))
                                      (cons span spans))))
                            '(0 ())
                            ((each-in-list fmts) vars))))))
          (chain (if (> len width)
                   (let loop ((len len) (spans spans))
                     (let1 span-len (string-width (span-text (car spans)))
                       (cond
                         ((> (- len span-len) width)
                           (loop (- len span-len) (cdr spans)))
                         ((= (- len span-len) width)
                           (cdr spans))
                         (else
                           (cons
                             (span-map-text
                               (cut substring/width <> 0 (- span-len (- len width)))
                               (car spans))
                             (cdr spans))))))
                   spans)
                 (reverse)
                 (list->generator)))))

    (define (fitted width . fmts)
      (assume (integer? width))
      (assume (positive? width))
      (call-with-output (padded width (each-in-list fmts))
                        (cut trimmed width <>)))

    (define (fitted/right width . fmts)
      (assume (integer? width))
      (assume (positive? width))
      (call-with-output (padded/right width (each-in-list fmts))
                        (cut trimmed/right width <>)))

    (define (fitted/both width . fmts)
      (assume (integer? width))
      (assume (positive? width))
      (call-with-output (padded/both width (each-in-list fmts))
                        (cut trimmed/both width <>)))

    (define (forked fmt1 fmt2)
      (each (λ vars (fmt1 (copy-vars vars))) fmt2))

    (define (call-with-output fmt mapper)
      (assume (procedure? mapper))
      (λ vars
        (let1 str-port (open-output-string)
          (%show (copy-vars vars) str-port fmt)
          ((mapper (get-output-string str-port)) vars))))

    (define (call-with-output-generator fmt mapper)
      (assume (procedure? mapper))
      (λ vars
        ((mapper (fmt (copy-vars vars))) vars)))))
