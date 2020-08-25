;; Command-line argument processor, loosely based on (chibi app) but with
;; a different spec syntax.
;;
;; (run-application <spec> <args> <proc>) parses the command-line argument list
;; <args> with the spec <spec>. It passes five arguments to <proc>:
;;
;; (<options> <extra-args> <command> <command-options> <command-extra-args>)
;;
;; <options> and <command-options> are alists of - or -- options, before and
;; after the command name.
;;
;; <extra-args> and <command-extra-args> are lists of string arguments; all
;; arguments before and after the command name that are not options.
;;
;; <command> is the command name; it is #f if no command is passed.
;;
;; A specification is a configuration alist, usually quoted. Here is an example,
;; a translation of (chibi app)'s zoo example:
;;
;; ((name "Zookeeper Application")
;;  (doc "Example application from (chibi app) documentation, adapted for \
;;        (schemepunk command).")
;;  (copyright "Copyright (c) 2020")
;;  (options
;;    (animals
;;      (type (list symbol))
;;      (doc "list of animals to act on (default all)"))
;;    (lions
;;      (short #\l)
;;      (doc "also apply the action to lions")))
;;  (commands
;;    (feed
;;      (short-doc "feed the animals")
;;      (doc-args <animals> ...))
;;    (wash
;;      (short-doc "wash the animals")
;;      (doc-args <animals> ...)
;;      (options (soap)))
;;    (help
;;      (short-doc "print help")))
;;  (default-help-option #t)
;;  (default-help-command #f)
;;  (require-command #t))
;;
(define-library (schemepunk command)
  (export parse-options parse-app run-application
          app-help command-help app-usage command-usage
          command-error?)

  (import (scheme base)
          (scheme process-context)
          (scheme char)
          (scheme read)
          (scheme write)
          (schemepunk syntax)
          (schemepunk list)
          (schemepunk function)
          (schemepunk string)
          (schemepunk show base)
          (schemepunk show columnar)
          (schemepunk show color))

  (begin
    (define (conf-get alist key)
      (match (assv key alist)
        ((_ x) x)
        ((_ . x) x)
        (else #f)))

    (define (conf-get-list alist key)
      (match (assv key alist)
        ((_ . (? pair? x)) x)
        ((_ . x) (list x))
        (else '())))

    (define command-error-marker (list 'command-error))

    (define (command-error? err)
      (and (error-object? err)
           (pair? (error-object-irritants err))
           (eq? command-error-marker (car (error-object-irritants err)))))

    (define (fail . fmts)
      (error (apply show #f fmts)
             command-error-marker
             (each-in-list fmts)))

    (define (run-application spec args proc)
      (guard (err ((command-error? err)
                     (show (current-error-port)
                       (as-red (cadr (error-object-irritants err)))
                       fl
                       nl
                       (app-usage spec args))
                     (exit 1)))
        (call-with-values
          (λ() (parse-app spec args))
          (chain proc
            ((if (conf-get spec 'default-help-command)
               (λ proc
                 (λ(o a command co ca)
                   (case command
                     ((help)
                       (match ca
                         ((command)
                           (show (current-error-port)
                             (command-help spec command args)))
                         (else (fail (each "Expected command name after "
                                           (as-bold "help")
                                           ".")))))
                     (else (proc o a command co ca)))))
               identity)
             _)
            ((if (conf-get spec 'default-help-option)
               (λ proc
                 (λ(options a c co ca)
                   (if (conf-get options 'help)
                     (show (current-error-port) (app-help spec args))
                     (proc options a c co ca))))
               identity)
             _)))))

    (define (spec-with-help spec)
      (define options-spec (conf-get-list spec 'options))
      (define commands-spec (conf-get-list spec 'commands))
      (when (conf-get spec 'default-help-option)
        (set! options-spec
          (snoc options-spec
                '(help (doc "Display this help and exit.")
                       (short #\h)))))
      (when (conf-get spec 'default-help-command)
        (set! commands-spec
          (snoc commands-spec
                '(help (short-doc "Display help and options for a command.")
                       (doc-args <command>)))))
      (map
        (λ((k . v))
          (case k
            ((options) (cons k options-spec))
            ((commands) (cons k commands-spec))
            (else (cons k v))))
        spec))

    (define (parse-app spec args)
      (define spec* (spec-with-help spec))
      (define options-spec (conf-get-list spec* 'options))
      (define commands-spec (conf-get-list spec* 'commands))
      (define require-command (conf-get spec* 'require-command))
      (let*-values
        (((options extra-args command rest)
           (parse-options options-spec commands-spec (cdr args)))
         ((command-spec)
           (if command
             (cdr (assv command commands-spec))
             (if (and require-command
                      (not (and (conf-get spec 'default-help-option)
                                (conf-get options 'help))))
               (fail "A command is required.")
               #f)))
         ((command-options command-extra-args _1 _2)
           (if command-spec
             (parse-options (conf-get-list command-spec 'options) '() rest)
             (values '() '() #f #f))))
        (values options extra-args command command-options command-extra-args)))

    (define (parse-options options-spec commands-spec args)
      (let loop ((options '()) (extra-args '()) (rest args))
        (match rest
          (()
            (values options (reverse extra-args) #f '()))
          (("--" . all-args)
            (values options (append (reverse extra-args) all-args) #f '()))
          (((? (λ=> (string->symbol _) (assv _ commands-spec)) command) . rest)
            (values options (reverse extra-args) (string->symbol command) rest))
          (((? (λ=> (substring _ 0 2) (equal? _ "--")) option) . rest)
            (match-let1 (opt rest)
                        (parse-long-option options-spec (string-drop option 2) rest)
              (loop (cons opt options) extra-args rest)))
          (("-" . rest)
            (loop options (cons "-" extra-args) rest))
          (((? (λ=> (substring _ 0 1) (equal? _ "-")) option) . rest)
            (match-let1 (new-options rest)
                        (parse-short-option options-spec (string-drop option 1) rest)
              (loop (append new-options options) extra-args rest)))
          ((arg . rest)
            (loop options (cons arg extra-args) rest)))))

    (define (parse-long-option options-spec option rest)
      (define (has-alias? alias spec)
        (member alias (conf-get-list (cdr spec) 'long)))
      (define len (string-length option))
      (define-values (before= after=)
        (do ((i 0 (+ i 1)))
            ((or (is i >= len)
                 (is (string-ref option i) eqv? #\=))
              (values (substring option 0 i)
                      (and (is i < len) (substring option (+ i 1) len))))))
      (define option-spec
        (or (assv (string->symbol before=) options-spec)
            (find (is before= has-alias? _) options-spec)))
      (if option-spec
        (let1-values (value rest)
                     (parse-option-value (cdr option-spec)
                                         (string-append "--" before=)
                                         after=
                                         rest)
          (list (cons (car option-spec) value) rest))
        (or
          (and-let* (((equal? "no-" (substring before= 0 3)))
                     (neg-opt (string-drop before= 3))
                     (neg-spec
                       (or (assv (string->symbol neg-opt) options-spec)
                           (find (is neg-opt has-alias? _) options-spec))))
            (case (or (conf-get (cdr neg-spec) 'type) 'boolean)
              ((boolean)
                 (let1-values (value rest)
                              (parse-option-value (cdr neg-spec)
                                                  (string-append "--" before=)
                                                  after=
                                                  rest)
                   (list (cons (car neg-spec) (not value)) rest)))
              (else
                (fail "Unrecognized option "
                      (as-bold "--" before=)
                      " (prefix 'no-' cannot be used with non-boolean option "
                      (as-bold "--" neg-opt)
                      ")."))))
          (fail "Unrecognized option " (as-bold "--" before=) "."))))

    (define (parse-short-option options-spec option rest)
      (define (has-alias? alias spec)
        (member alias (conf-get-list (cdr spec) 'short)))
      (chain (string->list option)
        (pair-fold
          (λ((ch . next) accum)
            (match (find (is ch has-alias? _) options-spec)
              ((name . spec)
                (cond
                  ((null? next)
                    (let1-values (value new-rest)
                                 (parse-option-value spec (string #\- ch) #f rest)
                      (set! rest new-rest)
                      (cons (cons name value) accum)))
                  ((eqv? 'boolean (or (conf-get spec 'type) 'boolean))
                    (cons (cons name #t) accum))
                  (else
                    (fail "Missing value for option " (as-bold "-" ch) "."))))
              (else
                (fail "Unrecognized option " (as-bold "-" ch) "."))))
          '()
          _)
        (list _ rest)))

    (define (parse-option-value option-spec option-name after= rest)
      (define flag? (eqv? 'boolean (or (conf-get option-spec 'type) 'boolean)))
      (define value-string
        (or after=
            (and (not flag?)
                 (pair? rest)
                 (or (is (string-length (car rest)) < 2)
                     (isnt (string-ref (car rest) 0) eqv? #\-))
                 (let1 x (car rest)
                   (set! rest (cdr rest))
                   x))))
      (cond
        (value-string
          (match (parse-value (or (conf-get option-spec 'type) 'boolean)
                              value-string)
            ((val #f) (values val rest))
            ((_ err) (fail "Bad value for option "
                           (as-bold option-name)
                           ": "
                           err
                           ", got "
                           (as-bold (written value-string))
                           "."))))
        (flag?
          (values #t rest))
        (else
          (fail "Missing value for option " (as-bold option-name) "."))))

    (define (parse-value type str)
      (match type
        (('list el)
          (let1-values (vals errs) (chain str
                                          (string-split _ ",")
                                          (map (cut parse-value el <>) _)
                                          (unzip2 _))
            (list vals (find string? errs))))
        ('boolean
          (chain str
                 (string-downcase _)
                 (member _ '("0" "no" "off" "false" "#f" "#false"))
                 (not _)
                 (list _ #f)))
        ((or 'integer 'number 'real)
          (let1 n (string->number str)
            (list n
              (cond
                ((not n) "expected a number")
                ((and (eq? type 'integer) (not (integer? n))) "expected an integer")
                ((and (eq? type 'real) (not (real? n))) "expected a real number")
                (else #f)))))
        ('symbol
          (list (string->symbol str) #f))
        ('char
          (if (= 1 (string-length str))
            (list (string-ref str 0) #f)
            (list #f "expected a single character")))
        ('sexp
          (list (guard (exn (else (list #f "expected a Scheme S-expression")))
                  (with-input-from-string str read))
                #f))
        (else
          (list str #f))))

    (define (long-option->string name doc-value)
      (string-append
        "--"
        name
        (if doc-value (string-append "=" doc-value) "")))

    (define (short-option->string name doc-value)
      (string-append
        (string #\- name)
        (if doc-value (string-append " " doc-value) "")))

    (define (option->string option-spec separator)
      (define type
        (or (conf-get (cdr option-spec) 'type) 'boolean))
      (define doc-value
        (or (chain-and (conf-get (cdr option-spec) 'doc-value)
                       (show #f _))
            (and (not (eqv? 'boolean type))
                 (format #f "<~a>" type))))
      (string-join
        (append
          (map (cut short-option->string <> doc-value)
               (conf-get-list (cdr option-spec) 'short))
          (list (long-option->string (symbol->string (car option-spec))
                                     doc-value))
          (map (λ=> (format #f "~a" _) (long-option->string _ doc-value))
               (conf-get-list (cdr option-spec) 'long)))
        separator))

    (define (app-help spec args)
      (define name (conf-get spec 'name))
      (define doc (conf-get spec 'doc))
      (define copyright (conf-get spec 'copyright))
      (each
        (if name
          (each (as-bold (wrapped name)) fl nl)
          nothing)
        (if doc
          (each (wrapped doc) fl nl)
          nothing)
        (app-usage spec args)
        (if copyright
          (each fl nl (as-gray (wrapped copyright)))
          nothing)
        fl))

    (define (command-help spec command args)
      (define command-spec
        (conf-get-list (conf-get-list spec 'commands)
                       (if (string? command)
                         (string->symbol command)
                         command)))
      (define doc (conf-get command-spec 'doc))
      (define copyright (conf-get spec 'copyright))
      (each
        (as-bold command)
        fl
        nl
        (if doc
          (each (wrapped doc) fl nl)
          nothing)
        (command-usage spec command args)
        (if copyright
          (each fl nl (as-gray (wrapped copyright)))
          nothing)
        fl))

    (define (app-usage spec args)
      (define spec* (spec-with-help spec))
      (define executable (if (pair? args) (car args) "app"))
      (define options-spec (conf-get-list spec* 'options))
      (define commands-spec (conf-get-list spec* 'commands))
      (define documented-options (filter (λ=> (cdr _) (assv 'doc _)) options-spec))
      (fn (string-width)
        (each
          (as-bold "Usage:")
          nl
          nl
          (collapsed-if-one-line
            (columnar
              (string-width executable) (each executable)
              " "
              (each
                (wrapped/list
                  (map (λ=> (option->string _ " | ") (format #f "[~a]" _))
                       options-spec))
                fl
                (wrapped/list
                  (append
                    (map (cut show #f <>) (conf-get-list spec 'doc-args))
                    (if (pair? commands-spec)
                      (if (conf-get spec 'require-command)
                        '("<command>" "<args>" "...")
                        '("[<command> <args> ...]"))
                      '()))))))
          (if (pair? documented-options)
            (each
              fl
              nl
              (as-bold "Options:")
              nl
              nl
              (joined
                (λ option-spec
                  (each
                    (as-light-white
                      (wrapped (option->string option-spec ", ")))
                    fl
                    (columnar
                      "  "
                      (wrapped (conf-get (cdr option-spec) 'doc)))))
                documented-options
                fl))
            nothing)
          (if (pair? commands-spec)
            (each
              fl
              nl
              (as-bold "Commands:")
              nl
              nl
              (let1 width (fold max 0
                            (map (compose string-width symbol->string car) commands-spec))
                (joined
                  (λ((command . spec))
                    (columnar width 'right (as-light-white command)
                              3 (as-gray " - ")
                              (wrapped (or (conf-get spec 'short-doc)
                                           (conf-get spec 'doc)
                                           ""))))
                  commands-spec
                  fl)))
            nothing)
          fl)))

    (define (command-usage spec command args)
      (define executable (if (pair? args) (car args) "app"))
      (define command-spec
        (conf-get-list (conf-get-list spec 'commands)
                       (if (string? command)
                         (string->symbol command)
                         command)))
      (define options-spec (conf-get-list command-spec 'options))
      (define documented-options (filter (λ=> (cdr _) (assv 'doc _)) options-spec))
      (fn (string-width)
        (each
          (as-bold "Usage:")
          nl
          nl
          (collapsed-if-one-line
            (columnar
              (+ (string-width executable)
                 1
                 (string-width (if (symbol? command)
                                 (symbol->string command)
                                 command)))
              (each executable " " command)
              " "
              (each
                (wrapped/list
                  (map (λ=> (option->string _ " | ") (format #f "[~a]" _))
                       options-spec))
                fl
                (wrapped/list
                  (map (cut show #f <>) (conf-get-list command-spec 'doc-args))))))
          (if (pair? documented-options)
            (each
              fl
              nl
              (as-bold "Options:")
              nl
              nl
              (joined
                (λ option-spec
                  (each
                    (as-light-white
                      (wrapped (option->string option-spec ", ")))
                    fl
                    (columnar
                      "  "
                      (wrapped (conf-get (cdr option-spec) 'doc)))))
                documented-options
                fl))
            nothing)
          fl)))))
