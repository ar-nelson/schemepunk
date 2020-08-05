(import (scheme base)
        (schemepunk syntax)
        (schemepunk command)
        (schemepunk show base)
        (schemepunk test))

(test-group "Command Line Argument Parsing"
  (define spec
    '((name "Zookeeper Application")
      (doc "Example application from (chibi app) documentation, adapted for \
            (schemepunk command).")
      (copyright "Copyright (c) 2020")
      (options
        (animals
          (type (list symbol))
          (short #\a)
          (long "animal-list")
          (doc "list of animals to act on (default all)"))
        (lions
          (type boolean)
          (short #\l)
          (doc "also apply the action to lions"))
        (tigers
          (type boolean)
          (short #\t)
          (doc "also apply the action to tigers"))
        (bears
          (type boolean)
          (short #\b)
          (doc "oh my")))
      (commands
        (feed
          (short-doc "feed the animals")
          (doc-args animals ...))
        (wash
          (short-doc "wash the animals")
          (doc-args animals ...)
          (options
            (soap
              (type boolean))))
        (help
          (short-doc "print help")))
      (default-help-option #t)
      (default-help-command #f)))

  (test "No options"
    (let1-values parsed (parse-app spec '("zoo"))
      (assert-equal parsed '(() () #f () ()))))

  (test "Short option"
    (let1-values parsed (parse-app spec '("zoo" "-l"))
      (assert-equal parsed '(((lions . #t)) () #f () ()))))

  (test "Multiple short options"
    (let1-values parsed (parse-app spec '("zoo" "-l" "-t" "-b"))
      (assert-equal parsed
        '(((bears . #t) (tigers . #t) (lions . #t)) () #f () ()))))

  (test "Grouped short options"
    (let1-values parsed (parse-app spec '("zoo" "-ltb"))
      (assert-equal parsed
        '(((bears . #t) (tigers . #t) (lions . #t)) () #f () ()))))

  (test "Short option with value"
    (let1-values parsed (parse-app spec '("zoo" "-a" "kangaroo"))
      (assert-equal parsed
        '(((animals kangaroo)) () #f () ()))))

  (test "Grouped short option with value"
    (let1-values parsed (parse-app spec '("zoo" "-ta" "kangaroo"))
      (assert-equal parsed
        '(((animals kangaroo) (tigers . #t)) () #f () ()))))

  (test "Long option"
    (let1-values parsed (parse-app spec '("zoo" "--lions"))
      (assert-equal parsed '(((lions . #t)) () #f () ()))))

  (test "Long option with value"
    (let1-values parsed (parse-app spec '("zoo" "--animals" "platypus"))
      (assert-equal parsed '(((animals platypus)) () #f () ()))))

  (test "Long option with list value"
    (let1-values parsed (parse-app spec '("zoo" "--animals" "lion,tiger,bear"))
      (assert-equal parsed '(((animals lion tiger bear)) () #f () ()))))

  (test "Long option with ="
    (let1-values parsed (parse-app spec '("zoo" "--animals=platypus"))
      (assert-equal parsed '(((animals platypus)) () #f () ()))))

  (test "Aliased long option"
    (let1-values parsed (parse-app spec '("zoo" "--animal-list=platypus"))
      (assert-equal parsed '(((animals platypus)) () #f () ()))))

  (test "Long option with 'no-' prefix"
    (let1-values parsed (parse-app spec '("zoo" "--no-lions"))
      (assert-equal parsed '(((lions . #f)) () #f () ()))))

  (test "Arguments"
    (let1-values parsed (parse-app spec '("zoo" "tortoise" "hare"))
      (assert-equal parsed '(() ("tortoise" "hare") #f () ()))))

  (test "Options and arguments"
    (let1-values parsed (parse-app spec '("zoo" "--animals" "tortoise" "hare"))
      (assert-equal parsed '(((animals tortoise)) ("hare") #f () ()))))

  (test "Command"
    (let1-values parsed (parse-app spec '("zoo" "feed"))
      (assert-equal parsed '(() () feed () ()))))

  (test "Options and command"
    (let1-values parsed (parse-app spec '("zoo" "-l" "wash"))
      (assert-equal parsed '(((lions . #t)) () wash () ()))))

  (test "Options, argument, and command"
    (let1-values parsed (parse-app spec '("zoo" "-l" "octopus" "wash"))
      (assert-equal parsed '(((lions . #t)) ("octopus") wash () ()))))

  (test "Command options"
    (let1-values parsed (parse-app spec '("zoo" "wash" "--soap"))
      (assert-equal parsed '(() () wash ((soap . #t)) ()))))

  (test "Command arguments"
    (let1-values parsed (parse-app spec '("zoo" "feed" "oats"))
      (assert-equal parsed '(() () feed () ("oats")))))

  (test "All five argument types"
    (let1-values parsed (parse-app spec '("zoo" "-l" "sheep" "wash" "--soap" "water"))
      (assert-equal parsed '(((lions . #t)) ("sheep") wash ((soap . #t)) ("water")))))

  (test "Unknown short option"
    (guard (err ((command-error? err) #t))
      (parse-app spec '("zoo" "-q"))
      (fail "did not raise command-error")))

  (test "Unknown long option"
    (guard (err ((command-error? err) #t))
      (parse-app spec '("zoo" "--fhqwhgads"))
      (fail "did not raise command-error")))

  (test "Unknown short command option"
    (guard (err ((command-error? err) #t))
      (parse-app spec '("zoo" "wash" "-q"))
      (fail "did not raise command-error")))

  (test "Unknown long command option"
    (guard (err ((command-error? err) #t))
      (parse-app spec '("zoo" "wash" "--fhqwhgads"))
      (fail "did not raise command-error")))

  (test "Arguments after --"
    (let1-values parsed (parse-app spec '("zoo" "-l" "foo" "--" "-t" "-b" "feed"))
      (assert-equal parsed '(((lions . #t)) ("foo" "-t" "-b" "feed") #f () ()))))

  (test "Default help option"
    (let1-values parsed (parse-app spec '("zoo" "--help"))
      (assert-equal parsed '(((help . #t)) () #f () ())))
    (let1-values parsed (parse-app spec '("zoo" "-h"))
      (assert-equal parsed '(((help . #t)) () #f () ()))))

  (test "Default help command"
    (let1-values parsed (parse-app spec '("zoo" "help" "wash"))
      (assert-equal parsed '(() () help () ("wash")))))

  (test "app-help doesn't crash"
    (assert-true (string? (show #f (app-help spec '("zoo"))))))

  (test "command-help doesn't crash"
    (assert-true (string? (show #f (command-help spec 'wash '("zoo"))))))
)
