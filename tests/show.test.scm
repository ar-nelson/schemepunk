(import (scheme base)
        (scheme char)
        (scheme cxr)
        (schemepunk syntax)
        (schemepunk term-colors)
        (schemepunk show)
        (schemepunk test))

(test-group "Show"
  (test-equal "" (show #f nothing))
  (test-equal "foo bar baz" (show #f "foo bar baz"))
  (test-equal "foobarbaz" (show #f "foo" "bar" "baz"))
  (test-equal "foobar" (with-output-to-string (λ() (show #t "foo") (show #t "bar"))))

  (test-group "displayed"
    (test-equal "foo" (show #f (displayed "foo")))
    (test-equal "x" (show #f (displayed #\x)))
    (test-equal "42" (show #f (displayed 42)))
    (test-equal "(1 2 3)" (show #f (displayed '(1 2 3))))
    (test-equal "#(1 2 3)" (show #f (displayed #(1 2 3)))))

  (test-group "written"
    (test-equal "\"foo\"" (show #f (written "foo")))
    (test-equal "#\\x" (show #f (written #\x)))
    (test-equal "42" (show #f (written 42)))
    (test-equal "(1 2 3)" (show #f (written '(1 2 3))))
    (test-equal "(1 2 3)" (show #f (written-simply '(1 2 3))))
    (test-equal "(1 2 3)" (show #f (written-shared '(1 2 3))))
    (test-equal "#(1 2 3)" (show #f (written #(1 2 3))))

    (test-equal "#0=(1 #0#)"
      (show #f (written (let1 xs (list 1 #f) (set-car! (cdr xs) xs) xs))))
    (test-equal "#0=(1 . #0#)"
      (show #f (written (let1 xs (cons 1 #f) (set-cdr! xs xs) xs))))
    (test-equal "((1 2) (1 2) (3 4) (3 4))"
      (show #f (written (let1 xs (list '(1 2) #f '(3 4) #f)
                          (set-car! (cdr xs) (car xs))
                          (set-car! (cdddr xs) (caddr xs))
                          xs))))

    (test-equal "#0=(1 #0#)"
      (show #f (written-shared (let1 xs (list 1 #f) (set-car! (cdr xs) xs) xs))))
    (test-equal "#0=(1 . #0#)"
      (show #f (written-shared (let1 xs (cons 1 #f) (set-cdr! xs xs) xs))))
    (test-equal "(#0=(1 2) #0# #1=(3 4) #1#)"
      (show #f (written-shared (let1 xs (list '(1 2) #f '(3 4) #f)
                                 (set-car! (cdr xs) (car xs))
                                 (set-car! (cdddr xs) (caddr xs))
                                 xs)))))

  (test-group "numeric"
    (test-equal "0" (show #f (numeric 0)))
    (test-equal "42" (show #f (numeric 42)))
    (test-equal "1/3" (show #f (numeric 1/3)))
    (test-equal "2a 2a" (show #f (numeric 42 16) " " (with ((radix 16)) (numeric 42))))
    (test-equal "9z 9z" (show #f (numeric 359 36) " " (with ((radix 36)) (numeric 359))))

    (test-equal "+23;8c:000 +23;8c:000"
      (show #f (numeric 9100 16 3 #t 2 #\; #\:)
               " "
               (with ((radix 16) (precision 3) (sign-rule #t)
                      (comma-rule 2) (comma-sep #\;) (decimal-sep #\:))
                 (numeric 9100))))
    (test-equal "0.00" (show #f (numeric 0 #f 2)))
    (test-equal "(100)" (show #f (numeric -100 #f #f #\()))
    (test-equal "0.33333333333333333333" (show #f (numeric 1/3 #f 20)))
    (test-equal "100,000,00" (show #f (numeric 10000000 #f #f #f '(2 3))))

    (test-equal "123,456,789" (show #f (numeric/comma 123456789)))
    (test-equal "1,23,45,67,89" (show #f (numeric/comma 123456789 2)))
    (test-equal "12,34,56,789" (show #f (numeric/comma 123456789 '(3 2))))

    (test-equal "608" (show #f (numeric/si 608)))
    (test-equal "608B" (show #f (numeric/si 608) "B"))
    (test-equal "608 B" (show #f (numeric/si 608 1000 " ") "B"))
    (test-equal "4k" (show #f (numeric/si 3986)))
    (test-equal "3.9KiB" (show #f (numeric/si 3986 1024) "B"))
    (test-equal "1.2µm" (show #f (numeric/si 1.23e-6) "m"))
    (test-equal "1.2 µm" (show #f (numeric/si 1.23e-6 1000 " ") "m"))

    (test-equal "1.25" (show #f (with ((precision 2)) (numeric/fitted 4 1.25))))
    (test-equal "#.##" (show #f (with ((precision 2)) (numeric/fitted 4 12.345))))
    (test-equal "##" (show #f (with ((precision 0)) (numeric/fitted 2 123.45)))))

  (test-group "escaped"
    (test-equal "hi, bob!" (show #f (escaped "hi, bob!")))
    (test-equal "hi, \\\"bob!\\\"" (show #f (escaped "hi, \"bob!\"")))
    (test-equal "hi, \"bob!\"" (show #f (escaped "hi, \"bob!\"" #\')))
    (test-equal "hi, ~\"bob!~\"" (show #f (escaped "hi, \"bob!\"" #\" #\~)))
    (test-equal "hi, \"\"bob!\"\"" (show #f (escaped "hi, \"bob!\"" #\" #f)))
    (test-equal "hi, ~\"bob~!~\""
      (show #f (escaped "hi, \"bob!\"" #\" #\~ (λ c (and (eqv? c #\!) c)))))

    (test-equal "foo" (show #f (maybe-escaped "foo" char-whitespace? #\")))
    (test-equal "\"foo bar\""
      (show #f (maybe-escaped "foo bar" char-whitespace? #\")))
    (test-equal "\"foo\\\"bar\\\"baz\""
      (show #f (maybe-escaped "foo\"bar\"baz" char-whitespace? #\"))))

  (test-group "joined"
    (test-equal "a, b, c" (show #f (joined displayed '(a b c) ", ")))
    (test-equal "/usr/local/bin" (show #f (joined/prefix displayed '(usr local bin) "/")))
    (test-equal "1\n2\n3\n" (show #f (joined/suffix displayed '(1 2 3) nl)))
    (test-equal "lions, tigers, and bears"
      (show #f (joined/last displayed
                  (λ last (each "and " last))
                  '(lions tigers bears)
                  ", ")))
    (test-equal  "(1 2 . 3)"
      (show #f
            "("
            (joined/dot displayed
                (λ dot (each ". " dot))
                '(1 2 . 3)
                " ")
            ")"))
   (test-equal "0 1 2 3 4" (show #f (joined/range displayed 0 5 " "))))

  (test-group "row and col"
    (test-equal "\n\n" (show #f fl nl fl nl fl))
    (test-equal "\nx\n\n" (show #f fl nl "x" fl nl fl))
    (test-equal "    x" (show #f (space-to 4) "x"))
    (test-equal "    x  y" (show #f (space-to 4) "x" (space-to 7) "y" (space-to 5)))
    (test-equal "    x  y" (show #f "    x " (space-to 7) "y"))
    (test-equal "(1 2 3)   " (show #f (written '(1 2 3)) (space-to 10)))
    (test-equal "    x\n     y" (show #f (space-to 4) "x" nl (space-to 5) "y"))
    (test-equal "    x\n     y" (show #f (space-to 4) "x\n" (space-to 5) "y"))
    (test-equal "....x  y"
      (show #f (with ((pad-char #\.)) (space-to 4)) "x" (space-to 7) "y"))
    (test-equal "x       y       " (show #f "x" (tab-to) (tab-to) "y" (tab-to)))
    (test-equal "x   y   " (show #f "x" (tab-to 4) (tab-to 4) "y" (tab-to 4)))
    (test-equal "x,,,,,,,y       "
      (show #f "x" (with ((pad-char #\,)) (tab-to)) "y" (tab-to))))

  (test-group "pad and trim"
    (define str "The rain in Spain stays mainly on the plain") ; length: 43
    (test-equal "       The rain in Spain stays mainly on the plain"
      (show #f (padded 50 str)))
    (test-equal ".......The rain in Spain stays mainly on the plain"
      (show #f (with ((pad-char #\.)) (padded 50 str))))
    (test-equal str (show #f (padded 40 str)))
    (test-equal "The rain in Spain stays mainly on the plain       "
      (show #f (padded/right 50 str)))
    (test-equal "The rain in Spain stays mainly on the plain......."
      (show #f (with ((pad-char #\.)) (padded/right 50 str))))
    (test-equal str (show #f (padded/right 40 str)))
    (test-equal "   The rain in Spain stays mainly on the plain    "
      (show #f (padded/both 50 str)))
    (test-equal "...The rain in Spain stays mainly on the plain...."
      (show #f (with ((pad-char #\.)) (padded/both 50 str))))
    (test-equal str (show #f (padded/both 40 str)))

    (test-equal " the plain" (show #f (trimmed 10 str)))
    (test-equal "...e plain" (show #f (with ((ellipsis "...")) (trimmed 10 str))))
    (test-equal str (show #f (trimmed 50 str)))
    (test-equal "The rain i" (show #f (trimmed/right 10 str)))
    (test-equal "The rai..." (show #f (with ((ellipsis "...")) (trimmed/right 10 str))))
    (test-equal str (show #f (trimmed/right 50 str)))
    (test-equal "n stays ma" (show #f (trimmed/both 10 str)))
    (test-equal "...tays..." (show #f (with ((ellipsis "...")) (trimmed/both 10 str))))
    (test-equal str (show #f (trimmed/both 50 str)))

    (test-equal "The rain i" (show #f (trimmed/lazy 10 str)))
    (test-equal "The rai..." (show #f (with ((ellipsis "...")) (trimmed/lazy 10 str))))
    (test-equal str (show #f (trimmed/lazy 50 str)))
    (test-equal "(42 42 42 42 42 4"
      (show #f (trimmed/lazy 17 (written-simply (let1 xs (cons 42 #f) (set-cdr! xs xs) xs)))))
    (test-equal "(42 (42 (42 (42 ("
      (show #f (trimmed/lazy 17 (written-simply (let1 xs (list 42 #f) (set-car! (cdr xs) xs) xs)))))

    (test-equal "       The rain in Spain stays mainly on the plain"
      (show #f (fitted 50 str)))
    (test-equal "The rain in Spain stays mainly on the plain       "
      (show #f (fitted/right 50 str)))
    (test-equal "   The rain in Spain stays mainly on the plain    "
      (show #f (fitted/both 50 str)))
    (test-equal " the plain" (show #f (fitted 10 str)))
    (test-equal "The rain i" (show #f (fitted/right 10 str)))
    (test-equal "n stays ma" (show #f (fitted/both 10 str)))

    (test-equal "日本" (show #f (trimmed/right 2 "日本語")))
    (test-equal "日" (show #f (terminal-aware (trimmed/right 2 "日本語")))))

  (test-group "unicode"
    (test-equal 0 (string-terminal-width ""))
    (test-equal 3 (string-terminal-width "foo"))
    (test-equal 3 (string-terminal-width/wide "foo"))
    (test-equal 6 (string-terminal-width "ｆｏｏ"))
    (test-equal 5 (string-terminal-width "ź̶̖a̸̱̔l̴̡̃g̴̩̈́o̸͉͑"))
    (test-equal 3 (string-terminal-width "42Å"))
    (test-equal 4 (string-terminal-width/wide "42Å"))

    (test-equal "bar" (substring-terminal-width "foobarbaz" 3 6))
    (test-equal "bar" (substring-terminal-width/wide "foobarbaz" 3 6))
    (test-equal "baz" (substring-terminal-width "foobarbaz" 6))
    (test-equal "baz" (substring-terminal-width/wide "foobarbaz" 6))
    (test-equal "ｏｏｂ" (substring-terminal-width "ｆｏｏｂａｒ" 2 8))
    (test-equal "ｏｂ" (substring-terminal-width "ｆｏｏｂａｒ" 3 8))
    (test-equal "ｏｏ" (substring-terminal-width "ｆｏｏｂａｒ" 2 7))
    (test-equal "ｂａｒ" (substring-terminal-width "ｆｏｏｂａｒ" 6))
    (test-equal "l̴̡̃g̴̩̈́o̸͉͑" (substring-terminal-width "ź̶̖a̸̱̔l̴̡̃g̴̩̈́o̸͉͑" 2))
    (test-equal "l̴̡̃g̴̩̈́" (substring-terminal-width "ź̶̖a̸̱̔l̴̡̃g̴̩̈́o̸͉͑" 2 4))
    (test-equal "4" (substring-terminal-width "12Å34" 4))
    (test-equal "34" (substring-terminal-width/wide "12Å34" 4))

    (test-equal
      (with-output-to-string (λ() (write-in-color red "oo")))
      (substring-terminal-width
        (with-output-to-string (λ() (write-in-color red "foo")))
        1))
    (test-equal
      (with-output-to-string (λ()
        (write-in-color blue "oo")
        (write-in-color red "b")))
      (substring-terminal-width
        (with-output-to-string (λ()
          (write-in-color blue "foo")
          (write-in-color red "bar")))
        1 4))

    (test-equal "ABC" (show #f (upcased "abc")))
    (test-equal "μέλος" (show #f (downcased "ΜΈΛΟΣ"))))

  (test-group "columnar"
    (test-equal "abc     123\ndef     456\n"
      (show #f (with ((width 16))
                 (columnar (each "abc\ndef\n")
                           (each "123\n456\n")))))
    (test-equal "abc     123\ndef     456\n"
      (show #f (with ((width 16))
                 (columnar (each "abc\ndef")
                           (each "123\n456")))))
    (test-equal "abc     123\ndef     \n"
      (show #f (with ((width 16))
                 (columnar (each "abc\ndef")
                           (each "123")))))
    (test-equal "abc.....123.....\ndef.....456.....\n"
      (show #f (with ((width 16) (pad-char #\.))
                 (columnar (each "abc\ndef\n")
                           (each "123\n456\n")))))
    (test-equal "     abc     123\n     def     456\n"
      (show #f (with ((width 16))
                 (columnar 'right (each "abc\ndef\n")
                           'right (each "123\n456\n")))))
    (test-equal "abc       123   \ndef       456   \n"
      (show #f (with ((width 16))
                 (columnar 'left (each "abc\ndef\n")
                           'center (each "123\n456\n")))))
    (test-equal "/* abc  | 123 */\n/* def  | 456 */\n"
      (show #f (with ((width 16))
                 (columnar "/* " (each "abc\ndef\n")
                           " | " (each "123\n456\n")
                           " */"))))
    (test-equal "|a  |123|\n|bc |45 |\n|def|6  |\n"
      (show #f (tabular "|" (each "a\nbc\ndef\n") "|"
                            (each "123\n45\n6\n") "|"))))

  (test-group "wrapped"
    (define doc
      (string-append
        "The fundamental list iterator.  Applies KONS to each "
        "element of LS and the result of the previous application, "
        "beginning with KNIL.  With KONS as CONS and KNIL as '(), "
        "equivalent to REVERSE."))

    (define doc/list
      '("The" "fundamental" "list" "iterator."
        "Applies" "KONS" "to" "each" "element" "of" "LS" "and" "the" "result"
        "of" "the" "previous" "application," "beginning" "with" "KNIL."
        "With" "KONS" "as" "CONS" "and" "KNIL" "as" "'()," "equivalent" "to"
        "REVERSE."))

    (test-equal "The fundamental list iterator.
Applies KONS to each element of
LS and the result of the previous
application, beginning with KNIL.
With KONS as CONS and KNIL as '(),
equivalent to REVERSE.
"
      (show #f (with ((width 36)) (wrapped doc))))

    (test-equal "The fundamental list iterator.
Applies KONS to each element of
LS and the result of the previous
application, beginning with KNIL.
With KONS as CONS and KNIL as '(),
equivalent to REVERSE.
"
      (show #f (with ((width 36)) (wrapped/list doc/list))))

    (test-equal "The fundamental list iterator.  Appl
ies KONS to each element of LS and t
he result of the previous applicatio
n, beginning with KNIL.  With KONS a
s CONS and KNIL as '(), equivalent t
o REVERSE.
"
      (show #f (with ((width 36)) (wrapped/char doc))))

    (test-equal "The   fundamental   list   iterator.
Applies  KONS  to  each  element  of
LS  and  the  result of the previous
application,  beginning  with  KNIL.
With  KONS  as CONS and KNIL as '(),
equivalent to REVERSE.
"
      (show #f (with ((width 36)) (justified doc)))))
)
