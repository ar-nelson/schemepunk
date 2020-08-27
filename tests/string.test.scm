;; String library tests taken directly from the SRFI 152 sample implementation.
(import (scheme base)
        (scheme char)
        (schemepunk test)
        (schemepunk string))

(define (complement proc) (lambda (x) (not (proc x))))
(define (char-newline? ch) (eqv? ch #\newline))
(define (char-is-r? ch) (eqv? ch #\r))
(define (char-is-colon? ch) (eqv? ch #\:))
(define (char-is-a? ch) (eqv? ch #\a))
(define (char-is-space? ch) (eq? ch #\space))

;; artefact of converting from cursors to indexes and back
(define (dummy-index string index) index)

(define ABC "abc")

;; Kawa is constrained by the JVM's maximum method size,
;; and the SRFI 152 test suite is so huge that it exceeds this limit.
;;
;; Breaking the test suite up into procedures is a workaround for this issue.

(define (srfi-152-gauche-predicates)
  (test-equal "string-null?" #f (string-null? "abc"))
  (test-equal "string-null?" #t (string-null? ""))
  (test-equal "string-every" #t (string-every char-is-a? ""))
  (test-equal "string-every" #t (string-every char-is-a? "aaaa"))
  (test-equal "string-every" #f (string-every char-is-a? "aaba"))
  (test-equal "string-every" #t (string-every char-lower-case? "aaba"))
  (test-equal "string-every" #f (string-every char-lower-case? "aAba"))
  (test-equal "string-every" #t (string-every char-lower-case? ""))
  (test-equal "string-every" #t (string-every (lambda (x) (char-ci=? x #\a)) "aAaA"))
  (test-equal "string-every" #f (string-every (lambda (x) (char-ci=? x #\a)) "aAbA"))
  (test-equal "string-every" (char->integer #\A)
    (string-every (lambda (x) (char->integer x)) "aAbA"))
  (test-equal "string-every" #t
    (string-every (lambda (x) (error "hoge")) ""))
  (test-equal "string-any" #t (string-any char-is-a? "aaaa"))
  (test-equal "string-any" #f (string-any char-is-a? "Abcd"))
  (test-equal "string-any" #f (string-any #\a ""))
  (test-equal "string-any" #t (string-any char-lower-case? "ABcD"))
  (test-equal "string-any" #f (string-any char-lower-case? "ABCD"))
  (test-equal "string-any" #f (string-any char-lower-case? ""))
  (test-equal "string-any" #t (string-any (lambda (x) (char-ci=? x #\a)) "CAaA"))
  (test-equal "string-any" #f (string-any (lambda (x) (char-ci=? x #\a)) "ZBRC"))
  (test-equal "string-any" #f (string-any (lambda (x) (char-ci=? x #\a)) ""))
  (test-equal "string-any" (char->integer #\a)
    (string-any (lambda (x) (char->integer x)) "aAbA")))

(define (srfi-152-gauche-constructors)
  (test-equal "string-tabulate" "0123456789"
         (string-tabulate (lambda (code)
                            (integer->char (+ code (char->integer #\0))))
                          10))
  (test-equal "string-tabulate" ""
         (string-tabulate (lambda (code)
                            (integer->char (+ code (char->integer #\0))))
                          0))
  (test-equal "reverse-list->string" "cBa"
         (reverse-list->string '(#\a #\B #\c)))
  (test-equal "reverse-list->string" ""
         (reverse-list->string '()))
  (test-equal "string-join" "foo+bar+baz"
        (string-join '("foo" "bar" "baz") "+"))
  (test-equal "string-join" "foo bar baz"
        (string-join '("foo" "bar" "baz")))
  (test-equal "string-join" "/foo/bar/baz"
        (string-join '("foo" "bar" "baz") "/" 'prefix))
  (test-equal "string-join" "foo;bar;baz;"
        (string-join '("foo" "bar" "baz") ";" 'suffix)))

(define (srfi-152-gauche-selectors)
  (test-equal "substring" "cde" (substring "abcde" 2 5))
  (test-equal "substring" "cd"  (substring "abcde" 2 4))
  (test-equal "string-copy!" "abCDEfg"
         (let ((x (string-copy "abcdefg")))
           (string-copy! x 2 "CDE")
           x))
  (test-equal "string-copy!" "abCDEfg"
         (let ((x (string-copy "abcdefg")))
           (string-copy! x 2 "ZABCDE" 3)
           x))
  (test-equal "string-copy!" "abCDEfg"
         (let ((x (string-copy "abcdefg")))
           (string-copy! x 2 "ZABCDEFG" 3 6)
           x))
  (test-equal "string-take" "Pete S"  (string-take "Pete Szilagyi" 6))
  (test-equal "string-take" ""        (string-take "Pete Szilagyi" 0))
  (test-equal "string-take" "Pete Szilagyi" (string-take "Pete Szilagyi" 13))
  (test-equal "string-drop" "zilagyi" (string-drop "Pete Szilagyi" 6))
  (test-equal "string-drop" "Pete Szilagyi" (string-drop "Pete Szilagyi" 0))
  (test-equal "string-drop" ""        (string-drop "Pete Szilagyi" 13))

  (test-equal "string-take-right" "rules" (string-take-right "Beta rules" 5))
  (test-equal "string-take-right" ""      (string-take-right "Beta rules" 0))
  (test-equal "string-take-right" "Beta rules" (string-take-right "Beta rules" 10))
  (test-equal "string-drop-right" "Beta " (string-drop-right "Beta rules" 5))
  (test-equal "string-drop-right" "Beta rules" (string-drop-right "Beta rules" 0))
  (test-equal "string-drop-right" ""      (string-drop-right "Beta rules" 10))

  (test-equal "string-pad" "  325" (string-pad "325" 5))
  (test-equal "string-pad" "71325" (string-pad "71325" 5))
  (test-equal "string-pad" "71325" (string-pad "8871325" 5))
  (test-equal "string-pad" "~~325" (string-pad "325" 5 #\~))
  (test-equal "string-pad" "~~~25" (string-pad "325" 5 #\~ 1))
  (test-equal "string-pad" "~~~~2" (string-pad "325" 5 #\~ 1 2))
  (test-equal "string-pad-right" "325  " (string-pad-right "325" 5))
  (test-equal "string-pad-right" "71325" (string-pad-right "71325" 5))
  (test-equal "string-pad-right" "88713" (string-pad-right "8871325" 5))
  (test-equal "string-pad-right" "325~~" (string-pad-right "325" 5 #\~))
  (test-equal "string-pad-right" "25~~~" (string-pad-right "325" 5 #\~ 1))
  (test-equal "string-pad-right" "2~~~~" (string-pad-right "325" 5 #\~ 1 2))

  (test-equal "string-trim"  "a b c d  \n"
         (string-trim "  \t  a b c d  \n"))
  (test-equal "string-trim"  "\t  a b c d  \n"
         (string-trim "  \t  a b c d  \n" char-is-space?))
  (test-equal "string-trim"  "a b c d  \n"
         (string-trim "4358948a b c d  \n" char-numeric?))

  (test-equal "string-trim-right"  "  \t  a b c d"
         (string-trim-right "  \t  a b c d  \n"))
  (test-equal "string-trim-right"  "  \t  a b c d  "
         (string-trim-right "  \t  a b c d  \n" char-newline?))
  (test-equal "string-trim-right"  "349853a b c d"
         (string-trim-right "349853a b c d03490" char-numeric?))

  (test-equal "string-trim-both"  "a b c d"
         (string-trim-both "  \t  a b c d  \n"))
  (test-equal "string-trim-both"  "  \t  a b c d  "
         (string-trim-both "  \t  a b c d  \n" char-newline?))
  (test-equal "string-trim-both"  "a b c d"
         (string-trim-both "349853a b c d03490" char-numeric?)))

(define (srfi-152-gauche-replacement)
  (test-equal "string-replace" "-ab01234cdefghi"
        (string-replace "-abcdefghi" "01234" 3 3))
  (test-equal "string-replace" "-ab012cdefghi"
        (string-replace "-abcdefghi" "01234" 3 3 0 3))
  (test-equal "string-replace" "-ab01234fghi"
        (string-replace "-abcdefghi" "01234" 3 6))
  (test-equal "string-replace" "-ab34fghi"
        (string-replace "-abcdefghi" "01234" 3 6 3 5))
  (test-equal "string-replace" "abcdXYZghi"
         (string-replace "abcdefghi" "XYZ" 4 6))
  (test-equal "string-replace" "abcdZghi"
         (string-replace "abcdefghi" "XYZ" 4 6 2))
  (test-equal "string-replace" "abcdZefghi"
         (string-replace "abcdefghi" "XYZ" 4 4 2))
  (test-equal "string-replace" "abcdefghi"
         (string-replace "abcdefghi" "XYZ" 4 4 1 1))
  (test-equal "string-replace" "abcdhi"
         (string-replace "abcdefghi" "" 4 7)))

(define (srfi-152-gauche-comparison)
  (test-equal "string=?" #t (string=? "foo" "foo"))
  (test-equal "string=?" #t (string=? "abcd" (string-append "a" "b" "c" "d")))

  (test-equal "string<=?" #t (string<=? "fol" "foo"))
  (test-equal "string<?" #t (string<? "fol" "foo"))
  (test-equal "string>=?" #t (string>=? "foo" "fol"))
  (test-equal "string>?" #t (string>? "foo" "fol"))

  (test-equal "string-ci=?" #t (string-ci=? "Foo" "foO"))

  (test-equal "string-ci<=?" #t (string-ci<=? "FOL" "foo"))
  (test-equal "string-ci<?" #t (string-ci<? "fol" "FOO"))
  (test-equal "string-ci>=?" #t (string-ci>=? "FOO" "fol"))
  (test-equal "string-ci>?" #t (string-ci>? "FOO" "fol")))

(define (srfi-152-gauche-presuffixes)
  (test-equal "string-prefix-length" 5
         (string-prefix-length "cancaNCAM" "cancancan"))
  (test-equal "string-suffix-length" 2
         (string-suffix-length "CanCan" "cankancan"))

  (test-equal "string-prefix?" #t    (string-prefix? "abcd" "abcdefg"))
  (test-equal "string-prefix?" #f    (string-prefix? "abcf" "abcdefg"))
  (test-equal "string-suffix?" #t    (string-suffix? "defg" "abcdefg"))
  (test-equal "string-suffix?" #f    (string-suffix? "aefg" "abcdefg")))

(define (srfi-152-gauche-searching)
  (test-equal "string-index #1" 4
         (string-index "abcd:efgh:ijkl" char-is-colon?))
  (test-equal "string-index #2" 4
         (string-index "abcd:efgh;ijkl" (complement char-alphabetic?)))
  (test-equal "string-index #3" #f
         (string-index "abcd:efgh;ijkl" char-numeric?))
  (test-equal "string-index #4" 9
         (string-index "abcd:efgh:ijkl" char-is-colon? 5))
  (test-equal "string-index-right #1" 4
         (string-index-right "abcd:efgh;ijkl" char-is-colon?))
  (test-equal "string-index-right #2" 9
         (string-index-right "abcd:efgh;ijkl" (complement char-alphabetic?)))
  (test-equal "string-index-right #3" #f
         (string-index-right "abcd:efgh;ijkl" char-numeric?))
  (test-equal "string-index-right #4" 4
         (string-index-right "abcd:efgh;ijkl" (complement char-alphabetic?) 2 5))
  (test-equal "string-contains" 3
         (string-contains "Ma mere l'oye" "mer"))
  (test-equal "string-contains" #f
         (string-contains "Ma mere l'oye" "Mer")))

(define (srfi-152-gauche-foldmap)
  (test-equal "string-map" "svool"
         (string-map (lambda (c)
                       (integer->char (- 219 (char->integer c))))
                     "hello"))

  (test-equal "string-fold" '(#\o #\l #\l #\e #\h . #t)
         (string-fold cons #t "hello"))
  (test-equal "string-fold" '(#\l #\e . #t)
         (string-fold cons #t "hello" 1 3))
  (test-equal "string-fold-right" '(#\h #\e #\l #\l #\o . #t)
         (string-fold-right cons #t "hello"))
  (test-equal "string-fold-right" '(#\e #\l . #t)
         (string-fold-right cons #t "hello" 1 3))

  (test-equal "string-unfold" "hello"
         (string-unfold null? car cdr '(#\h #\e #\l #\l #\o)))
  (test-equal "string-unfold" "hi hello"
         (string-unfold null? car cdr '(#\h #\e #\l #\l #\o) "hi "))
  (test-equal "string-unfold" "hi hello ho"
         (string-unfold null? car cdr
                        '(#\h #\e #\l #\l #\o) "hi "
                        (lambda (x) " ho")))

  (test-equal "string-unfold-right" "olleh"
         (string-unfold-right null? car cdr '(#\h #\e #\l #\l #\o)))
  (test-equal "string-unfold-right" "olleh hi"
         (string-unfold-right null? car cdr '(#\h #\e #\l #\l #\o) " hi"))
  (test-equal "string-unfold-right" "ho olleh hi"
         (string-unfold-right null? car cdr
                              '(#\h #\e #\l #\l #\o) " hi"
                              (lambda (x) "ho ")))

  (test-equal "string-for-each" "CLtL"
         (let ((out (open-output-string))
               (prev #f))
           (string-for-each (lambda (c)
                              (if (or (not prev)
                                      (char-whitespace? prev))
                                  (write-char c out))
                              (set! prev c))
                            "Common Lisp, the Language")

           (get-output-string out)))

  #;(test-equal "string-for-each-index" '(4 3 2 1 0)
         (let ((r '()))
           (string-for-each-index (lambda (i) (set! r (cons i r))) "hello")
           r))
  #;(test-equal "string-for-each-index" '(4 3 2 1)
         (let ((r '()))
           (string-for-each-index (lambda (i) (set! r (cons i r))) "hello" 1)
           r))
  #;(test-equal "string-for-each-index" '(2 1)
         (let ((r '()))
           (string-for-each-index (lambda (i) (set! r (cons i r))) "hello" 1 3)
           r))
  (test-equal "string-count #1" 2
         (string-count "abc def\tghi jkl" char-is-space?))
  (test-equal "string-count #2" 3
         (string-count "abc def\tghi jkl" char-whitespace?))
  (test-equal "string-count #3" 2
         (string-count "abc def\tghi jkl" char-whitespace? 4))
  (test-equal "string-count #4" 1
         (string-count "abc def\tghi jkl" char-whitespace? 4 9))

  (test-equal "string-filter" "rrrr"
         (string-filter char-is-r? "Help make programs run, run, RUN!"))
  (test-equal "string-filter" "HelpmakeprogramsrunrunRUN"
         (string-filter char-alphabetic? "Help make programs run, run, RUN!"))

  (test-equal "string-filter" "programsrunrun"
         (string-filter (lambda (c) (char-lower-case? c))
                        "Help make programs run, run, RUN!"
                        10))
  (test-equal "string-filter" ""
         (string-filter (lambda (c) (char-lower-case? c)) ""))
  (test-equal "string-remove" "Help make pogams un, un, RUN!"
         (string-remove char-is-r? "Help make programs run, run, RUN!"))
  (test-equal "string-remove" "   , , !"
         (string-remove char-alphabetic? "Help make programs run, run, RUN!"))
  (test-equal "string-remove" " , , RUN!"
         (string-remove (lambda (c) (char-lower-case? c))
                        "Help make programs run, run, RUN!"
                        10))
  (test-equal "string-remove" ""
         (string-remove (lambda (c) (char-lower-case? c)) "")))

(define (srfi-152-gauche-replisplit)
  (test-equal "string-replicate" "cdefab"
         (string-replicate "abcdef" 2 8))
  (test-equal "string-replicate" "efabcd"
         (string-replicate "abcdef" -2 4))
  (test-equal "string-replicate" "abcabca"
         (string-replicate "abc" 0 7))
  ;; (test-equal "string-replicate" "abcabca"
  ;;        (string-replicate "abc"
  ;;                    30000000000000000000000000000000
  ;;                    30000000000000000000000000000007))
  (test-equal "string-replicate" "defdefd"
         (string-replicate "abcdefg" 0 7 3 6))
  (test-equal "string-replicate" ""
         (string-replicate "abcdefg" 9 9 3 6))

  (test-equal "string-segment" '("ab" "cd" "ef")
    (string-segment "abcdef" 2))
  (test-equal "string-segment" '("ab" "cd" "ef" "g")
    (string-segment "abcdefg" 2))
  (test-equal "string-segment" '()
    (string-segment "" 2))
  (test-equal "string-split" '("Help" "make" "programs" "run," "run," "RUN!")
         (string-split "Help make programs run, run, RUN!" " "))
  (test-equal "string-split" '("Help" "make" "programs run, run, RUN!")
         (string-split "Help make programs run, run, RUN!" " " 'infix 2))
  (test-equal "string-split" '("usr" "local" "bin")
         (string-split "/usr/local/bin" "/" 'prefix))
  (test-equal "string-split" '("be()" "here()" "now()")
         (string-split "be(); here(); now(); " "; " 'suffix)))


(define (srfi-152-gauche-append)
  (test-equal "string-append" #f
         (let ((s "test")) (eq? s (string-append s))))
  (test-equal "string-concatenate" #f
         (let ((s "test")) (eq? s (string-concatenate (list s)))))
  (test-equal "string-concatenate" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
         (string-concatenate
          '("A" "B" "C" "D" "E" "F" "G" "H"
            "I" "J" "K" "L" "M" "N" "O" "P"
            "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
            "a" "b" "c" "d" "e" "f" "g" "h"
            "i" "j" "k" "l" "m" "n" "o" "p"
            "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
  (test-equal "string-concatenate-reverse" "zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA"
         (string-concatenate-reverse
          '("A" "B" "C" "D" "E" "F" "G" "H"
            "I" "J" "K" "L" "M" "N" "O" "P"
            "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
            "a" "b" "c" "d" "e" "f" "g" "h"
            "i" "j" "k" "l" "m" "n" "o" "p"
            "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
  (test-equal "string-concatenate-reverse" #f
         (let ((s "test"))
           (eq? s (string-concatenate-reverse (list s))))))

(define (srfi-152-gauche-regression)
  ;;; Regression tests: check that reported bugs have been fixed

  ; From: Matthias Radestock <matthias@sorted.org>
  ; Date: Wed, 10 Dec 2003 21:05:22 +0100
  ;
  ; Chris Double has found the following bug in the reference implementation:
  ;
  ;  (string-contains "xabc" "ab") => 1    ;good
  ;  (string-contains "aabc" "ab") => #f   ;bad
  ;
  ; Matthias.

  (test-equal "string-contains" 1 (string-contains "aabc" "ab"))

  (test-equal "string-contains" 5 (string-contains "ababdabdabxxas" "abdabx"))


  ; (message continues)
  ;
  ; PS: There is also an off-by-one error in the bounds check of the
  ; unoptimized version of string-contains that is included as commented out
  ; code in the reference implementation. This breaks things like
  ; (string-contains "xab" "ab") and (string-contains "ab" "ab").

  ; This off-by-one bug has been fixed in the comments of the version
  ; of SRFI-13 shipped with Larceny.  In a version of the code without
  ; the fix the following test will catch the bug:

  (test-equal "string-contains" 0 (string-contains "ab" "ab"))

  ; From: dvanhorn@emba.uvm.edu
  ; Date: Wed, 26 Mar 2003 08:46:41 +0100
  ;
  ; The SRFI document gives,
  ;
  ;   string-filter s char/char-set/pred [start end] -> string
  ;   string-remove s char/char-set/pred [start end] -> string
  ;
  ; Yet the reference implementation switches the order giving,
  ;
  ;   ;;; string-remove char/char-set/pred string [start end]
  ;   ;;; string-filter char/char-set/pred string [start end]
  ;   ...
  ;   (define (string-remove criterion s . maybe-start+end)
  ;   ...)
  ;   (define (string-filter criterion s . maybe-start+end)
  ;   ...)
  ;
  ; I reviewed the SRFI-13 mailing list and c.l.scheme, but found no mention of
  ; this issue.  Apologies if I've missed something.

  (test-equal "ADR" (string-filter char-upper-case? "abrAcaDabRa"))

  (test-equal "abrcaaba" (string-remove char-upper-case? "abrAcaDabRa")))

(define (srfi-152-larceny-predicates)
  (test-assert (string-null? ""))
  (test-assert (not (string-null? "abc")))
  (test-equal #t (string-every (lambda (c) (if (char? c) c #f)) ""))
  (test-equal #\c (string-every (lambda (c) (if (char? c) c #f)) "abc"))
  (test-equal #f (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc"))
  (test-equal #\c (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 2))
  (test-equal #t (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 1 1))
  (test-equal #f (string-any (lambda (c) (if (char? c) c #f)) ""))
  (test-equal #\a (string-any (lambda (c) (if (char? c) c #f)) "abc"))
  (test-equal #\c (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc"))
  (test-equal #\c (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 2))
  (test-equal #f (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 0 2)))

(define (srfi-152-larceny-constructors)
  (test-equal ""
              (string-tabulate (lambda (i)
                                 (integer->char (+ i (char->integer #\a))))
                               0))
  (test-equal "abc"
              (string-tabulate (lambda (i)
                                 (integer->char (+ i (char->integer #\a))))
                               3))
  (test-equal "abc"
              (let ((p (open-input-string "abc")))
                (string-unfold eof-object?
                               values
                               (lambda (x) (read-char p))
                               (read-char p))))
  (test-equal "" (string-unfold null? car cdr '()))
  (test-equal "abc" (string-unfold null? car cdr (string->list "abc")))
  (test-equal "def" (string-unfold null? car cdr '() "def"))
  (test-equal "defabcG"
              (string-unfold null?
                             car
                             cdr
                             (string->list "abc")
                             "def"
                             (lambda (x) (and (null? x) "G"))))
  (test-equal "" (string-unfold-right null? car cdr '()))
  (test-equal "cba" (string-unfold-right null? car cdr (string->list "abc")))
  (test-equal "def" (string-unfold-right null? car cdr '() "def"))
  (test-equal "Gcbadef"
              (string-unfold-right null?
                                   car
                                   cdr
                                   (string->list "abc")
                                   "def"
                                   (lambda (x) (and (null? x) "G")))))

(define (srfi-152-larceny-conversion)
  (test-equal '() (string->list ""))
  (test-equal '() (string->list "" 0))
  (test-equal '() (string->list "" 0 0))
  (test-equal '(#\a #\b #\c) (string->list "abc"))
  (test-equal '() (string->list "abc" 3))
  (test-equal '(#\b #\c) (string->list "abc" 1 3))
  (test-equal '(#\b #\c)
              (string->list "abc"
                                    (dummy-index "abc" 1)
                                    (dummy-index "abc" 3)))
  (test-equal '#() (string->vector ""))
  (test-equal '#() (string->vector "" 0))
  (test-equal '#() (string->vector "" 0 0))
  (test-equal '#(#\a #\b #\c) (string->vector "abc"))
  (test-equal '#() (string->vector "abc" 3))
  (test-equal '#(#\b #\c) (string->vector "abc" 1 3))
  (test-equal '#(#\b #\c)
              (string->vector "abc"
                                    (dummy-index "abc" 1)
                                    (dummy-index "abc" 3)))
  (test-equal "" (reverse-list->string '()))
  (test-equal "cba" (reverse-list->string '(#\a #\b #\c)))
  (test-equal "" (string-join '()))
  (test-equal " ab cd  e f "
              (string-join '("" "ab" "cd" "" "e" "f" "")))
  (test-equal "" (string-join '() ""))
  (test-equal "abcdef"
              (string-join '("" "ab" "cd" "" "e" "f" "") ""))
  (test-equal "" (string-join '() "xyz"))
  (test-equal "xyzabxyzcdxyzxyzexyzfxyz"
              (string-join '("" "ab" "cd" "" "e" "f" "") "xyz"))
  (test-equal "" (string-join '() "" 'infix))
  (test-equal "abcdef"
              (string-join '("" "ab" "cd" "" "e" "f" "") "" 'infix))
  (test-equal "" (string-join '() "xyz" 'infix))
  (test-equal "xyzabxyzcdxyzxyzexyzfxyz"
              (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'infix))
  (test-error
               (string-join '() "" 'strict-infix))
  (test-equal "abcdef"
              (string-join '("" "ab" "cd" "" "e" "f" "") "" 'strict-infix))
  (test-error
               (string-join '() "xyz" 'strict-infix))
  (test-equal "xyzabxyzcdxyzxyzexyzfxyz"
              (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'strict-infix))
  (test-equal "" (string-join '() "" 'suffix))
  (test-equal "abcdef"
              (string-join '("" "ab" "cd" "" "e" "f" "") "" 'suffix))
  (test-equal "" (string-join '() "xyz" 'suffix))
  (test-equal "xyzabxyzcdxyzxyzexyzfxyzxyz"
              (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'suffix))
  (test-equal "" (string-join '() "" 'prefix))
  (test-equal "abcdef"
              (string-join '("" "ab" "cd" "" "e" "f" "") "" 'prefix))
  (test-equal "" (string-join '() "xyz" 'prefix))
  (test-equal "xyzxyzabxyzcdxyzxyzexyzfxyz"
              (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'prefix)))

(define (srfi-152-larceny-selection1)
  (test-equal #\a (string-ref "abc" 0))
  (test-equal #\c (string-ref "abc" 2))
  (test-equal #\a (string-ref "abc" (dummy-index "abc" 0)))
  (test-equal #\c (string-ref "abc" (dummy-index "abc" 2)))
  (test-equal "" (substring "" 0 0))
  (test-equal "" (substring "abc" 0 0))
  (test-equal "" (substring "abc" 3 3))
  (test-equal ABC (substring ABC 0 3))
  (test-equal ABC
                (substring ABC
                                   (dummy-index "abc" 0)
                                   (dummy-index "abc" 3)))
  (test-equal "b" (substring "abc" 1 2))
  (test-equal "" (string-copy ""))
  (test-equal "abc" (string-copy "abc"))
  (test-equal "" (string-copy "abc" 3))
  (test-equal "c" (string-copy "abc" 2))
  (test-equal "abc" (string-copy "abc" 0))
  (test-equal "b" (string-copy "abc" 1 2))
  (test-equal "" (string-copy "" 0 0))
  (test-equal "" (string-copy "abc" 0 0))
  (test-equal "" (string-copy "abc" 3 3))
  (test-equal "abc" (string-copy "abc" 0 3))
  (test-equal "b" (string-copy "abc" 1 2))
  (test-equal (substring ABC 1 2)
                (string-copy ABC
                                     (dummy-index "abc" 1)
                                     (dummy-index "abc" 2)))
  (test-equal "" (string-take "" 0))
  (test-equal "" (string-take "abcdef" 0))
  (test-equal "ab" (string-take "abcdef" 2))
  (test-equal "" (string-drop "" 0))
  (test-equal "abcdef" (string-drop "abcdef" 0))
  (test-equal "cdef" (string-drop "abcdef" 2))
  (test-equal "" (string-take-right "" 0))
  (test-equal "" (string-take-right "abcdef" 0))
  (test-equal "ef" (string-take-right "abcdef" 2))
  (test-equal "" (string-drop-right "" 0))
  (test-equal "abcdef" (string-drop-right "abcdef" 0))
  (test-equal "abcd" (string-drop-right "abcdef" 2))
  (test-equal "" (string-pad "" 0))
  (test-equal "     " (string-pad "" 5))
  (test-equal "  325" (string-pad "325" 5))
  (test-equal "71325" (string-pad "71325" 5))
  (test-equal "71325" (string-pad "8871325" 5))
  (test-equal "" (string-pad "" 0 #\*))
  (test-equal "*****" (string-pad "" 5 #\*))
  (test-equal "**325" (string-pad "325" 5 #\*))
  (test-equal "71325" (string-pad "71325" 5 #\*))
  (test-equal "71325" (string-pad "8871325" 5 #\*))
  (test-equal "" (string-pad "" 0 #\* 0))
  (test-equal "*****" (string-pad "" 5 #\* 0))
  (test-equal "**325" (string-pad "325" 5 #\* 0))
  (test-equal "71325" (string-pad "71325" 5 #\* 0))
  (test-equal "71325" (string-pad "8871325" 5 #\* 0))
  (test-equal "***25" (string-pad "325" 5 #\* 1))
  (test-equal "*1325" (string-pad "71325" 5 #\* 1))
  (test-equal "71325" (string-pad "8871325" 5 #\* 1))
  (test-equal "" (string-pad "" 0 #\* 0 0))
  (test-equal "*****" (string-pad "" 5 #\* 0 0))
  (test-equal "**325" (string-pad "325" 5 #\* 0 3))
  (test-equal "**713" (string-pad "71325" 5 #\* 0 3))
  (test-equal "**887" (string-pad "8871325" 5 #\* 0 3))
  (test-equal "***25" (string-pad "325" 5 #\* 1 3))
  (test-equal "**132" (string-pad "71325" 5 #\* 1 4))
  (test-equal "*8713" (string-pad "8871325" 5 #\* 1 5))
  (test-equal "" (string-pad-right "" 0))
  (test-equal "     " (string-pad-right "" 5))
  (test-equal "325  " (string-pad-right "325" 5))
  (test-equal "71325" (string-pad-right "71325" 5))
  (test-equal "88713" (string-pad-right "8871325" 5))
  (test-equal "" (string-pad-right "" 0 #\*))
  (test-equal "*****" (string-pad-right "" 5 #\*))
  (test-equal "325**" (string-pad-right "325" 5 #\*))
  (test-equal "71325" (string-pad-right "71325" 5 #\*))
  (test-equal "88713" (string-pad-right "8871325" 5 #\*))
  (test-equal "" (string-pad-right "" 0 #\* 0))
  (test-equal "*****" (string-pad-right "" 5 #\* 0))
  (test-equal "325**" (string-pad-right "325" 5 #\* 0))
  (test-equal "71325" (string-pad-right "71325" 5 #\* 0))
  (test-equal "88713" (string-pad-right "8871325" 5 #\* 0))
  (test-equal "25***" (string-pad-right "325" 5 #\* 1))
  (test-equal "1325*" (string-pad-right "71325" 5 #\* 1))
  (test-equal "87132" (string-pad-right "8871325" 5 #\* 1))
  (test-equal "" (string-pad-right "" 0 #\* 0 0))
  (test-equal "*****" (string-pad-right "" 5 #\* 0 0))
  (test-equal "325**" (string-pad-right "325" 5 #\* 0 3))
  (test-equal "713**" (string-pad-right "71325" 5 #\* 0 3))
  (test-equal "887**" (string-pad-right "8871325" 5 #\* 0 3))
  (test-equal "25***" (string-pad-right "325" 5 #\* 1 3))
  (test-equal "132**" (string-pad-right "71325" 5 #\* 1 4))
  (test-equal "8713*" (string-pad-right "8871325" 5 #\* 1 5))
  (test-equal "" (string-trim ""))
  (test-equal "a  b  c  " (string-trim "  a  b  c  "))
  (test-equal "" (string-trim "" char-whitespace?))
  (test-equal "a  b  c  " (string-trim "  a  b  c  " char-whitespace?))
  (test-equal "" (string-trim "  a  b  c  " char?))
  (test-equal "" (string-trim "" char-whitespace? 0))
  (test-equal "a  b  c  " (string-trim "  a  b  c  " char-whitespace? 0))
  (test-equal "" (string-trim "  a  b  c  " char? 0))
  (test-equal "b  c  " (string-trim "  a  b  c  " char-whitespace? 3))
  (test-equal "" (string-trim "  a  b  c  " char? 3))
  (test-equal "" (string-trim "  a  b  c  " char? 0 11))
  (test-equal "b  c  " (string-trim "  a  b  c  " char-whitespace? 3 11))
  (test-equal "" (string-trim "  a  b  c  " char? 3 11))
  (test-equal "" (string-trim "  a  b  c  " char? 0 8))
  (test-equal "b  " (string-trim "  a  b  c  " char-whitespace? 3 8))
  (test-equal "" (string-trim "  a  b  c  " char? 3 8))
  (test-equal "" (string-trim-right ""))
  (test-equal "  a  b  c" (string-trim-right "  a  b  c  "))
  (test-equal "" (string-trim-right "" char-whitespace?))
  (test-equal "  a  b  c" (string-trim-right "  a  b  c  " char-whitespace?))
  (test-equal "" (string-trim-right "  a  b  c  " char?))
  (test-equal "" (string-trim-right "" char-whitespace? 0))
  (test-equal "  a  b  c" (string-trim-right "  a  b  c  " char-whitespace? 0))
  (test-equal "" (string-trim-right "  a  b  c  " char? 0))
  (test-equal "  b  c" (string-trim-right "  a  b  c  " char-whitespace? 3))
  (test-equal "" (string-trim-right "  a  b  c  " char? 3))
  (test-equal "" (string-trim-right "  a  b  c  " char? 0 11))
  (test-equal "  b  c" (string-trim-right "  a  b  c  " char-whitespace? 3 11))
  (test-equal "" (string-trim-right "  a  b  c  " char? 3 11))
  (test-equal "" (string-trim-right "  a  b  c  " char? 0 8))
  (test-equal "  b" (string-trim-right "  a  b  c  " char-whitespace? 3 8))
  (test-equal "" (string-trim-right "  a  b  c  " char? 3 8))
  (test-equal "" (string-trim-both ""))
  (test-equal "a  b  c" (string-trim-both "  a  b  c  "))
  (test-equal "" (string-trim-both "" char-whitespace?))
  (test-equal "a  b  c" (string-trim-both "  a  b  c  " char-whitespace?))
  (test-equal "" (string-trim-both "  a  b  c  " char?))
  (test-equal "" (string-trim-both "" char-whitespace? 0))
  (test-equal "a  b  c" (string-trim-both "  a  b  c  " char-whitespace? 0))
  (test-equal "" (string-trim-both "  a  b  c  " char? 0))
  (test-equal "b  c" (string-trim-both "  a  b  c  " char-whitespace? 3))
  (test-equal "" (string-trim-both "  a  b  c  " char? 3))
  (test-equal "" (string-trim-both "  a  b  c  " char? 0 11))
  (test-equal "b  c" (string-trim-both "  a  b  c  " char-whitespace? 3 11))
  (test-equal "" (string-trim-both "  a  b  c  " char? 3 11))
  (test-equal "" (string-trim-both "  a  b  c  " char? 0 8))
  (test-equal "b" (string-trim-both "  a  b  c  " char-whitespace? 3 8))
  (test-equal "" (string-trim-both "  a  b  c  " char? 3 8)))

(define (srfi-152-larceny-selection2)
  (test-equal 0 (string-prefix-length "" ""))
  (test-equal 0 (string-prefix-length "" "aabbccddee"))
  (test-equal 0 (string-prefix-length "aisle" ""))
  (test-equal 0 (string-prefix-length "" "aabbccddee"))
  (test-equal 1 (string-prefix-length "aisle" "aabbccddee"))
  (test-equal 0 (string-prefix-length "bail" "aabbccddee"))
  (test-equal 4 (string-prefix-length "prefix" "preface"))
  (test-equal 0 (string-prefix-length "" "" 0))
  (test-equal 0 (string-prefix-length "" "aabbccddee" 0))
  (test-equal 0 (string-prefix-length "aisle" "" 0))
  (test-equal 1 (string-prefix-length "aisle" "aabbccddee" 0))
  (test-equal 0 (string-prefix-length "bail" "aabbccddee" 0))
  (test-equal 4 (string-prefix-length "prefix" "preface" 0))
  (test-equal 0 (string-prefix-length "aisle" "" 1))
  (test-equal 0 (string-prefix-length "aisle" "aabbccddee" 1))
  (test-equal 1 (string-prefix-length "bail" "aabbccddee" 1))
  (test-equal 0 (string-prefix-length "prefix" "preface" 1))
  (test-equal 0 (string-prefix-length "" "" 0 0))
  (test-equal 0 (string-prefix-length "" "aabbccddee" 0 0))
  (test-equal 0 (string-prefix-length "aisle" "" 0 4))
  (test-equal 1 (string-prefix-length "aisle" "aabbccddee" 0 4))
  (test-equal 0 (string-prefix-length "bail" "aabbccddee" 0 1))
  (test-equal 0 (string-prefix-length "aisle" "" 1 4))
  (test-equal 0 (string-prefix-length "aisle" "aabbccddee" 1 4))
  (test-equal 1 (string-prefix-length "bail" "aabbccddee" 1 4))
  (test-equal 0 (string-prefix-length "prefix" "preface" 1 5))
  (test-equal 0 (string-prefix-length "" "" 0 0 0))
  (test-equal 0 (string-prefix-length "" "aabbccddee" 0 0 0))
  (test-equal 0 (string-prefix-length "aisle" "" 0 4 0))
  (test-equal 0 (string-prefix-length "aisle" "aabbccddee" 0 4 2))
  (test-equal 1 (string-prefix-length "bail" "aabbccddee" 0 1 2))
  (test-equal 0 (string-prefix-length "prefix" "preface" 0 5 1))
  (test-equal 0 (string-prefix-length "aisle" "" 1 4 0))
  (test-equal 0 (string-prefix-length "aisle" "aabbccddee" 1 4 3))
  (test-equal 0 (string-prefix-length "bail" "aabbccddee" 1 4 3))
  (test-equal 3 (string-prefix-length "prefix" "preface" 1 5 1))
  (test-equal 0 (string-prefix-length "" "" 0 0 0 0))
  (test-equal 0 (string-prefix-length "" "aabbccddee" 0 0 0 0))
  (test-equal 0 (string-prefix-length "aisle" "" 0 4 0 0))
  (test-equal 0 (string-prefix-length "aisle" "aabbccddee" 0 4 2 10))
  (test-equal 1 (string-prefix-length "bail" "aabbccddee" 0 1 2 10))
  (test-equal 0 (string-prefix-length "prefix" "preface" 0 5 1 6))
  (test-equal 0 (string-prefix-length "aisle" "" 1 4 0 0))
  (test-equal 0 (string-prefix-length "aisle" "aabbccddee" 1 4 3 3))
  (test-equal 0 (string-prefix-length "bail" "aabbccddee" 1 4 3 6))
  (test-equal 3 (string-prefix-length "prefix" "preface" 1 5 1 7))
  (test-equal 0 (string-suffix-length "" ""))
  (test-equal 0 (string-suffix-length "" "aabbccddee"))
  (test-equal 0 (string-suffix-length "aisle" ""))
  (test-equal 0 (string-suffix-length "" "aabbccddee"))
  (test-equal 1 (string-suffix-length "aisle" "aabbccddee"))
  (test-equal 0 (string-suffix-length "bail" "aabbccddee"))
  (test-equal 3 (string-suffix-length "place" "preface"))
  (test-equal 0 (string-suffix-length "" "" 0))
  (test-equal 0 (string-suffix-length "" "aabbccddee" 0))
  (test-equal 0 (string-suffix-length "aisle" "" 0))
  (test-equal 1 (string-suffix-length "aisle" "aabbccddee" 0))
  (test-equal 0 (string-suffix-length "bail" "aabbccddee" 0))
  (test-equal 3 (string-suffix-length "place" "preface" 0))
  (test-equal 0 (string-suffix-length "aisle" "" 1))
  (test-equal 1 (string-suffix-length "aisle" "aabbccddee" 1))
  (test-equal 0 (string-suffix-length "bail" "aabbccddee" 1))
  (test-equal 3 (string-suffix-length "place" "preface" 1))
  (test-equal 0 (string-suffix-length "" "" 0 0))
  (test-equal 0 (string-suffix-length "" "aabbccddee" 0 0))
  (test-equal 0 (string-suffix-length "aisle" "" 0 4))
  (test-equal 0 (string-suffix-length "aisle" "aabbccddee" 0 4))
  (test-equal 0 (string-suffix-length "bail" "aabbccddee" 0 1))
  (test-equal 0 (string-suffix-length "aisle" "" 1 4))
  (test-equal 0 (string-suffix-length "aisle" "aabbccddee" 1 4))
  (test-equal 1 (string-suffix-length "aisle" "aabbccddee" 1 5))
  (test-equal 0 (string-suffix-length "bail" "aabbccddee" 1 4))
  (test-equal 3 (string-suffix-length "place" "preface" 1 5))
  (test-equal 0 (string-suffix-length "" "" 0 0 0))
  (test-equal 0 (string-suffix-length "" "aabbccddee" 0 0 0))
  (test-equal 0 (string-suffix-length "aisle" "" 0 4 0))
  (test-equal 0 (string-suffix-length "aisle" "aabbccddee" 0 4 2))
  (test-equal 0 (string-suffix-length "bail" "aabbccddee" 0 1 2))
  (test-equal 3 (string-suffix-length "place" "preface" 0 5 1))
  (test-equal 0 (string-suffix-length "aisle" "" 1 4 0))
  (test-equal 0 (string-suffix-length "aisle" "aabbccddee" 1 4 3))
  (test-equal 0 (string-suffix-length "bail" "aabbccddee" 1 4 3))
  (test-equal 3 (string-suffix-length "place" "preface" 1 5 1))
  (test-equal 0 (string-suffix-length "" "" 0 0 0 0))
  (test-equal 0 (string-suffix-length "" "aabbccddee" 0 0 0 0))
  (test-equal 0 (string-suffix-length "aisle" "" 0 4 0 0))
  (test-equal 1 (string-suffix-length "aisle" "aabbccddee" 0 5 2 10))
  (test-equal 1 (string-suffix-length "bail" "aabbccddee" 0 1 2 4))
  (test-equal 0 (string-suffix-length "place" "preface" 0 5 1 6))
  (test-equal 2 (string-suffix-length "place" "preface" 0 4 1 6))
  (test-equal 0 (string-suffix-length "aisle" "" 1 4 0 0))
  (test-equal 0 (string-suffix-length "aisle" "aabbccddee" 1 4 3 3))
  (test-equal 0 (string-suffix-length "bail" "aabbccddee" 1 4 3 6))
  (test-equal 3 (string-suffix-length "place" "preface" 1 5 1 7))
  (test-equal #t (string-prefix? "" ""))
  (test-equal #t (string-prefix? "" "abc"))
  (test-equal #t (string-prefix? "a" "abc"))
  (test-equal #f (string-prefix? "c" "abc"))
  (test-equal #t (string-prefix? "ab" "abc"))
  (test-equal #f (string-prefix? "ac" "abc"))
  (test-equal #t (string-prefix? "abc" "abc"))
  (test-equal #t (string-suffix? "" ""))
  (test-equal #t (string-suffix? "" "abc"))
  (test-equal #f (string-suffix? "a" "abc"))
  (test-equal #t (string-suffix? "c" "abc"))
  (test-equal #f (string-suffix? "ac" "abc"))
  (test-equal #t (string-suffix? "bc" "abc"))
  (test-equal #t (string-suffix? "abc" "abc"))
  (test-equal #t (string-prefix? "" "" 0))
  (test-equal #t (string-prefix? "" "abc" 0))
  (test-equal #t (string-prefix? "a" "abc" 0))
  (test-equal #f (string-prefix? "c" "abc" 0))
  (test-equal #t (string-prefix? "ab" "abc" 0))
  (test-equal #f (string-prefix? "ac" "abc" 0))
  (test-equal #t (string-prefix? "abc" "abc" 0))
  (test-equal #t (string-suffix? "" "" 0))
  (test-equal #t (string-suffix? "" "abc" 0))
  (test-equal #f (string-suffix? "a" "abc" 0))
  (test-equal #t (string-suffix? "c" "abc" 0))
  (test-equal #f (string-suffix? "ac" "abc" 0))
  (test-equal #t (string-suffix? "bc" "abc" 0))
  (test-equal #t (string-suffix? "abc" "abc" 0))
  (test-equal #t (string-prefix? "ab" "abc" 2))
  (test-equal #t (string-prefix? "ac" "abc" 2))
  (test-equal #f (string-prefix? "abc" "abc" 2))
  (test-equal #t (string-suffix? "ac" "abc" 2))
  (test-equal #t (string-suffix? "bc" "abc" 2))
  (test-equal #t (string-suffix? "abc" "abc" 2))
  (test-equal #t (string-prefix? "" "" 0 0))
  (test-equal #t (string-prefix? "" "abc" 0 0))
  (test-equal #t (string-prefix? "a" "abc" 0 0))
  (test-equal #f (string-prefix? "c" "abc" 0 1))
  (test-equal #t (string-prefix? "ab" "abc" 0 1))
  (test-equal #t (string-prefix? "ab" "abc" 0 2))
  (test-equal #f (string-prefix? "ac" "abc" 0 2))
  (test-equal #t (string-prefix? "abc" "abc" 0 3))
  (test-equal #t (string-suffix? "" "" 0 0))
  (test-equal #t (string-suffix? "" "abc" 0 0))
  (test-equal #f (string-suffix? "a" "abc" 0 1))
  (test-equal #t (string-suffix? "c" "abc" 0 1))
  (test-equal #t (string-suffix? "ac" "abc" 1 2))
  (test-equal #f (string-suffix? "ac" "abc" 0 2))
  (test-equal #t (string-suffix? "bc" "abc" 0 2))
  (test-equal #t (string-suffix? "abc" "abc" 0 3))
  (test-equal #t (string-prefix? "ab" "abc" 2 2))
  (test-equal #t (string-prefix? "ac" "abc" 2 2))
  (test-equal #f (string-prefix? "abc" "abc" 2 3))
  (test-equal #t (string-suffix? "ac" "abc" 2 2))
  (test-equal #t (string-suffix? "bc" "abc" 2 2))
  (test-equal #t (string-suffix? "abc" "abc" 2 3))
  (test-equal #t (string-prefix? "" "" 0 0 0))
  (test-equal #t (string-prefix? "" "abc" 0 0 0))
  (test-equal #t (string-prefix? "a" "abc" 0 0 0))
  (test-equal #f (string-prefix? "c" "abc" 0 1 0))
  (test-equal #t (string-prefix? "ab" "abc" 0 1 0))
  (test-equal #t (string-prefix? "ab" "abc" 0 2 0))
  (test-equal #f (string-prefix? "ac" "abc" 0 2 0))
  (test-equal #t (string-prefix? "abc" "abc" 0 3 0))
  (test-equal #t (string-suffix? "" "" 0 0 0))
  (test-equal #t (string-suffix? "" "abc" 0 0 0))
  (test-equal #f (string-suffix? "a" "abc" 0 1 0))
  (test-equal #t (string-suffix? "c" "abc" 0 1 0))
  (test-equal #t (string-suffix? "ac" "abc" 1 2 0))
  (test-equal #f (string-suffix? "ac" "abc" 0 2 0))
  (test-equal #t (string-suffix? "bc" "abc" 0 2 0))
  (test-equal #t (string-suffix? "abc" "abc" 0 3 0))
  (test-equal #t (string-prefix? "ab" "abc" 2 2 0))
  (test-equal #t (string-prefix? "ac" "abc" 2 2 0))
  (test-equal #f (string-prefix? "abc" "abc" 2 3 0))
  (test-equal #t (string-suffix? "ac" "abc" 2 2 0))
  (test-equal #t (string-suffix? "bc" "abc" 2 2 0))
  (test-equal #t (string-suffix? "abc" "abc" 2 3 0))
  (test-equal #t (string-prefix? "" "abc" 0 0 1))
  (test-equal #t (string-prefix? "a" "abc" 0 0 1))
  (test-equal #t (string-prefix? "c" "abc" 0 1 2))
  (test-equal #f (string-prefix? "ab" "abc" 0 1 2))
  (test-equal #f (string-prefix? "ab" "abc" 0 2 1))
  (test-equal #f (string-prefix? "ac" "abc" 0 2 1))
  (test-equal #f (string-prefix? "abc" "abc" 0 3 1))
  (test-equal #f (string-suffix? "a" "abc" 0 1 2))
  (test-equal #t (string-suffix? "c" "abc" 0 1 1))
  (test-equal #t (string-suffix? "ac" "abc" 1 2 2))
  (test-equal #t (string-suffix? "bc" "abc" 0 2 1))
  (test-equal #f (string-suffix? "bc" "abc" 0 2 2))
  (test-equal #t (string-prefix? "" "" 0 0 0 0))
  (test-equal #t (string-prefix? "" "abc" 0 0 0 3))
  (test-equal #t (string-prefix? "a" "abc" 0 0 0 3))
  (test-equal #f (string-prefix? "c" "abc" 0 1 0 3))
  (test-equal #t (string-prefix? "ab" "abc" 0 1 0 3))
  (test-equal #t (string-prefix? "ab" "abc" 0 2 0 3))
  (test-equal #f (string-prefix? "ac" "abc" 0 2 0 3))
  (test-equal #t (string-prefix? "abc" "abc" 0 3 0 3))
  (test-equal #t (string-suffix? "" "abc" 0 0 0 3))
  (test-equal #f (string-suffix? "a" "abc" 0 1 0 3))
  (test-equal #t (string-suffix? "c" "abc" 0 1 0 3))
  (test-equal #t (string-suffix? "ac" "abc" 1 2 0 3))
  (test-equal #f (string-suffix? "ac" "abc" 0 2 0 3))
  (test-equal #t (string-suffix? "bc" "abc" 0 2 0 3))
  (test-equal #t (string-suffix? "abc" "abc" 0 3 0 3))
  (test-equal #t (string-prefix? "ab" "abc" 2 2 0 3))
  (test-equal #t (string-prefix? "ac" "abc" 2 2 0 3))
  (test-equal #f (string-prefix? "abc" "abc" 2 3 0 3))
  (test-equal #t (string-suffix? "ac" "abc" 2 2 0 3))
  (test-equal #t (string-suffix? "bc" "abc" 2 2 0 3))
  (test-equal #t (string-suffix? "abc" "abc" 2 3 0 3))
  (test-equal #t (string-prefix? "" "abc" 0 0 1 3))
  (test-equal #t (string-prefix? "a" "abc" 0 0 1 3))
  (test-equal #t (string-prefix? "c" "abc" 0 1 2 3))
  (test-equal #f (string-prefix? "ab" "abc" 0 1 2 3))
  (test-equal #f (string-prefix? "ab" "abc" 0 2 1 3))
  (test-equal #f (string-prefix? "ac" "abc" 0 2 1 3))
  (test-equal #f (string-prefix? "abc" "abc" 0 3 1 3))
  (test-equal #f (string-suffix? "a" "abc" 0 1 2 3))
  (test-equal #t (string-suffix? "c" "abc" 0 1 1 3))
  (test-equal #t (string-suffix? "ac" "abc" 1 2 2 3))
  (test-equal #t (string-suffix? "bc" "abc" 0 2 1 3))
  (test-equal #f (string-suffix? "bc" "abc" 0 2 2 3))
  (test-equal #t (string-prefix? "" "abc" 0 0 0 2))
  (test-equal #t (string-prefix? "a" "abc" 0 0 0 2))
  (test-equal #f (string-prefix? "c" "abc" 0 1 0 2))
  (test-equal #t (string-prefix? "ab" "abc" 0 1 0 2))
  (test-equal #f (string-prefix? "abc" "abc" 0 3 0 2))
  (test-equal #t (string-suffix? "" "abc" 0 0 0 2))
  (test-equal #f (string-suffix? "c" "abc" 0 1 0 2))
  (test-equal #f (string-suffix? "ac" "abc" 1 2 0 2)))

(define (srfi-152-larceny-searching)
  (test-equal #f
         (dummy-index ""
                               (string-index "" char?)))
  (test-equal 0
         (dummy-index "abcdef"
                               (string-index "abcdef" char?)))
  (test-equal 4
         (dummy-index "abcdef"
                               (string-index "abcdef"
                                             (lambda (c) (char>? c #\d)))))
  (test-equal #f
         (dummy-index "abcdef"
                               (string-index "abcdef" char-whitespace?)))
  (test-equal #f
         (dummy-index "abcdef"
                               (string-index-right "" char?)))
  (test-equal 5
         (dummy-index "abcdef"
                               (string-index-right "abcdef" char?)))
  (test-equal 5
         (dummy-index "abcdef"
                               (string-index-right "abcdef"
                                                   (lambda (c) (char>? c #\d)))))
  (test-equal #f
         (dummy-index "abcdef"
                               (string-index-right "abcdef" char-whitespace?)))
  (test-equal #f
         (dummy-index "" (string-skip "" string?)))
  (test-equal 0
         (dummy-index "abcdef"
                               (string-skip "abcdef" string?)))
  (test-equal 4
         (dummy-index "abcdef"
                               (string-skip "abcdef"
                                            (lambda (c) (char<=? c #\d)))))
  (test-equal #f
         (dummy-index "abcdef"
                               (string-skip "abcdef" char?)))
  (test-equal #f
         (dummy-index "" (string-skip-right "" string?)))
  (test-equal 5
         (dummy-index "abcdef"
                               (string-skip-right "abcdef" string?)))
  (test-equal 5
         (dummy-index "abcdef"
                               (string-skip-right "abcdef"
                                                  (lambda (c) (char<=? c #\d)))))
  (test-equal #f
         (dummy-index "abcdef"
                               (string-skip-right "abcdef" char?)))
  (test-equal 2
         (dummy-index "abcdef"
                               (string-index "abcdef" char? 2)))
  (test-equal 4
         (dummy-index "abcdef"
                               (string-index "abcdef"
                                             (lambda (c) (char>? c #\d)) 2)))
  (test-equal #f
         (dummy-index "abcdef"
                               (string-index "abcdef" char-whitespace? 2)))
  (test-equal 5
         (dummy-index "abcdef"
                               (string-index-right "abcdef" char? 2)))
  (test-equal 5
         (dummy-index "abcdef"
                               (string-index-right "abcdef"
                                                   (lambda (c)
                                                     (char>? c #\d)) 2)))
  (test-equal #f
         (dummy-index "abcdef"
                               (string-index-right "abcdef" char-whitespace? 2)))
  (test-equal 2
         (dummy-index "abcdef"
                               (string-skip "abcdef" string? 2)))
  (test-equal 4
         (dummy-index "abcdef"
                               (string-skip "abcdef"
                                            (lambda (c)
                                              (char<=? c #\d)) 2)))
  (test-equal #f
         (dummy-index "abcdef"
                               (string-skip "abcdef" char? 2)))
  (test-equal 5
         (dummy-index "abcdef"
                               (string-skip-right "abcdef" string? 2)))
  (test-equal 5
         (dummy-index "abcdef"
                               (string-skip-right "abcdef"
                                                  (lambda (c)
                                                    (char<=? c #\d)) 2)))
  (test-equal #f
         (dummy-index "abcdef"
                               (string-skip-right "abcdef" char? 2)))
  (test-equal 2
         (dummy-index "abcdef"
                               (string-index "abcdef" char? 2 5)))
  (test-equal 4
         (dummy-index "abcdef"
                               (string-index "abcdef"
                                             (lambda (c) (char>? c #\d)) 2 5)))
  (test-equal #f
         (dummy-index "abcdef"
                               (string-index "abcdef" char-whitespace? 2 5)))
  (test-equal 4
         (dummy-index "abcdef"
                               (string-index-right "abcdef" char? 2 5)))
  (test-equal 4
         (dummy-index "abcdef"
                               (string-index-right "abcdef"
                                                   (lambda (c)
                                                     (char>? c #\d)) 2 5)))
  (test-equal #f
         (dummy-index "abcdef"
                               (string-index-right "abcdef"
                                                   char-whitespace? 2 5)))
  (test-equal 2
         (dummy-index "abcdef"
                               (string-skip "abcdef" string? 2 5)))
  (test-equal 4
         (dummy-index "abcdef"
                               (string-skip "abcdef"
                                            (lambda (c) (char<=? c #\d)) 2 5)))
  (test-equal #f
         (dummy-index "abcdef"
                               (string-skip "abcdef" char? 2 5)))
  (test-equal 4
         (dummy-index "abcdef"
                               (string-skip-right "abcdef" string? 2 5)))
  (test-equal 4
         (dummy-index "abcdef"
                               (string-skip-right "abcdef"
                                                  (lambda (c)
                                                    (char<=? c #\d)) 2 5)))
  (test-equal #f
         (dummy-index "abcdef"
                               (string-skip-right "abcdef" char? 2 5)))
  (test-equal 0
            (dummy-index ""
                                  (string-contains "" "")))
  (test-equal 0
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo" "")))
  (test-equal 0
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo" "a")))
  (test-equal 5
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo" "ff")))
  (test-equal 4
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo" "eff")))
  (test-equal 8
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo" "foo")))
  (test-equal #f
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo" "efffoo")))
  (test-equal 0
            (dummy-index ""
                                  (string-contains-right "" "")))
  (test-equal 11
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo" "")))
  (test-equal 0
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo" "a")))
  (test-equal 7
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo" "ff")))
  (test-equal 4
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo" "eff")))
  (test-equal 8
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo" "foo")))
  (test-equal #f
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "efffoo")))
  (test-equal 0
            (dummy-index ""
                                  (string-contains "" "" 0)))
  (test-equal 2
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo" "" 2)))
  (test-equal #f
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo" "a" 2)))
  (test-equal 5
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo" "ff" 2)))
  (test-equal 4
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo" "eff" 2)))
  (test-equal 8
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo" "foo" 2)))
  (test-equal #f
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo" "efffoo" 2)))
  (test-equal 0
            (dummy-index ""
                                  (string-contains-right "" "" 0)))
  (test-equal 11
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "" 2)))
  (test-equal #f
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "a" 2)))
  (test-equal 7
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "ff" 2)))
  (test-equal 4
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "eff" 2)))
  (test-equal 8
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "foo" 2)))
  (test-equal #f
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "efffoo" 2)))
  (test-equal 0
            (dummy-index ""
                                  (string-contains "" "" 0 0)))
  (test-equal 2
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "" 2 10)))
  (test-equal #f
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "a" 2 10)))
  (test-equal 5
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "ff" 2 10)))
  (test-equal 4
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "eff" 2 10)))
  (test-equal #f
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "foo" 2 10)))
  (test-equal #f
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "efffoo" 2 10)))
  (test-equal 0
            (dummy-index ""
                                  (string-contains-right "" "" 0 0)))
  (test-equal 10
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "" 2 10)))
  (test-equal #f
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "a" 2 10)))
  (test-equal 7
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "ff" 2 10)))
  (test-equal 4
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "eff" 2 10)))
  (test-equal #f
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "foo" 2 10)))
  (test-equal #f
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "efffoo" 2 10)))
  (test-equal 0
            (dummy-index ""
                                  (string-contains "" "" 0 0 0)))
  (test-equal 2
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "" 2 10 0)))
  (test-equal 2
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "a" 2 10 1)))
  (test-equal 5
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "ff" 2 10 1)))
  (test-equal 5
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "eff" 2 10 1)))
  (test-equal #f
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "foo" 2 10 1)))
  (test-equal #f
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "efffoo" 2 10 1)))
  (test-equal 0
            (dummy-index ""
                                  (string-contains-right "" "" 0 0 0)))
  (test-equal 10
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "" 2 10 0)))
  (test-equal 10
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "a" 2 10 1)))
  (test-equal 8
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "ff" 2 10 1)))
  (test-equal 7
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "eff" 2 10 1)))
  (test-equal #f
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "foo" 2 10 1)))
  (test-equal #f
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "efffoo" 2 10 1)))
  (test-equal 0
            (dummy-index ""
                                  (string-contains "" "" 0 0 0 0)))
  (test-equal 2
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "" 2 10 0 0)))
  (test-equal 2
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "a" 2 10 1 1)))
  (test-equal 5
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "ff" 2 10 1 2)))
  (test-equal 5
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "eff" 2 10 1 2)))
  (test-equal 9
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "foo" 2 10 1 2)))
  (test-equal 4
            (dummy-index "abcdeffffoo"
                                  (string-contains "abcdeffffoo"
                                                   "efffoo" 2 10 0 2)))
  (test-equal 0
            (dummy-index ""
                                  (string-contains-right "" "" 0 0 0 0)))
  (test-equal 10
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "" 2 10 0 0)))
  (test-equal 10
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "a" 2 10 1 1)))
  (test-equal 8
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "ff" 2 10 1 2)))
  (test-equal 8
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "eff" 2 10 1 2)))
  (test-equal 9
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "foo" 2 10 1 2)))
  (test-equal 7
            (dummy-index "abcdeffffoo"
                                  (string-contains-right "abcdeffffoo"
                                                         "efffoo" 2 10 1 3))))

(define (srfi-152-larceny-wholestring)
  (test-equal "" (string-concatenate '()))
  (test-equal "abcdef" (string-concatenate '("" "a" "bcd" "" "ef" "" "")))
  (test-equal "" (string-concatenate-reverse '()))
  (test-equal "efbcda"
                (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "")))
  (test-equal "huh?" (string-concatenate-reverse '() "huh?"))
  (test-equal "efbcdaxy"
                (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "xy"))
  (test-equal "huh" (string-concatenate-reverse '() "huh?" 3))
  (test-equal "efbcdax"
                (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "x" 1))
  (test-equal 8
         (string-fold (lambda (c count)
                        (if (char-whitespace? c)
                            (+ count 1)
                            count))
                      0
                      " ...a couple of spaces in this one... "))
  (test-equal 7
         (string-fold (lambda (c count)
                        (if (char-whitespace? c)
                            (+ count 1)
                            count))
                      0
                      " ...a couple of spaces in this one... "
                      1))
  (test-equal 6
         (string-fold (lambda (c count)
                        (if (char-whitespace? c)
                            (+ count 1)
                            count))
                      0
                      " ...a couple of spaces in this one... "
                      1
                      32))
  (test-equal (string->list "abcdef")
              (string-fold-right cons '() "abcdef"))
  (test-equal (string->list "def")
              (string-fold-right cons '() "abcdef" 3))
  (test-equal (string->list "cde")
              (string-fold-right cons '() "abcdef" 2 5))
  (test-equal "aabraacaadaabraa"
                (let* ((s "abracadabra")
                       (ans-len (string-fold (lambda (c sum)
                                               (+ sum (if (char=? c #\a) 2 1)))
                                             0 s))
                       (ans (make-string ans-len)))
                  (string-fold (lambda (c i)
                                 (let ((i (if (char=? c #\a)
                                              (begin (string-set! ans i #\a)
                                                     (+ i 1))
                                                     i)))
                                   (string-set! ans i c)
                               (+ i 1)))
                               0 s)
                  ans))
  (test-equal '(101 100 99 98 97)
              (let ((s "abcde") (v '()))
                (string-for-each
                 (lambda (char)
                   (set! v (cons (char->integer char) v)))
                 s)
                v))
  (test-equal "cdefabcdefabcd"
                (string-replicate "abcdef" -4 10))
  (test-equal "bcdefbcdefbcd"
                (string-replicate "abcdef" 90 103 1))
  (test-equal "ecdecdecde"
                (string-replicate "abcdef" -13 -3 2 5))
  (test-equal 6 (string-count "abcdef" char?))
  (test-equal 4 (string-count "counting  whitespace, again " char-whitespace? 5))
  (test-equal 3 (string-count "abcdefwxyz"
                         (lambda (c) (odd? (char->integer c)))
                         2 8))
  (test-equal "It's lots of fun to code it up in Scheme."
                (string-replace "It's easy to code it up in Scheme."
                                "lots of fun"
                                5 9))
  (test-equal "The miserable perl programmer endured daily ridicule."
                (string-replace "The TCL programmer endured daily ridicule."
                                "another miserable perl drone"
                                4 7 8 22))
  (test-equal "It's really easy to code it up in Scheme."
                (string-replace "It's easy to code it up in Scheme."
                                "really "
                                5 5))
  (test-assert (null? (string-split "" "")))
  (test-equal '("a" "b" "c") (string-split "abc" ""))
  (test-equal '() (string-split "" "" 'infix))
  (test-equal '("a" "b" "c") (string-split "abc" "" 'infix))
  (test-equal '("a" "b" "c") (string-split "abc" "" 'strict-infix))
  (test-equal '() (string-split "" "" 'prefix))
  (test-equal '("a" "b" "c") (string-split "abc" "" 'prefix))
  (test-equal '() (string-split "" "" 'suffix))
  (test-equal '("a" "b" "c") (string-split "abc" "" 'suffix))
  (test-equal '() (string-split "" "" 'infix #f))
  (test-equal '("a" "b" "c") (string-split "abc" "" 'infix #f))
  (test-error
               (string-split "" "" 'strict-infix))
  (test-equal '("a" "b" "c") (string-split "abc" "" 'strict-infix 3))
  (test-equal '() (string-split "" "" 'prefix 3))
  (test-equal '("a" "b" "c") (string-split "abc" "" 'prefix 3))
  (test-equal '() (string-split "" "" 'suffix 3))
  (test-equal '("a" "b" "c") (string-split "abc" "" 'suffix 3))
  (test-equal '("b" "c") (string-split "abc" "" 'strict-infix 3 1))
  (test-equal '() (string-split "" "" 'prefix 3 0))
  (test-equal '("b" "c") (string-split "abc" "" 'prefix 3 1))
  (test-equal '("b") (string-split "abc" "" 'strict-infix 3 1 2))
  (test-equal '() (string-split "" "" 'prefix 3 0 0))
  (test-equal '("b") (string-split "abc" "" 'prefix 3 1 2))
  (test-equal '() (string-split "" "" 'suffix 3 0 0))
  (test-equal '("b") (string-split "abc" "" 'suffix 3 1 2))
  (test-equal "aiueaaaoi"
                (string-filter (lambda (c) (memv c (string->list "aeiou")))
                               "What is number, that man may know it?"))
  (test-equal "And wmn, tht sh my knw nmbr?"
                (string-remove (lambda (c) (memv c (string->list "aeiou")))
                               "And woman, that she may know number?"))
  (test-equal "iueaaaoi"
                (string-filter (lambda (c) (memv c (string->list "aeiou")))
                               "What is number, that man may know it?"
                               4))
  (test-equal "mn, tht sh my knw nmbr?"
                (string-remove (lambda (c) (memv c (string->list "aeiou")))
                               "And woman, that she may know number?"
                               6))
  (test-equal "aaao"
                (string-filter (lambda (c) (memv c (string->list "aeiou")))
                               "What is number, that man may know it?"
                               16 32))
  (test-equal "And woman, that sh may know"
                (string-remove (lambda (c) (memv c (string->list "eiu")))
                               "And woman, that she may know number?"
                               0 28)))

(define (srfi-152-residual)
  (test-equal #t (string? "abc"))
  (test-equal #f (string? 32))
  (test-equal "$$$" (make-string 3 #\$))
  (test-equal "$$$" (string #\$ #\$ #\$))
  (test-equal '(#\b #\c) (string->list "abcde" 1 3))
  (test-equal "abcde" (list->string '(#\a #\b #\c #\d #\e)))
  (test-equal "abcde" (vector->string '#(#\a #\b #\c #\d #\e)))
  (test-equal '("12345" "abcde")
    (call-with-values (lambda () (string-span "12345abcde" char-numeric?)) list))
  (test-equal '("12345" "abcde")
    (call-with-values (lambda () (string-break "12345abcde" char-alphabetic?)) list))
  (test-equal "abcde" (string-take-while "abcde12345" char-alphabetic?))
  (test-equal "abcde" (string-take-while-right "12345abcde" char-alphabetic?))
  (test-equal "abcde" (string-drop-while "   abcde" char-whitespace?))
  (test-equal "abcde" (string-drop-while-right "abcde  " char-whitespace?))
  (test-equal 5 (string-length "abcde"))
  (test-equal "ab!"
    (let ((abc (string-copy "abc")))
      (string-set! abc 2 #\!)
      abc))
  (test-equal "ab!"
    (let ((abc (string-copy "abc")))
      (string-set! abc 2 #\!)
      abc))
  (test-equal "!!!"
    (let ((abc (string-copy "abc")))
      (string-fill! abc #\!)
      abc))
  (test-equal "a!c"
    (let ((abc (string-copy "abc")))
      (string-fill! abc #\! 1 2)
      abc))

  (test-equal '("x" "")
    (string-split "x\n" "\n"))
  (test-equal '("x")
    (string-split "x\n" "\n" 'suffix))
  (test-equal '("" "x")
    (string-split "\nx" "\n"))
  (test-equal '("x" "" "y")
    (string-split "x\n\ny" "\n")))

(test-group "Strings"

  (test-group "srfi-152:gauche"
    (test-group "srfi-152:gauche:predicates"
      (srfi-152-gauche-predicates))
    (test-group "srfi-152:gauche:constructors"
      (srfi-152-gauche-constructors))
    (test-group "srfi-152:gauche:selectors"
      (srfi-152-gauche-selectors))
    (test-group "srfi-152:gauche:replacement"
      (srfi-152-gauche-replacement))
    #;(test-group "srfi-152:extended-comparisons"
        (test-equal "base cases for extended string comparisons"
          '(#t #t #t #t #t #t #t #t #t #t)
          (map (lambda (f) (and (f) (f "foo")))
               (list string=? string<? string>? string<=? string>=?
                     string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?))))
    (test-group "srfi-152:gauche:comparison"
      (srfi-152-gauche-comparison))
    (test-group "srfi-152:gauche:presuffixes"
      (srfi-152-gauche-presuffixes))
    (test-group "srfi-152:gauche:searching"
      (srfi-152-gauche-searching))
    (test-group "srfi-152:gauche:append"
      (srfi-152-gauche-append))
    (test-group "srfi-152:gauche:foldmap"
      (srfi-152-gauche-foldmap))
    (test-group "srfi-152:gauche:replisplit"
      (srfi-152-gauche-replisplit))
    (test-group "srfi-152:gauche:regression"
      (srfi-152-gauche-regression)))

  (test-group "srfi-152:larceny"
    (test-group "srfi-152:larceny:predicates"
      (srfi-152-larceny-predicates))
    (test-group "srfi-152:larceny:constructors"
      (srfi-152-larceny-constructors))
    (test-group "srfi-152:larceny:conversion"
      (srfi-152-larceny-conversion))
    (test-group "srfi-152:larceny:selection"
      (srfi-152-larceny-selection1)
      (srfi-152-larceny-selection2))
    (test-group "srfi-152:larceny:searching"
      (srfi-152-larceny-searching))
    (test-group "srfi-152:larceny:wholestring"
      (srfi-152-larceny-wholestring)))

  (test-group "srfi-152:residual"
    (srfi-152-residual)))
