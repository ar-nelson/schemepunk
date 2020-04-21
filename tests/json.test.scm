(import (scheme base)
        (schemepunk syntax)
        (schemepunk json)
        (schemepunk test))

(test-group "JSON Reader"
  (test "reads keywords"
    (assert-equal (string->json "null") 'null)
    (assert-equal (string->json "true") 'true)
    (assert-equal (string->json "false") 'false))

  (test "reads numbers"
    (assert-equal (string->json "0") 0)
    (assert-equal (string->json "0.1") 0.1)
    (assert-equal (string->json "1") 1)
    (assert-equal (string->json "1.5") 1.5)
    (assert-equal (string->json ".5") 0.5)
    (assert-equal (string->json "-.5") -0.5)
    (assert-equal (string->json "-1") -1)
    (assert-equal (string->json "1e3") 1000.0))

  (test "reads strings"
    (assert-equal (string->json "\"\"") "")
    (assert-equal (string->json "\"foo\"") "foo")
    (assert-equal
      (string->json "\"foo \\\\\\/\\'\\\"\\n\\r\\t\\b\\v\\f\\u0020\"")
      (string-append "foo \\/'\"\n\r\t\b" (string #\x0b #\x0c) " "))
    (assert-equal (string->json "\"={foo}\"") "={foo}"))

  (test "reads Unicode in strings"
    (assert-equal (string->json "\"λ\"") "λ"))

  (test "reads arrays"
    (assert-equal (string->json "[]") #())
    (assert-equal (string->json "[1, 2, 3]") #(1 2 3))
    (assert-equal
      (string->json "[[1, 2] ,[true, \"]foo[\"], [[]]]")
      ; Use vector instead of #() because of a Sagittarius bug
      (vector #(1 2) (vector 'true "]foo[") #(#()))))

  (test "reads objects"
    (assert-equal (string->json "{}") '())
    (assert-equal (string->json "{\"a\": 1, \"b\": 2, \"c\": 3}")
                  '(("c" . 3) ("b" . 2) ("a" . 1))))

  (test "reads nested objects"
    (assert-equal (string->json "{\"a\":{},\"b\":{}}")
                  '(("b" . ()) ("a" . ())))
    (assert-equal (string->json "{\"a\":{\"b\":\"c\"}}")
                  '(("a" . (("b" . "c")))))
    (assert-equal (string->json "{\"a\":{},\"b\":{\"\":\"foo\"},\"c\":{\"d\":{\"e\":\"f\"}}}")
                  '(("c" . (("d" . (("e" . "f")))))
                    ("b" . (("" . "foo")))
                    ("a" . ()))))

  (test "reads nested arrays and objects"
    (assert-equal (string->json "[[{\"\":[{\"\":[]}]}]]")
                  #(#((("" . #((("" . #()))))))))))

(test-group "JSON Writer"
  (test "writes primitives"
    (assert-equal (json->string 'null) "null")
    (assert-equal (json->string 'false) "false")
    (assert-equal (json->string 'true) "true")
    (assert-equal (json->string 91) "91")
    (assert-equal (json->string "foo") "\"foo\""))

  (test "writes floats in a consistent representation"
    (assert-equal (json->string 1.5) "1.5")
    (assert-equal (json->string 0.01) ".01")
    (assert-equal (json->string -0.1) "-.1")
    (assert-equal (json->string 1/2) ".5"))

  (test "writes strings with escapes"
    (assert-equal (json->string (string-append "foo \\/'\"\n\r\t\b" (string #\x0b #\x0c)))
                  "\"foo \\\\/'\\\"\\n\\r\\t\\b\\v\\f\""))

  (test "writes arrays"
    (assert-equal (json->string #()) "[]")
    (assert-equal (json->string #(1)) "[1]")
    (assert-equal (json->string #(1 2 3)) "[1,2,3]")
    (assert-equal (json->string #(#(#(1)))) "[[[1]]]"))

  (test "writes objects"
    (assert-equal (json->string '()) "{}")
    (assert-equal (json->string '(("foo" . "bar"))) "{\"foo\":\"bar\"}")))
