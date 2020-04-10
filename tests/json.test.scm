(import (scheme base)
        (schemepunk syntax)
        (schemepunk json)
        (schemepunk test))

(test-suite "JSON Parser"
  (test "parses keywords"
    (assert-equal (string->json "null") 'null)
    (assert-equal (string->json "true") 'true)
    (assert-equal (string->json "false") 'false))

  (test "parses numbers"
    (assert-equal (string->json "0") 0)
    (assert-equal (string->json "0.1") 0.1)
    (assert-equal (string->json "1") 1)
    (assert-equal (string->json "1.5") 1.5)
    (assert-equal (string->json ".5") 0.5)
    (assert-equal (string->json "-1") -1)
    (assert-equal (string->json "1e3") 1000.0))

  (test "parses strings"
    (assert-equal (string->json "\"\"") "")
    (assert-equal (string->json "\"foo\"") "foo")
    (assert-equal
      (string->json "\"foo \\\\\\/\\'\\\"\\n\\r\\t\\b\\v\\f\\u0020\"")
      (string-append "foo \\/'\"\n\r\t\b" (string #\x0b #\x0c) " "))
    (assert-equal (string->json "\"={foo}\"") "={foo}"))

  (test "parses Unicode in strings"
    (assert-equal (string->json "\"λ\"") "λ"))

  (test "parses arrays"
    (assert-equal (string->json "[]") '())
    (assert-equal (string->json "[1, 2, 3]") '(1 2 3))
    (assert-equal
      (string->json "[[1, 2] ,[true, \"]foo[\"], [[]]]")
      '((1 2) (true "]foo[") (()))))

  (test "parses objects"
    (assert-equal (string->json "{}") '(object))
    (assert-equal (string->json "{\"a\": 1, \"b\": 2, \"c\": 3}")
                  '(object ("c" . 3) ("b" . 2) ("a" . 1))))

  (test "parses nested objects"
    (assert-equal (string->json "{\"a\":{},\"b\":{}}")
                  '(object ("b" . (object)) ("a" . (object))))
    (assert-equal (string->json "{\"a\":{\"b\":\"c\"}}")
                  '(object ("a" . (object ("b" . "c")))))
    (assert-equal (string->json "{\"a\":{},\"b\":{\"\":\"foo\"},\"c\":{\"d\":{\"e\":\"f\"}}}")
                  '(object ("c" . (object ("d" . (object ("e" . "f")))))
                           ("b" . (object ("" . "foo")))
                           ("a" . (object)))))

  (test "parses nested arrays and objects"
    (assert-equal (string->json "[[{\"\":[{\"\":[]}]}]]")
                  '(((object ("" . ((object ("" . ()))))))))))
