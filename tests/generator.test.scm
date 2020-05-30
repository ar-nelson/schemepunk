;; This is an adaptation of the SRFI 158 tests in the official repo:
;;
;; https://github.com/scheme-requests-for-implementation/srfi-158
;;
;; The original tests used chibi-test; this test file uses a few shim macros to
;; make these tests fit the (schemepunk test) mold.
;;
;; The tests themselves are in srfi-158-tests.scm.

(import (scheme base)
        (scheme read)
        (scheme write)
        (schemepunk syntax)
        (only (schemepunk list) unfold)
        (schemepunk generator)
        (schemepunk test))

(chibi-test-shim test
  (cond-expand
    (chibi
      (include "tests/srfi-158-tests.scm"))
    (else
      (include "srfi-158-tests.scm"))))
