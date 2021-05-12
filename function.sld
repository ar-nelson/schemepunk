(define-library (schemepunk function)
  (export identity const flip compose bind complement)
  (import (scheme base))
  (include "function/schemepunk.scm"))
