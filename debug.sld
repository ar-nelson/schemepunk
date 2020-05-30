(define-library (schemepunk debug)
  (export form->indent json->indent
          write-debug write-debug-json

          make-report
          make-paragraph
          write-report
          write-reports
          write-report-separator)

  (import (scheme base)
          (scheme write)
          (schemepunk term-colors)
          (schemepunk debug indent)
          (schemepunk debug indent scheme)
          (schemepunk debug indent json)
          (schemepunk debug report))

  (begin
    (define (write-debug form)
      (parameterize ((output-width (max 40 (get-terminal-width))))
        (write-indented (form->indent form)))
      (newline))

    (define (write-debug-json json)
      (parameterize ((output-width (max 40 (get-terminal-width))))
        (write-indented (json->indent json)))
      (newline))))
