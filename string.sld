(define-library (schemepunk string)
  (export string? string-null? string-every string-any)
  (export make-string string string-tabulate string-unfold string-unfold-right)
  (export string->vector string->list
          vector->string list->string reverse-list->string)
  (export string-length string-ref substring string-copy
          string-take string-take-right
          string-drop string-drop-right
          string-pad string-pad-right
          string-trim string-trim-right string-trim-both)
  (export string-replace)
  (export string=? string-ci=?
          string<? string-ci<?
          string>? string-ci>?
          string<=? string-ci<=?
          string>=? string-ci>=?)
  (export string-prefix-length string-suffix-length
          string-prefix? string-suffix?)
  (export string-index string-index-right
          string-skip string-skip-right
          string-contains string-contains-right
          string-take-while string-take-while-right
          string-drop-while string-drop-while-right
          string-break string-span)
  (export string-append string-concatenate string-concatenate-reverse
          string-join)
  (export string-fold string-fold-right
          string-map string-for-each
          string-count
          string-filter string-remove)
  (export string-replicate string-segment string-split)
  (export read-string write-string)
  (export string-set! string-fill! string-copy!)

  (cond-expand
    (chicken
      (import (except (scheme base)
                string-length string-ref string-copy substring string-set!
                make-string string string-for-each string-map)
              (only (scheme char)
                string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=? char-whitespace?)
              (only (utf8)
                string-length string-ref string-set! substring make-string string
                reverse-list->string)
              (schemepunk syntax)
              (only (schemepunk list) unfold snoc)
              (rename (except (utf8-srfi-13) string-concatenate-reverse string-trim-right string-trim-both)
                (string-delete string-remove)
                (xsubstring string-replicate)))
      (begin
        (define+ (string-trim-right str :optional (pred? char-whitespace?) (start #f) (end #f))
          (if start
            (string-trim-right
              (substring str start (or end (string-length str)))
              pred?)
            (let1 last (string-skip-right str pred?)
              (if last (string-take str (+ last 1)) ""))))

        (define+ (string-trim-both str :optional (pred? char-whitespace?) (start #f) (end #f))
          (string-trim (string-trim-right str pred? start end) pred?))

        (define+ (string-concatenate-reverse strs :optional (final "") (end (string-length final)))
          (if (string-null? final)
            (string-concatenate (reverse strs))
            (string-concatenate (reverse (cons (substring final 0 end) strs))))))

      (include "polyfills/152-from-13.scm"))
    (gerbil
      (import (scheme base)
              (only (scheme char)
                string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?)
              (schemepunk syntax)
              (only (schemepunk list) unfold snoc)
              (rename (except (std srfi 13) string-for-each string-map)
                (string-delete string-remove)))
      (include "polyfills/152-from-13.scm")
      (begin
        (define+ (string-replicate str from to :optional (start 0) (end (string-length str)))
          (if (and (= from to) (= start end))
            ""
            (let1 len (- end start)
              (assume (positive? len))
              (string-unfold (is _ >= to)
                             (λ=> (modulo <> len) (+ start <>) (string-ref str <>))
                             (cut + <> 1)
                             from))))))
    ((library (srfi 152))
      (import (scheme base)
              (only (scheme char)
                string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?)
              (except (srfi 152)
                string=? string<? string>? string<=? string>=?
                string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?)))
    ((library (srfi 13))
      (import (scheme base)
              (only (scheme char)
                string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?)
              (schemepunk syntax)
              (only (schemepunk list) unfold snoc)
              (rename (except (srfi 13) string-for-each string-map)
                (string-delete string-remove)
                (xsubstring string-replicate)))
      (include "polyfills/152-from-13.scm"))
    ((library (srfi 130))
      (import (scheme base)
              (only (scheme char)
                string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?)
              (schemepunk syntax)
              (only (schemepunk list) unfold snoc)
              (rename (srfi 130)
                (string-every string-every/130)
                (string-prefix? string-prefix?/130)
                (string-suffix? string-suffix?/130)
                (string-index string-index/130)
                (string-index-right string-index-right/130)
                (string-skip string-skip/130)
                (string-skip-right string-skip-right/130)
                (string-contains string-contains/130)
                (string-contains-right string-contains-right/130)
                (string-join string-join/130)
                (string-split string-split/130)))
      (include "polyfills/152-from-130.scm"))))
