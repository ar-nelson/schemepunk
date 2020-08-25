(define+ (string-contains-right str1
                                str2
                                :optional
                                (start1 0)
                                (end1 (string-length str1))
                                (start2 0)
                                (end2 (string-length str2)))
  (if (= start2 end2)
    end1
    (let1 found (string-contains str1 str2 start1 end1 start2 end2)
      (and found
           (or (string-contains-right str1 str2 (+ found (- end2 start2)) end1 start2 end2)
               found)))))

(define (string-segment str k)
  (define len (string-length str))
  (unfold (is _ >= len)
          (λ i (substring str i (min (+ i k) len)))
          (cut + <> k)
          0))

(define+ (string-split str
                       delim
                       :optional
                       (grammar 'infix)
                       (limit #f)
                       (start 0)
                       (end (string-length str)))
  (define dlen (string-length delim))
  (define chunks
    (cond
      ((is start >= end)
        '())
      ((string-null? delim)
        (if (and limit (is limit < (- end start)))
          (let* ((limit* (case grammar ((suffix) (+ limit 1)) (else limit)))
                 (chars (chain (substring str start (+ start limit*))
                               (string->list _)
                               (map string _))))
            (snoc (case grammar ((prefix) (cons "" chars)) (else chars))
                  (substring str (+ start limit*) end)))
          (chain (substring str start end)
                 (string->list _)
                 (map string _))))
      (else
        (unfold not
                (λ((last next _))
                  (substring str (min last next) (min next end)))
                (λ((_ last count))
                  (cond
                    ((is last >= end)
                      #f)
                    ((and limit (is count >= limit))
                      (list (+ last dlen) end (+ count 1)))
                    (else
                      (list (+ last dlen)
                            (or (string-contains str delim (+ last dlen) end)
                                end)
                            (+ count 1)))))
                (list start
                      (or (string-contains str delim start end)
                          end)
                      1)))))
  (case grammar
    ((infix)
      chunks)
    ((strict-infix)
      (if (null? chunks)
        (error "cannot string-split empty string with strict-infix")
        chunks))
    ((prefix)
      (match chunks
        (("" . rest) rest)
        (else chunks)))
    ((suffix)
      (match (reverse chunks)
        (("" . rest) (reverse rest))
        (else chunks)))
    (else
      (error "invalid grammar for string-split" grammar))))

(define+ (string-take-while str pred? :optional (start #f) (end #f))
  (if start
    (string-take-while
      (substring str start (or end (string-length str)))
      pred?)
    (let1 last (string-skip str pred?)
      (if last (string-take str last) str))))

(define+ (string-take-while-right str pred? :optional (start #f) (end #f))
  (if start
    (string-take-while-right
      (substring str start (or end (string-length str)))
      pred?)
    (let1 last (string-skip-right str pred?)
      (if last (string-take-right str (+ last 1)) str))))

(define string-drop-while string-trim)
(define string-drop-while-right string-trim-right)

(define+ (string-span str pred? :optional (start #f) (end #f))
  (if start
    (string-span
      (substring str start (or end (string-length str)))
      pred?)
    (let1 last (string-skip str pred?)
      (if last
        (values (string-take str last) (string-drop str last))
        (values str "")))))

(define (string-break str pred? . o)
  (apply string-span str (lambda (x) (not (pred? x))) o))
