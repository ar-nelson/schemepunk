(define (%start-cursor str start)
  (if start (string-index->cursor str start) (string-cursor-start str)))

(define (%end-cursor str end)
  (if end (string-index->cursor str end) (string-cursor-end str)))

(define (%substr str start end)
  (substring/cursors str (%start-cursor str start) (%end-cursor str end)))

(define+ (string-every pred str :optional (start #f) (end #f))
  (let* ((start-cursor (%start-cursor str start))
         (end-cursor (%end-cursor str end)))
    (or (is start-cursor string-cursor>=? end-cursor)
        (let lp ((c start-cursor))
          (and (pred (string-ref/cursor str c))
               (let1 c+ (string-cursor-next str c)
                 (if (is c+ string-cursor>=? end-cursor)
                   (pred (string-ref/cursor str c))
                   (lp c+))))))))

(define+ (string-prefix? str1 str2 :optional (start1 #f) (end1 #f) (start2 #f) (end2 #f))
  (if start1
    (string-prefix?/130 (%substr str1 start1 end1) (%substr str2 start2 end2))
    (string-prefix?/130 str1 str2)))

(define+ (string-suffix? str1 str2 :optional (start1 #f) (end1 #f) (start2 #f) (end2 #f))
  (if start1
    (string-suffix?/130 (%substr str1 start1 end1) (%substr str2 start2 end2))
    (string-suffix?/130 str1 str2)))

(define+ (string-index str delim :optional (start #f) (end #f))
  (let* ((start-cursor (%start-cursor str start))
         (end-cursor (%end-cursor str end))
         (match-cursor (string-index/130 str delim start-cursor end-cursor)))
    (and (isnt match-cursor string-cursor=? end-cursor)
         (string-cursor->index str match-cursor))))

(define+ (string-index-right str delim :optional (start #f) (end #f))
  (let* ((start-cursor (%start-cursor str start))
         (end-cursor (%end-cursor str end))
         (match-cursor (string-index-right/130 str delim start-cursor end-cursor)))
    (and (isnt match-cursor string-cursor=? start-cursor)
         (string-cursor->index str (string-cursor-prev str match-cursor)))))

(define+ (string-skip str delim :optional (start #f) (end #f))
  (let* ((start-cursor (%start-cursor str start))
         (end-cursor (%end-cursor str end))
         (match-cursor (string-skip/130 str delim start-cursor end-cursor)))
    (and (isnt match-cursor string-cursor=? end-cursor)
         (string-cursor->index str match-cursor))))

(define+ (string-skip-right str delim :optional (start #f) (end #f))
  (let* ((start-cursor (%start-cursor str start))
         (end-cursor (%end-cursor str end))
         (match-cursor (string-skip-right/130 str delim start-cursor end-cursor)))
    (and (isnt match-cursor string-cursor=? start-cursor)
         (string-cursor->index str (string-cursor-prev str match-cursor)))))

(define (string-contains str1 . o)
  (chain-and (apply string-contains/130 str1 o)
             (string-cursor->index str1 <>)))

(define (string-contains-right str1 . o)
  (chain-and (apply string-contains-right/130 str1 o)
             (string-cursor->index str1 <>)))

(define+ (string-join strs :optional (delim " ") (grammar 'infix))
  (string-join/130 strs delim grammar))

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
  (case grammar
    ((strict-infix)
      (when (string-null? str)
        (error "cannot string-split empty string with strict-infix")))
    ((infix prefix suffix) #f)
    (else
      (error "invalid grammar for string-split" grammar)))
  (if (isnt delim string-null?)
    (let1 result (string-split/130 str delim grammar limit start end)
      (cond-expand
        (chibi
          ; Chibi bug: prefix doesn't work
          ; FIXME: When Chibi fixes this, remove this patch
          (match (list grammar result)
            (('prefix ("" . rest)) rest)
            (else result)))
        (else result)))
    (if (and limit (is limit < (- end start)))
      (let* ((limit* (case grammar
                       ((prefix infix strict-infix) limit)
                       ((suffix) (+ limit 1))))
             (chars (chain (substring str start (+ start limit*))
                           (string->list <>)
                           (map string <>))))
        (snoc (case grammar ((prefix) (cons "" chars)) (else chars))
              (substring str (+ start limit*) end)))
      (chain (substring str start end)
             (string->list <>)
             (map string <>)))))

(define+ (string-take-while str pred? :optional (start #f) (end #f))
  (if start
    (string-take-while (%substr str start end) pred?)
    (substring/cursors str 
      (string-cursor-start str)
      (string-skip/130 str pred?))))

(define+ (string-take-while-right str pred? :optional (start #f) (end #f))
  (if start
    (string-take-while-right (%substr str start end) pred?)
    (substring/cursors str
      (string-skip-right/130 str pred?)
      (string-cursor-end str))))

(define string-drop-while string-trim)
(define string-drop-while-right string-trim-right)

(define+ (string-span str pred? :optional (start #f) (end #f))
  (if start
    (string-span (%substr str start end) pred?)
    (let1 last (string-skip/130 str pred?)
      (values 
        (substring/cursors str (string-cursor-start str) last)
        (substring/cursors str last (string-cursor-end str))))))

(define (string-break str pred? . o)
  (apply string-span str (isnt _ pred?) o))