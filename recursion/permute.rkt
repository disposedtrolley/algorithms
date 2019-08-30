#lang racket

(define (insert-between lst val idx)
  (append [take lst idx]
          [cons val (drop lst idx)]))

(define (permute-helper s chosen)
  (cond
    [(empty? s) (displayln chosen)]
    [else
     ;; choose/explore/unchoose
     ;; there are letters to be chosen in s, so for each letter, we need to choose it,
     ;; explore the resulting path, and then unchoose it
     (for ([char s])
       ;;(displayln (format "char: ~a chosen: ~a" char chosen))
       (define char-idx (index-of s char))
       (set! chosen (append chosen (list char)))
       (set! s (remove char s))
       (permute-helper s chosen)

       ;; un-choose
       (set! s (insert-between s char char-idx))
       (set! chosen (take chosen (- (length chosen) 1)))
       )
     ]))

(define (permute s)
  (permute-helper (string->list s) '()))


(permute "hello")
