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

       ;; choose
       (define char-idx (index-of s char))
       (set! chosen (append chosen (list char)))
       (displayln (format "s: ~a char: ~a chosen: ~a" s char chosen))
       (set! s (remove char s))

       ;; explore
       (permute-helper s chosen)

       ;; un-choose
       (set! s (insert-between s char char-idx))
       (set! chosen (take chosen (- (length chosen) 1))))
     ]))

(define (permute s)
  (permute-helper (string->list s) '()))


(permute "hello")
