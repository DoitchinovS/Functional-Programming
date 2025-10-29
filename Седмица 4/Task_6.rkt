#lang racket

(define (remove-non-negative xs)
 (cond
   [(null? xs) null]
   [(<= 0 (car xs)) (remove-non-negative (cdr xs))]
   [else (cons (car xs) (remove-non-negative (cdr xs)))]
 )
)

(define (kth-max-min xs)
  (lambda (k) 
   (if (> k (length (remove-non-negative (remove-duplicates xs))))
          (error "No such number!")
          (list-ref (sort (remove-non-negative (remove-duplicates xs)) >) (sub1 k))
    )
  )
)

(= ((kth-max-min '(-1)) 1) -1)
(= ((kth-max-min '(-1 -5 -6 -6 -6 -6)) 2) -5)
(= ((kth-max-min '(-1 -1 -5 -6 -6 -6 -6)) 2) -5)
(= ((kth-max-min '(1 2 3 4 -5 6 7 -2 -1 0)) 2) -2)
(= ((kth-max-min '( 6 7 5 12 -2 -5 -3 0)) 1) -2)
;my test
((kth-max-min '(-1 0 -1 0 -2 3 1 -1)) 3) ; error: No such number!