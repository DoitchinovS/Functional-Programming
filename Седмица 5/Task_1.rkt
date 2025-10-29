#lang racket

(define (longest-ascending-sub xs)
  (define (helper leftover longest current)
    (cond
      [(and (null? leftover)(>= (length current) (length longest)))  current] ;if the current subsequence has the same length as the longest one so far, we return the current one
      [(and (null? leftover)(> (length longest) (length current)))  longest]
      [(>= (car leftover) (last current)) (helper (cdr leftover) longest (append current (list (car leftover))))]
      [(>= (length current) (length longest)) (helper (cdr leftover) current (list (car leftover)))]
      [else (helper (cdr leftover) longest (list (car leftover)))]
     )
   )
  (helper (cdr xs) (list (car xs)) (list (car xs)))
)


(equal? (longest-ascending-sub '(1 0 5)) '(0 5))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 6 7 7 1 5)) '(1 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 2 7 7 15)) '(2 7 7 15))
(equal? (longest-ascending-sub '(1 5 2 3 4 5 6 7 7 1 5)) '(2 3 4 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 4 6 8 3 4 1)) '(2 4 6 8))