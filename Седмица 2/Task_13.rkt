#lang racket

(define (calculate-sum x n)
  (define (helper term-to-add curr-sum terms-left)
    (if(negative? terms-left);we stop when we have passed zero, because of the x^0 term
       curr-sum
       (helper (* term-to-add x)(+ curr-sum term-to-add)(sub1 terms-left))
     )
   )

  (helper 1 0 n)
)

(= (calculate-sum 5 0) 1)
(= (calculate-sum 5 1) 6)
(= (calculate-sum 10 1) 11)
(= (calculate-sum 1 11) 12)
(= (calculate-sum 2 11) 4095)
(=(calculate-sum 3 2) 13)
; my test


