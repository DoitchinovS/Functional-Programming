#lang racket

(define (right-max xs)
  (define (helper leftover result)
    (if (null? leftover)
        result
        (helper (cdr leftover) (append result (list (car (sort leftover >))) ))
     )
   )
  (helper xs null)
)


(equal? (right-max '(5 8 9 12)) '(12 12 12 12))
(equal? (right-max '(4 3 2 1 0)) '(4 3 2 1 0))
(equal? (right-max '(1 2 3 4 -5 6 7 -2 -1 0)) '(7 7 7 7 7 7 7 0 0 0))