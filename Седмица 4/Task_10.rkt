#lang racket

(define (concat-rec xs ys)
  (define (helper res leftover)
    (if (null? leftover)
        res
        (helper (flatten (cons res (car leftover))) (cdr leftover))
    )
 )
  (helper xs ys)
)

(define (concat-proc xs ys)
  (append xs ys)
 )

(equal? (concat-rec '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(equal? (concat-proc '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(equal? (concat-rec '(1 3 4 5) '(7 18 3)) '(1 3 4 5 7 18 3))
;my test
(equal? (concat-proc '(1 2 5 7) '(10 11 14)) '(1 2 5 7 10 11 14))
;my test