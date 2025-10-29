#lang racket

(define (sum-digits-iter n)
  (define (helper leftover res)
    (if (zero? leftover)
        res
        (helper (quotient leftover 10)(+ (remainder leftover 10)res))
     )
   )
 (if (negative? n)
     (error "Argument should be a non-negative number")
     (helper n 0)
  )
)

(= (sum-digits-iter 12345) 15)
(= (sum-digits-iter 123) 6)
(= (sum-digits-iter 23467)22)
; my test