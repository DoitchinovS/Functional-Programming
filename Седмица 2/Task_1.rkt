#lang racket

(define (count-digits-iter n)
  (define (helper leftover res)
    (if (zero? leftover)
        res
        (helper (quotient leftover 10)(add1 res))
     )
   )
  (if (negative? n)
     (error "Argument should be a non-negative number")            
     (helper n 0)
   )
)

(define (count-digits-rec n)
 (cond
   [(negative? n)(error "Argument should be non-negative number")]
   [(zero? n) 0]
  [else (add1 (count-digits-rec (quotient n 10)))]
  )
)
(= (count-digits-iter 12345) 5)
(= (count-digits-iter 123) 3)
(= (count-digits-iter 11245)5)
; my test
(= (count-digits-rec 12345) 5)
(= (count-digits-rec 123) 3)
(= (count-digits-rec 123674)6)
; my test