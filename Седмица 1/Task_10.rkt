#lang racket

(define (rev n)
  (define (helper leftover result)
    (if (zero? leftover)
        result
        (helper (quotient leftover 10)(+(* 10 result)(remainder leftover 10)))
     )
   )
  (if(negative? n)
     (error "Invalid argument!")
     (helper n 0)
  )
)
(= (rev 1) 1)
(= (rev 123) 321)
(= (rev 987654321) 123456789)
(= (rev 9567)7659)
; my test




