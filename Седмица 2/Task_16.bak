#lang racket
(define (digit-sum leftover res)
    (if(zero? leftover)
       res
       (digit-sum (quotient leftover 10) (+ res (remainder leftover 10)))
     )
 )

(define (digital-root n)
  (define (helper leftover)
    (if (> (digit-sum leftover 0) 10)
        (helper (digit-sum leftover 0))
        (digit-sum leftover 0)
     )
   )
  (if (<= n 0)
      (error"The number has to be natural!")
      (helper n)
   )

)



(= (digital-root 16) 7)
(= (digital-root 942) 6)
(= (digital-root 132189) 6)
(= (digital-root 493193) 2)
(= (digital-root 575) 8)
;my test