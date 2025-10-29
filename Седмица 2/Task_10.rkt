#lang racket
(require math/number-theory)

(define (digit-num n)
  (if(zero? n)
     0
     (add1 (digit-num (quotient n 10)))
   )
)

(define (automorphic? n)
  (if (<= n 0)
      (error "The number has to be at least 1")
      (divides?
       (expt 10 (digit-num n))
       (- (* n n) n)
       )
   )
)

(equal? (automorphic? 3)#f)
(equal? (automorphic? 10)#f)
(equal? (automorphic? 5)#t)
(equal? (automorphic? 25)#t)
(equal? (automorphic? 76)#t) 
(equal? (automorphic? 890625)#t) 
(equal? (automorphic? 625)#t) 
(equal? (automorphic? 36) #f)
(equal? (automorphic? 11) #f)
(equal? (automorphic? 7) #f)
; my test

