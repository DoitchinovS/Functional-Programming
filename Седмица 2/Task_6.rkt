#lang racket

(require math/number-theory)

(define (contain-digit? num dig)
 #| (cond
    [(zero? num) #f]
    [(= (remainder num 10) dig) #t]
    [else (contain-digit? (quotient num 10) dig)] 
   )
|#
  
 (if (< num 10)
     (= num dig)
     (or
      (= (remainder num 10) dig)
      (contain-digit? (quotient num 10) dig)
      )
  )
  
)

(define (sum-special-primes n d)
  (define (helper nums-left curr-num)
    (cond
      [(zero? nums-left) 0]
      [(and
        (contain-digit? curr-num d)
        (prime? curr-num)
        )
       (+ curr-num (helper (sub1 nums-left) (add1 curr-num)))
      ]
      
     [else (helper nums-left (add1 curr-num))]
    )
 )
  
  (helper n 1)
)

(= (sum-special-primes 5 2) 392)
(= (sum-special-primes 5 3) 107)
(= (sum-special-primes 10 3) 462)
(= (sum-special-primes 3 9) 107)
; my test
