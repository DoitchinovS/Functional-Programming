#lang racket
(require math/number-theory)
(require racket/trace)
(define (digit-sum n)
  (if (< n 10)
      n
      (+ (remainder n 10)
         (digit-sum (quotient n 10))
       )
   )
)

(define (sum-divisible-numbers start finish k)
  (define (sum-div st end)
    (cond
      [(> st end) 0]
      [(divides? k (digit-sum st))(+ st (sum-div (add1 st) end))]
      [else (sum-div (add1 st) end)]  
   )
  )
  (sum-div (min start finish)(max start finish))
)


(= (sum-divisible-numbers 0 10 5) 5)

(= (sum-divisible-numbers 0 100 5) 990)
(= (sum-divisible-numbers 100 0 5) 990)
(= (sum-divisible-numbers 20 0 5) 38)
; my test