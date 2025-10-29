#lang racket

(require math/number-theory)

(define (decreasing-order? number)
  (if (< number 10)
      (>= (remainder number 10) 0) ; a number with one digit is always decreasing
      (and
       (<= (remainder number 10)(remainder (quotient number 10) 10))
       (decreasing-order? (quotient number 10))
       )
   )
 )
  

(define (sum-numbers start finish)
   (define (helper begin end)
     (cond
       [(negative? begin) (error "The lower bound has to be at least 0")]
       [(> begin end) 0]
       [(decreasing-order? begin)(+ (helper (add1 begin) end) begin)]
       [else (helper (add1 begin) end)]
     )
   )
 (helper (min start finish) (max start finish))
)

(= (sum-numbers 1 9) 45)
(= (sum-numbers 199 203) 200)
(= (sum-numbers 219 225) 663)
(= (sum-numbers 225 219) 663)
(= (sum-numbers 1 20) 86)
;my test








