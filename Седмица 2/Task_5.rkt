#lang racket
(require math/number-theory)



(define (amicable? num1 num2)
  (and
   (= num1 (-(divisor-sum num2) num2))
   (= num2 (- (divisor-sum num1) num1))
   )
 ) 


(equal? (amicable? 200 300) #f)
(equal? (amicable? 220 284) #t)
(equal? (amicable? 284 220) #t)
(equal? (amicable? 1184 1210) #t)
(equal? (amicable? 2620 2924) #t)
(equal? (amicable? 6232 6368) #t)
(equal? (amicable? 79 20) #f)
;my test