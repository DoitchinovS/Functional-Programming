#lang racket

(define (digit-num n)
 (if (< n 10)
     1
     (add1 (digit-num (quotient n 10)))
   )
)
(define (sub-num? small-num greater-num)
    (and(<= small-num greater-num)
        (or
         (zero? (remainder (- greater-num small-num)(expt 10 (digit-num small-num))))
         (sub-num? small-num (quotient greater-num 10))
         )
     )
)


(equal? (sub-num? 123 5123783) #t)
(equal? (sub-num? 0 0) #t)
(equal? (sub-num? 10 101) #t)
(equal? (sub-num? 101 101) #t)
(equal? (sub-num? 10 0) #f)
(equal? (sub-num? 1253 5123783) #f)
(equal? (sub-num? 12 0) #f)
(equal? (sub-num? 12 1981234) #t)