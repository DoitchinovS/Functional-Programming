#lang racket
(require math/number-theory)

(define (interesting? n)
  (define (helper leftover digit-sum)
    (if (zero? leftover)
        digit-sum
        (helper (quotient leftover 10) (+ digit-sum (remainder leftover 10)))
     )
  )

  (divides? (helper n 0) n)
)

(equal? (interesting? 410) #t)
(equal? (interesting? 212) #f)
(equal? (interesting? 567) #f)
(equal? (interesting? 70) #t)
(equal? (interesting? 5) #t)
(equal? (interesting? 4) #t)
(equal? (interesting? 20) #t)
; my test