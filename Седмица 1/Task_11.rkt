#lang racket

(define (palindrome? n)
  (define (helper leftover result)
    (cond
      [(negative? leftover) (error "Invalid argument!")]
      [(zero? leftover) result]
      [else (helper (quotient leftover 10)(+(* 10 result)(remainder leftover 10)))]
      )
   )
  
  (equal? (helper n 0) n)
)

(equal? (palindrome? 1) #t)
(equal? (palindrome? 6) #t)
(equal? (palindrome? 1010) #f)
(equal? (palindrome? 505) #t)
(equal? (palindrome? 123321) #t)
(equal? (palindrome? 654) #f)
(equal? (palindrome? 121) #t)
(equal? (palindrome? 12) #f)
(equal? (palindrome? 120) #f)
(equal? (palindrome? 12321) #t)
(equal? (palindrome? 1221) #t)
(equal? (palindrome? 234) #f)
; my test





