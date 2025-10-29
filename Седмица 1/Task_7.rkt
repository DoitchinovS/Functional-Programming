#lang racket
(define (can-carry? c k w)
  (cond
    [(negative? c) (error "number of products should be non-negative number")]
    [(negative? k)(error "The amount of products John can carry is non-negative")]
    [(negative? w)(error "Item should weight at least 0 kilograms")]
    [(<= (* c w) k)#t]
    [else #f]
   )
)
(equal? (can-carry? 5 15 3) #t)
(equal? (can-carry? 1 5 4) #t)
(equal? (can-carry? 13 25 2) #f)
(equal? (can-carry? 24 104.44 21.12) #f)
(equal? (can-carry? 51 34.75 19.852) #f)
(equal? (can-carry? 42 95.11 0.51) #t)
(equal? (can-carry? 12 45 4)#f)
; my test


