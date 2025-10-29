#lang racket
(require math/number-theory)
(require racket/trace)

(define (accumulate f acc start end transform next)
  (if (> start end)
      acc
      (accumulate f (f (transform start) acc) (next start) end transform next)
   )
)

(define (any? beg end p?)
  (accumulate (lambda (x y) (or x y)) #f beg end p? add1)
)

(define (cubed-interval-till num)
  (accumulate + 0 2 num (lambda (x) (expt x 3)) (curry + 3))
)

(define (fact-accum n)
  (accumulate * 1 1 n identity add1)
 )

(define (prime-accum? p)
  (or
   (accumulate (lambda (x y) (not (or x y))) #f 2 (sub1 p) (lambda (a)(divides? a p)) add1)
   (= p 2)
   )
 )

(= (fact-accum 5) 120)
(= (fact-accum 8) 40320)
(= (fact-accum 6) 720)
;my test

(equal? (prime-accum? 1) #f)
(equal? (prime-accum? 2) #t)
(equal? (prime-accum? 3) #t)
(equal? (prime-accum? 6) #f)
(equal? (prime-accum? 42) #f)
(equal? (prime-accum? 61) #t)
(equal? (prime-accum? 97) #t)
;my test

(equal? (any? 1001 1500 (λ (x) (< x 1000))) #f)
(equal? (any? 1 100 odd?) #t)
(equal? (any? 96 100 prime?) #t)
;my test

(= (cubed-interval-till 11) 1976)
(= (cubed-interval-till 15) 4720)
(= (cubed-interval-till 8) 645)
;my test