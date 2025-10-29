#lang racket

(define (p n)
  (define (helper num res)
    (if (zero? num)
        res
        (helper (sub1 num)(+ res (add1 (* 3 (sub1 num)))))
     )
  )
  (helper n 0)
)

(= (p 1) 1)
(= (p 2) 5)
(= (p 3) 12)
(= (p 4) 22)
(= (p 5) 35)
(= (p 6) 51)
(= (p 7) 70)
;my test
