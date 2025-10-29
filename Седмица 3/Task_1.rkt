#lang racket

(define (upper-bound f y)
  (lambda (x)
    (if(>= y  (f x))
       y
       (f x)
     )
    )
)

(= ((upper-bound (lambda (a) (* 2 a)) 100) 50) 100)
(= ((upper-bound (lambda (a) (* 2 a)) 100.236) 500.002) 1000.004)
(= ((upper-bound (lambda (a) (* 2 a)) 80) 3) 80)
(= ((upper-bound (lambda (a) (* 2 a)) 70) 30) 70)
(= ((upper-bound (lambda (a) (* 2 a)) 30) 70) 140)
(= ((upper-bound (lambda (a) (expt a 2)) 200) 14) 200)
;my test