#lang racket

(define (find-sum a b n)
  (define (helper num res)
     (if(negative? num)
        res
        (helper (sub1 num) (+ res (* b (expt 2 num))))
      )
    )
    (+ (helper (sub1 n) a) (helper (- n 2) a) (helper (- n 3) a))
)


(= (find-sum 0 2 10) 3578)
(= (find-sum 5 3 5) 174)
(= (find-sum 4 2 4) 62)
; my test


