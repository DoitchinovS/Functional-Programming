#lang racket

(define (f x y)
  ((lambda (a b) (+ (* x (* a a)) (* y b) (* a b)))
   (+ 1 (* x y))
   (- 1 y)
   )
)
(f 10 10)