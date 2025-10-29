#lang racket

(define (derive-x f eps)
  (lambda (x y)
    (/
     (- (f (+ x eps) y) (f x y))
     eps
    )
   )
)

(define (derive-y f eps)
  (lambda (x y)
    (
     /
     (- (f x (+ y eps)) (f x y))
     eps
     )
   )
)

(define (g x y) (+ (* x x x) (* x y) (* y y)))

(= ((derive-x g 1e-4) 2 3) 15.000600010033338)
(= ((derive-y g 1e-4) 2 3) 8.00009999998963)
(= ((derive-x g 1e-4) 5 4) 79.00150000978101)
;my test