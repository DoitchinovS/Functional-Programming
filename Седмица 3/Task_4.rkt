#lang racket

(define (derive f eps)
  (lambda (x)
    (/
     (- (f (+ x eps)) (f x))
     eps
     )
  )
)

(define (derive-n f n eps)
  (lambda (x)
    (if (zero? n)
        (f x)
        ((derive-n (derive f eps) (sub1 n) eps) x)
      )
   )
 )
(= ((derive-n (λ (x) (* 2 x x x)) 3 1e-3) 2) 12.000015203739167)
(= ((derive-n (λ (x) (* 2 x x x)) 2 1e-3) 1) 12.012000000449774)
;my test