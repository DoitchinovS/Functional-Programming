#lang racket

(define (apply-n f counter)
  (lambda (x)
    (if (zero? counter)    
        x                  ; could be done with returning (f x) instead, but the condition will be (= counter 1)
        ((apply-n f (sub1 counter))(f x))
     )
  )
)

(= ((apply-n (λ (x) (* 2 x)) 5) 2) 64)
(= ((apply-n (λ (x) (quotient x 10)) 2) 100) 1)
(= ((apply-n (lambda (x) (add1 (* 3 x))) 3) 2) 67)
;my test