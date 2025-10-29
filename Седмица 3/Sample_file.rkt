#lang racket

(define (switch-sum f g n)
  (λ (x)
    (if (zero? n)
        0
        (+ (f x) ((switch-sum g f (sub1 n)) (f x)))
        )
    )
  )


