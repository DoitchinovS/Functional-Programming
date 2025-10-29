#lang racket

(require math/number-theory)

(define (coprime? a b)
  (= (gcd a b) 1)
  )

(define (g-l-sum limit)
  (define (helper divisor)

    (cond
      [(< limit 2) (error "Limit has to be at least 2!")]
      [(= limit 2) (cons 1 1)]
      [(and
         (divides? divisor (sub1 limit))
         (coprime? divisor (quotient (sub1 limit) divisor))
        ) (cons divisor (quotient (sub1 limit) divisor))
      ]
      [else (helper (sub1 divisor))]
     )
  )
  (helper (- limit 2))
)

;(g-l-sum 1) Limit has to be at least 2!
(g-l-sum 2)
(g-l-sum 3)
(g-l-sum 13)
