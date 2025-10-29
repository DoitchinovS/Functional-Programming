#lang racket
(require math/number-theory)

(define (cube-diff n)
  (- (* (add1 n)(add1 n) (add1 n))
     (* n n n)
   )
 )

(define (nth-cubic n)
  (define (helper cubic-numbers-left cubic-base)
    (cond
      [(and
        (= cubic-numbers-left 1)
        (prime? (cube-diff cubic-base))
        )
       (cube-diff cubic-base)
      ]
      [(prime? (cube-diff cubic-base)) (helper (sub1 cubic-numbers-left)(add1 cubic-base))]
      [else (helper cubic-numbers-left (add1 cubic-base))]
     )
   )
  
  (if(<= n 0)
     (error "Invalid argument")
     (helper n 1)
   )
)

(= (nth-cubic 1) 7)
(= (nth-cubic 4) 61) ; 61 is the 4th cubic prime number
(= (nth-cubic 50) 55897) ; 55897 is the 50th cubic prime number
(= (nth-cubic 100) 283669)
(= (nth-cubic 200) 1570357)
(= (nth-cubic 3) 37)
; my test

