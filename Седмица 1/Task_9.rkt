#lang racket




(define (snail col-height up down)
  (define (days-num height up down)
    (if (zero? (remainder (- height up)(- up down))) (+ (quotient (- height up)(- up down)) 1)
        (+ (quotient (- height up)(- up down)) 2)
    )
  )
  
  (cond
    [(or (negative? col-height)(negative? up)(negative? down))(error "Invalid parameters!")]
    [(< up down)(error "Cannot climb!")]
    [(< col-height up) 1]
    [else (days-num col-height up down)]
   )
)
(= (snail 3 2 1) 2)
(= (snail 10 3 1) 5)
(= (snail 10 3 2) 8)
(= (snail 100 20 5) 7)
(= (snail 5 10 3) 1)
(=(snail 9 4 2) 4)
;my test


