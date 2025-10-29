#lang racket

(define (growing-plant up-speed down-speed desired-height)
  (define (day-num up down desired)
    (if ( zero? (remainder (- desired up)(- up down)))  (+(quotient (- desired up)(- up down))1)
        (+ (quotient (- desired up)(- up down)) 2)
     )
   )
   (cond
     [(< up-speed down-speed)(error "Cannot grow!")]
     [(or (negative? up-speed)(negative? down-speed)(negative? desired-height))(error "Invalid parameters!")] 
     [(< up-speed desired-height)(day-num up-speed down-speed desired-height)]
     [else 1]
    ) 
)

(= (growing-plant 5 2 5) 1)
(= (growing-plant 5 2 6) 2)
(= (growing-plant 10 9 4) 1)
(= (growing-plant 100 10 910) 10)
(=(growing-plant 4 2 9) 4)
; my test








