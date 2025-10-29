#lang racket

(require math/number-theory)

(define (max-multiple d b)
  (define (helper curr-num curr-max)
    (cond
      [(> curr-num b) curr-max]
      [(divides? d curr-num)(helper (add1 curr-num) curr-num)]
      [else (helper (add1 curr-num) curr-max)]
     )
   )
  
  (helper 1 0)
)

(= (max-multiple 2 7) 6)
(= (max-multiple 3 10) 9)
(= (max-multiple 7 17) 14)
(= (max-multiple 10 50) 50)
(= (max-multiple 37 200) 185)
(= (max-multiple 7 100) 98)
(= (max-multiple 14 200) 196)
; my test

