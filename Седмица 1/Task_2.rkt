#lang racket
(define (even-if? x)
  (if (= (remainder x 2) 0) "Yes" "No")
)
(define (even-guards? x)
  (cond
    [(= (remainder x 2) 0) "Yes"]
    [else "No"]
    )
)




(equal? (even-if? 2) "Yes")
(equal? (even-if? 15452) "Yes")
(equal? (even-if? 321) "No")
(equal? (even-if? 1237) "No")
;my test

(equal? (even-guards? 2) "Yes")
(equal? (even-guards? 15452) "Yes")
(equal? (even-guards? 321) "No")
(equal? (even-guards? 435) "No")
; my test