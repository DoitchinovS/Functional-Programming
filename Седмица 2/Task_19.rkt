#lang racket

(define (odd-fact n)
  (if (zero? n)
      1
      (* (+ (* 2 n) 1) (odd-fact (sub1 n)))
   )
)
  
(define (calc-series-sum x n)
  (define (helper val num res)
    (if (zero? num)
        res
        (helper val (sub1 num)
             (+ res (/ (* (expt -2 (add1 num))
                          (expt x num)
                        )
                       (odd-fact num)
                      )
              )
         )
    )
  )
    (helper x n -2)
)








(calc-series-sum 1 0) ; -2
(calc-series-sum 1 1) ; -2/3
(calc-series-sum 1 2) ; -1 1/5
(calc-series-sum 1 3) ; -1 1/21
(calc-series-sum 1 4) ; -1 11/135
(calc-series-sum 1 5) ; -1 29/385
(calc-series-sum 1 6) ; -1 937/12285
(calc-series-sum 1 7)
; my test