#lang racket
(require math/number-theory)

(define (sum-prime-divs-rec n)
  (define (helper leftover div sum)
    (cond
      [(> div leftover)sum]
      [(and
        (prime? div)
        (divides? div leftover)
        )
       (helper (/ leftover div) (add1 div) (+ div sum))]
      
      [else (helper leftover (add1 div) sum)]   
      )
    )
  (if (negative? n)
      (error "Argument should be a non-negative number")
      (helper n 2 0)
      )
)

(= (sum-prime-divs-rec 0) 0)
(= (sum-prime-divs-rec 6) 5) ; 2 + 3
(= (sum-prime-divs-rec 18) 5) ; 2 + 3
(= (sum-prime-divs-rec 19) 19)
(= (sum-prime-divs-rec 45136) 53)
(=(sum-prime-divs-rec 64)2)
; my test