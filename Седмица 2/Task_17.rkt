#lang racket

(require racket/trace)

(define (behind-the-digit n digit res exponent)
  (if (or
        (= (remainder n 10) digit)
        (zero? n)
       )
      res
      (behind-the-digit (quotient n 10) digit  (+ res (* (expt 10 exponent) (remainder n 10)))  (add1 exponent))
   )
)

  (define (digit-num n)
    (if (< n 10)
     1
     (add1 (digit-num (quotient n 10)))
   )
)

(define (after-the-digit n digit)
  (quotient n (expt 10 (add1 (digit-num (behind-the-digit n digit 0 0)))))
)
  
(define (remove-first-occurrence n digit)
  (if (= (remainder n 10)digit)
      (quotient n 10)
  (+ (behind-the-digit n digit 0 0)
     (*
      (after-the-digit n digit)
      (expt 10 (digit-num (behind-the-digit n digit 0 0))))
  )
 )
)


(= (remove-first-occurrence 15365 5) 1536)
(= (remove-first-occurrence 15360 0) 1536)
(= (remove-first-occurrence 15300 0) 1530)
(= (remove-first-occurrence 15365 1) 5365)
(= (remove-first-occurrence 35365 3) 3565)
(= (remove-first-occurrence 1212 1) 122)
(= (remove-first-occurrence 1212 2) 121)
(= (remove-first-occurrence (remove-first-occurrence 1212 1) 1) 22)