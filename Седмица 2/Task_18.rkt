#lang racket

(define (find-max n max-digit)
  (if (zero? n)
    max-digit
    (find-max (quotient n 10) (max max-digit (remainder n 10)))
   )
)

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

(define (sort-n n)
  (define (helper num res)
    (if (< num 10)
       (+ (* 10 res) num)
        (helper (remove-first-occurrence num (find-max num 0)) (+ (* 10 res) (find-max num 0)))
    )
  )
  (helper n 0)
)

(= (sort-n 1714) 7411)
(= (sort-n 123450) 543210)
(= (sort-n 123405) 543210)
(= (sort-n 123045) 543210)
(= (sort-n 120345) 543210)
(= (sort-n 102345) 543210)
(= (sort-n 8910) 9810)
(= (sort-n 321) 321)
(= (sort-n 29210) 92210)
(= (sort-n 1230) 3210)
(= (sort-n 55345) 55543)
(= (sort-n 14752) 75421)
(= (sort-n 329450) 954320)
(= (sort-n 9125) 9521)
(= (sort-n 54672) 76542)
; my test