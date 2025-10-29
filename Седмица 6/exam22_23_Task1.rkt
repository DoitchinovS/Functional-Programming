#lang racket

(require math/number-theory)

(define (primes-prod n)
  (define (helper current-num result)
    (cond
      [(> (* current-num current-num) n) result]
      [ (prime? current-num) (helper (add1 current-num) (* result current-num))]
      [else (helper (add1 current-num) result)]
    )
  )
  (helper 2 1)
 )


(= (primes-prod 12) 6)
(= (primes-prod 49) 210)
(= (primes-prod 1200) 200560490130)

