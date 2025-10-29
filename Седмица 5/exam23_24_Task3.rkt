#lang racket

(define (list-to-num xs)
  (foldl (lambda (dig acc) (+ (* 10 acc) dig)) 0 xs)
)

(define (num-to-xs n)
  (define (helper result leftover)
    (if (zero? leftover)
        result
        (helper (cons (remainder leftover 10) result) (quotient leftover 10))
     )
    )
  (helper null n)
)

(define (palindromize n)
     (let ([first-part (sort (remove-duplicates (num-to-xs n)) <)])
       ( list-to-num (append first-part (reverse first-part)))
     )
)

(= (palindromize 11) 11)
(= (palindromize 3354457878) 3457887543)
(= (palindromize 11335445789789) 13457899875431)