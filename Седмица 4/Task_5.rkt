#lang racket
(require math/number-theory)


(define (factorize num)
  (define (helper leftover div res)
    (cond
      [(= 1 leftover) res]
      [(and
        (divides? div leftover)
        (prime? div)
        )
       (helper (quotient leftover div) div (sort (cons div res) <= ))]
      [else (helper leftover (add1 div) res)]
    )
  )
  (if (< num 1)
      (error "Number should be at least one")
      (helper num 2 null)
    )
 )

(equal? (factorize 2) '(2))
(equal? (factorize 6) '(2 3))
(equal? (factorize 13) '(13))
(equal? (factorize 123) '(3 41))
(equal? (factorize 152) '(2 2 2 19))
(equal? (factorize 12356498) '(2 7 11 19 41 103))
(equal? (factorize 96) '( 2 2 2 2 2 3))
;my test