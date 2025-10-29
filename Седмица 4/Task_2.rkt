#lang racket

(define (set-union xs ys)
  (sort (remove-duplicates (append xs ys)) <)
 )

(equal? (set-union '(1 3 5 7) '(5 7 13)) '(1 3 5 7 13))
(equal? (set-union '(5 7 13) '(1 3 5 7)) '(1 3 5 7 13))
(equal? (set-union '(2 4 6 6 8) '(1 1 3 5 7)) '(1 2 3 4 5 6 7 8))
;my test
