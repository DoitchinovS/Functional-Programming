#lang racket

(define (count-occurrences n digit)
  (cond
    [(and
      (< n 10)
      (not (= n digit))
      ) 0]

    [(and
      (< n 10)
      (= n digit)) 1]
    
    [(= digit (remainder n 10))(add1 (count-occurrences (quotient n 10) digit))]
    [else (count-occurrences (quotient n 10) digit)]
   )
)

(= (count-occurrences 121 1) 2)
(= (count-occurrences 222 1) 0)
(= (count-occurrences 100 0) 2)
(= (count-occurrences 0 0) 1)
(= (count-occurrences 12705659 5) 2)
;my test
