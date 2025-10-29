#lang racket

(define (rev-fold xs)
  (foldr (lambda (x acc) (+ (* 10 acc) x)) 0 xs)
  )

(define (rev-lin-iter xs)
  (define (ys-to-number ys res)
    (if (null? ys)
        res
        (ys-to-number (cdr ys) (+ (* 10 res) (car ys)))
    )
  )
  (ys-to-number (reverse xs) 0)
)
  
(= (rev-fold '(1 2 3)) 321)
(= (rev-fold '(1 2 3 4 5 6 7 8 9)) 987654321)
(= (rev-fold '(1 0 2 4 6)) 64201)
;my test
(= (rev-lin-iter '(1 2 3)) 321)
(= (rev-lin-iter '(1 2 3 4 5 6 7 8 9)) 987654321)
(= (rev-lin-iter '(1 0 2 7 8 3)) 387201)
;my test