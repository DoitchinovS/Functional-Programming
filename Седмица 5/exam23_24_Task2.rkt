#lang racket

(define (kth-number xs)
  (lambda (p? x)
    (if (> x (length (filter p? xs)))
        (error "No such number!")
        (list-ref (sort (filter p? xs) <) (sub1 x))
    )
  )
)

(= ((kth-number '(1 2 3 4 -5 6)) odd? 2) 1)
(= ((kth-number '(1 2 3 4 -5 6)) negative? 1) -5)
(= ((kth-number '(1 2 3 4 -5 -5 6)) negative? 2) -5)
(= ((kth-number '(1 -4 2 3 4 -5 -5 6)) negative? 3) -4)
((kth-number '(-1 0 -1 0 -2)) negative? 4) ;No such number
