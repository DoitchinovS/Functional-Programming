#lang racket


(define (num-bigger-elements xs)
  (map (lambda (x) (cons x (length (filter ( lambda (k) (> k x)) xs)))) xs)
 )



(equal? (num-bigger-elements '(5 6 3 4)) '((5 . 1) (6 . 0) (3 . 3) (4 . 2)))
(equal? (num-bigger-elements '(1 1 1)) '((1 . 0) (1 . 0) (1 . 0)))
