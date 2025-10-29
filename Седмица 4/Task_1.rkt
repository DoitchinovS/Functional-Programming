#lang racket

(define (remove-all-proc x xs)
  (if (not (list? (member x xs)))
      xs
      (remove-all-proc x (remove x xs))
   )
)

(define (remove-all-no-proc x xs)
  (define (remove-first-occ x xs)
    (cond
      [(null? xs) null]
      [(equal? x (car xs)) (cdr xs)]
      [else (cons (car xs)(remove-first-occ x (cdr xs)))]
     )
    )
  (remove-first-occ x (remove-duplicates xs))
 )

(equal? (remove-all-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-proc 1 '(1)) '())
(equal? (remove-all-proc 1 '(1 2 1 1)) '(2))
(equal? (remove-all-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))
(equal? (remove-all-proc 3 '(3 4 5 3)) '(4 5))
;my test
(equal? (remove-all-no-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-no-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-no-proc 1 '(1)) '())
(equal? (remove-all-no-proc 1 '(1 2 1 1)) '(2))
(equal? (remove-all-no-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))
(equal? (remove-all-no-proc 4 '(1 2 3 4 5 6 4 7)) '(1 2 3 5 6 7))
;my test
