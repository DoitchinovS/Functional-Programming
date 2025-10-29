#lang racket

(define (insert-at x i xs)
  (define (helper res leftover counter)
    (cond
     [(negative? i) (error "Index value has to be at least zero")]
     [(zero? counter) (append res (list x) leftover)]
     [else (helper (append res (list (car leftover))) (cdr leftover) (sub1 counter))]
    )
  )

  (helper null xs i)
)

(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))
(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))
(equal? (insert-at 7 0 '(1 2 3)) '(7 1 2 3))
(equal? (insert-at 7 1 '(1 2 3)) '(1 7 2 3))
(equal? (insert-at 7 3 '(1 2 3)) '(1 2 3 7))