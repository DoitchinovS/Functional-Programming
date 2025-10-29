#lang racket

(define (shuffle-merge xs ys)
  (define (helper  leftover1 leftover2 counter result)
    (cond
      [(or (null? leftover1) (null? leftover2)) (append result leftover1 leftover2)]
      [(even? counter) (helper (cdr leftover1) leftover2 (add1 counter) (append result (list (car leftover1))))]
      [else (helper leftover1 (cdr leftover2) (add1 counter) (append result (list (car leftover2))))]
    )
  )
  (helper xs ys 0 null)
)

(define (shuffle-merge2 xs ys)
  (define (helper leftover1 leftover2)
    (cond
      [(null? xs) ys]
      [(null? ys) xs]
      [else (cons (car leftover1) (shuffle-merge2 leftover2 (cdr leftover1)))]
     )
  )
  (helper xs ys)
 )



(equal? (shuffle-merge '(1) '()) '(1))
(equal? (shuffle-merge '(3 4 5) '(2)) '(3 2 4 5))
(equal? (shuffle-merge '(3 4 5) '(9 2)) '(3 9 4 2 5))
(equal? (shuffle-merge '(3 2 8) '(5 6 1 9 11)) '(3 5 2 6 8 1 9 11))

(equal? (shuffle-merge2 '(1) '()) '(1))
(equal? (shuffle-merge2 '(3 4 5) '(2)) '(3 2 4 5))
(equal? (shuffle-merge2 '(3 4 5) '(9 2)) '(3 9 4 2 5))
(equal? (shuffle-merge2 '(3 2 8) '(5 6 1 9 11)) '(3 5 2 6 8 1 9 11))