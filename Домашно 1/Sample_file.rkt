#lang racket


(define (next-list xs)
 (if (null? (cdr xs))
       null
      (cons (- (cadr xs) (car xs)) (next-list (cdr xs)))
  )
)

(define (list-of-nulls? xs)
  (cond
    [(null? xs) #t]
    [(not (zero? (car xs))) #f]
    [else (list-of-nulls? (cdr xs))]
   )
 )

(define (sum-predictions-forward xs)
  (define (helper leftover res)
    (if (or
         (null? leftover)
         (list-of-nulls? leftover)
         )
        (foldl (lambda (x acc) (+ x acc)) 0 res)
        (helper (next-list leftover)  (append res (list (last leftover))))
     ) 
   )

  (helper xs null)
 )

(define (sum-all-predictions-forward xs)
    (if (null? xs)
        0
        (+ (sum-predictions-forward (car xs)) (sum-all-predictions-forward (cdr xs)))
     )
  )


(define (sum-predictions-backward ys)
  (define (helper leftover res count)
    (if (or
         (null? leftover)
         (list-of-nulls? leftover)
         )
        (foldl (lambda (x acc) (+ x acc)) 0 res)
        (helper (next-list leftover)  (append res (list (* (car leftover) (expt -1 count)) ) )  (add1 count) )
     ) 
  )
  (helper ys null 0)
)

(sum-all-predictions-forward (list (list 10 13 16 21 30 45)))
(sum-predictions-forward '(7 9 12))

(sum-predictions-backward '(7 9 12 16 21 27 34))