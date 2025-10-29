#lang racket

(define (around-fib n)
  (lambda (k)
    (map find-max-pair (separate-groups (num-to-xs (find-nth-fibonacci n)) k))
  )
)

(define (num-to-xs n)
  (define (helper result leftover)
    (if (zero? leftover)
        result
        (helper (cons (remainder leftover 10) result) (quotient leftover 10))
     )
    )
  (helper null n)
)

(define (find-nth-fibonacci n)
  (define (helper a b counter)
    (if (zero? counter)
        a
        (helper b (+ a b) (sub1 counter))
     )
   )
     (helper 0 1 n)
)

(define (separate-groups xs k)
  (define (helper leftover res-list)
    (if (<= (length leftover) k)
       (reverse (cons  leftover res-list))
        (helper (drop leftover k) (cons (take leftover k) res-list))
     )
   )
  (helper xs null)
 )

(define (find-max-pair xs)
  (define (helper leftover digit-max occurrences-max)
    (let ([next (dropf leftover (lambda (x) (= x (car leftover))))])
        (cond
          [(null? leftover) (cons digit-max occurrences-max)]
          [(> (length (takef leftover (lambda (x) (= x (car leftover)))))
              occurrences-max)
           (helper next (car leftover) (length (takef leftover (lambda (x) (= x (car leftover))))))
           ]
          [else (helper next digit-max occurrences-max)]
        )
    )
   )
  (helper (sort xs <) 0 0)
)

((around-fib 100) 25)
((around-fib 180) 25)
((around-fib 1700) 25)
((around-fib 500) 42)
((around-fib 6000) 242)