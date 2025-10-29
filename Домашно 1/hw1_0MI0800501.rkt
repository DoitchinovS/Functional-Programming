#lang racket
;Task 1

(define (count-digit num)
  (if (< num 10)
      1
      (add1 (count-digit (quotient num 10)))
  )
)

(define (digit-sum n res)
    (if(zero? n)
       res
       (digit-sum (quotient n 10) (+ res (remainder n 10)))
     )
 )

(define (find-endurance num)
  (define (helper leftover counter)
    (if (>= (digit-sum leftover 0) 10)
        (helper (digit-sum leftover 0) (add1 counter))
        counter
     )
   )
    (cond
      [(<= num 0)(error "The number has to be at least 1")]
      [(<= num 10) 0]
      [else (helper num 1)]
     )  
) 

(define (min-endurance-max-length start end)
  (define (helper beg fin current-num)
    (cond
      [(> beg fin) current-num]
      [(or
        (and
         (= (find-endurance beg) (find-endurance current-num))
         (> (count-digit beg) (count-digit current-num))
         )
        (< (find-endurance beg) (find-endurance current-num))
       )
       (helper (add1 beg) fin beg)
      ]
      [else (helper (add1 beg) fin current-num)]
    )
   )
  (helper (min start end) (max start end) (min start end))
)

;Tests task 1
(= (min-endurance-max-length 333 1000) 1000)
(= (min-endurance-max-length 333 2000) 1000)
(= (min-endurance-max-length 356 460) 360)
(= (min-endurance-max-length 498 701) 500)
(= (min-endurance-max-length 583 889) 600)
(= (min-endurance-max-length 34 621) 100)
(= (min-endurance-max-length 234 651) 234)

;Task 2

(define (list-of-nulls? xs)
  (cond
    [(null? xs) #t]
    [(not (zero? (car xs))) #f]
    [else (list-of-nulls? (cdr xs))]
   )
 )

(define (next-list xs)
 (if (null? (cdr xs))
       null
      (cons (- (cadr xs) (car xs)) (next-list (cdr xs)))
  )
)

(define (sum-predictions strategy data)
  (define (sum-all-predictions-forward xs)
    (if (null? xs)
        0
        (+ (sum-predictions-forward (car xs)) (sum-all-predictions-forward (cdr xs)))
        )
    )

  (define (sum-all-predictions-backward xs)
    (if (null? xs)
        0
        (+ (sum-predictions-backward (car xs)) (sum-all-predictions-backward (cdr xs)))
        )
    )
  (cond
    [(string=? strategy "forwards") (sum-all-predictions-forward data)]
    [(string=? strategy "backwards") (sum-all-predictions-backward data)]
    [else (error "Invalid command!")]
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

;Tests task 2
(= (sum-predictions "backwards" (list (list 10 13 16 21 30 45))) 5)
(= (sum-predictions "forwards" (list (list 10 13 16 21 30 45))) 68)
(= (sum-predictions "backwards" (list (list 7 9 12 16 21 27 34))) 6)
(= (sum-predictions "forwards" (list (list 7 9 12 16 21 27 34))) 42)
(= (sum-predictions "forwards" (list (list 7 9 12) (list 7 9 12 16) (list 7 9 12 16 21))) 64)
(= (sum-predictions "backwards" (list (list 21 30 45) (list 16 21 30 45) (list 13 16 21 30 45))) 41)
(= (sum-predictions "forwards"  (list (list 0 3 6 9 12 15) (list 1 3 6 10 15 21) (list 10 13 16 21 30 45))) 114)