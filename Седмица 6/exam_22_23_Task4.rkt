#lang racket

(define (find-position-1 xs)
  (if (= (car xs) 1)
      0
      (add1 (find-position-1 (cdr xs)))
   )
)

(define (steps-bm xss)
   (define (helper leftover result)
     (if (list? (member 1 (car leftover)))
         (+ (abs (- result 2)) (abs (-  (find-position-1 (car leftover)) 2)))
         (helper (cdr leftover) (add1 result))
     )
   )
  (helper xss 0)
)

(steps-bm '((0 0 0 0 0)
            (0 0 0 0 1)
            (0 0 0 0 0)
            (0 0 0 0 0)
            (0 0 0 0 0)) )

(steps-bm '((0 0 0 0 0)
            (0 0 0 0 0)
            (0 1 0 0 0)
            (0 0 0 0 0)
            (0 0 0 0 0)) )

(steps-bm '((0 0 0 0 0)
            (0 0 0 0 0)
            (0 0 1 0 0)
            (0 0 0 0 0)
            (0 0 0 0 0)) )

(steps-bm '((0 0 0 0 0)
            (0 0 0 0 0)
            (0 0 0 0 0)
            (0 0 0 0 0)
            (0 0 0 0 1)) )
