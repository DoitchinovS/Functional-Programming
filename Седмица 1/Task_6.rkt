#lang racket


(define (leap-year-one-line? year1)
  (or (and (zero? (remainder year1 4))(not (zero? (remainder year1 100))))  (zero? (remainder year1 400)))
)

(define (leap-year-guards? year2)
  (cond
    [(negative? year2)(error "This is not an actual year!")]
    [(zero? (remainder year2 400)) #t]
    [(zero? (remainder year2 100)) #f]
    [(zero? (remainder year2 4)) #t]
    [else #f]
    )
)

(equal? (leap-year-one-line? 2020) #t)
(equal? (leap-year-one-line? 1988) #t)
(equal? (leap-year-one-line? 1600) #t)
(equal? (leap-year-one-line? 2400) #t)
(equal? (leap-year-one-line? 2023) #f)
(equal? (leap-year-one-line? 1700) #f)
(equal? (leap-year-one-line? 1800) #f)
(equal? (leap-year-one-line? 2100) #f)
(equal? (leap-year-one-line? 1940) #t)
;my test

(equal? (leap-year-guards? 2020) #t)
(equal? (leap-year-guards? 1988) #t)
(equal? (leap-year-guards? 1600) #t)
(equal? (leap-year-guards? 2400) #t)
(equal? (leap-year-guards? 2023) #f)
(equal? (leap-year-guards? 1700) #f)
(equal? (leap-year-guards? 1800) #f)
(equal? (leap-year-guards? 2100) #f)
(equal? (leap-year-guards? -20)#f)
; my test



