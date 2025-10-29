#lang racket

(define (repeater str)
  (lambda (count glue)
    (if (= count 1)
        str
        (string-append (string-append str glue) ((repeater str)(sub1 count) glue))
     )
   )
 )

(equal? ((repeater "I love Racket") 3 " ") "I love Racket I love Racket I love Racket")
(equal? ((repeater "Quack") 5 "!") "Quack!Quack!Quack!Quack!Quack")
(equal? ((repeater "Hi ") 3 "Simo ") "Hi Simo Hi Simo Hi ")
;my test