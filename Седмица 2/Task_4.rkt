#lang racket
(require math/number-theory)

(define (reverse leftover result)
    (cond
      [(negative? leftover) (error "Invalid argument!")]
      [(zero? leftover) result]
      [else (reverse (quotient leftover 10)(+(* 10 result)(remainder leftover 10)))]
     )
 )

(define (palindrome? n)
  (= (reverse n 0) n)
)

(define (num-palindromes-iter lower-it upper-it)
  (define (helper curr-num count)
    (cond
      [(> curr-num
          (max lower-it upper-it)) count]
      [(palindrome? curr-num)(helper (add1 curr-num)(add1 count))]
      [else (helper(add1 curr-num) count)]
     )
   )
  
  (helper (min lower-it upper-it) 0)
)     

(define (num-palindromes-rec low up)
  (define (helper lower upper)
     (cond
       [(> lower upper) 0]
       [(palindrome? lower) (add1 (helper (add1 lower) upper))]
       [else (helper (add1 lower) upper)] 
     )
  )
  (helper (min low up)(max low up))
)


(= (num-palindromes-iter 1 101) 19)
(= (num-palindromes-iter 1 100) 18)
(= (num-palindromes-iter 100 1) 18)
(= (num-palindromes-iter 1 60) 14)
;my test

(= (num-palindromes-rec 1 101) 19)
(= (num-palindromes-rec 1 100) 18)
(= (num-palindromes-rec 100 1) 18)
(= (num-palindromes-rec 1 50) 13)
;my test