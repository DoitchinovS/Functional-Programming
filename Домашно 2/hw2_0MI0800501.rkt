#lang racket
(require racket/trace)


(define (find-position-list xs)
  (if (equal? (car xs) "S")
      0
      (add1 (find-position-list (cdr xs)))
   )
 )

(define (find-start player-map)
  (define (helper leftover result)
    (if (list? (member "S" (car leftover)))
        (cons result (find-position-list (car leftover)))
         (helper (cdr leftover) (add1 result))
    )
  )
  (helper player-map 0) 
)

(define (is-valid-pos pair-coord map)
   (and
        (>= (car pair-coord) 0)
        (>= (cdr pair-coord) 0)
        (< (car pair-coord) (length  map))
        (< (cdr pair-coord) (length (car map)))
    )
  )

(define (find-symbol coordinates xss)
  (list-ref ( list-ref xss (car coordinates)) (cdr coordinates))   
)

(define (get-next-steps player-map current-pos)
  (define (helper result)
    (let ([curr-symb (find-symbol current-pos player-map)]
          
          [add1-none
           (if (is-valid-pos (cons (add1 (car current-pos)) (cdr current-pos)) player-map  )
               (list (cons (add1 (car current-pos)) (cdr current-pos)))
               null
           )
           ]
          
          [none-add1
           (if (is-valid-pos (cons (car current-pos) (add1 (cdr current-pos))) player-map)
               (list (cons (car current-pos) (add1 (cdr current-pos))))
               null
           )
           ]
          
          [sub1-none
            (if (is-valid-pos (cons (sub1(car current-pos)) (cdr current-pos)) player-map)
                (list (cons (sub1(car current-pos)) (cdr current-pos)))
                null
           )
           ]
          
          [none-sub1
           (if (is-valid-pos (cons (car current-pos) (sub1 (cdr current-pos))) player-map)
               (list (cons (car current-pos) (sub1(cdr current-pos))))
               null
           )
          ]
          )
      (cond
        [(equal? curr-symb "|") (append result add1-none sub1-none)]
        [(equal? curr-symb "-") (append result none-add1 none-sub1)]
        [(equal? curr-symb "L") (append result sub1-none none-add1)]
        [(equal? curr-symb "J") (append result sub1-none none-sub1)]
        [(equal? curr-symb "7") (append result add1-none none-sub1)]
        [(equal? curr-symb "F") (append result add1-none none-add1)]
        [(equal? curr-symb ".") result]
        [(equal? curr-symb "S") (append result add1-none sub1-none none-add1 none-sub1)]
       )
     )
  )
  (helper null)
)
 
(define (num-steps-farthest-away map)
 (define (helper ending-list start-list result) 
   (if (equal? (last ending-list) (last start-list))
               (add1 result)
         (helper (remove-duplicates (filter  (lambda (x) (not (equal? x (find-start map))) )  (append ending-list (get-next-steps map (last ending-list))) ) )
                      (remove-duplicates  (filter (lambda (x) (not (equal? x (find-start map)))) (append start-list (get-next-steps map (last start-list))) ))
                      (add1 result)
          )
    )
  )
  (trace helper)
  (helper ( list (car (positions-after-start map))) ( list (last (positions-after-start map))) 0)
  
)


(define (positions-after-start map)
  (filter (lambda (x) (not (equal? "." (find-symbol x map))) ) (get-next-steps map (find-start map)))
 )



(= (num-steps-farthest-away '(("7" "-" "F" "7" "-")
("." "F" "J" "|" "7")
("S" "J" "L" "L" "7")
("|" "F" "-" "-" "J")
("L" "J" "." "L" "J"))) 8)

(= (num-steps-farthest-away '(("-" "." "|" "F" "7")
("." "S" "-" "7" "|")
("L" "|" "7" "|" "|")
("-" "L" "-" "J" "|")
("L" "|" "-" "J" "F"))) 4)

(= (num-steps-farthest-away '(("F" "-" "-" "7")
("|" "F" "-" "7")
("." "S" "." "|")
("|" "L" "-" "J")
("L" "-" "-" "J"))) 4)
