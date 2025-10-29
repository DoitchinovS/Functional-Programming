#lang racket

(define (longest str1 str2)
  (list->string
   (sort
    (remove-duplicates
     (string->list (string-append str1 str2))
     )
    char<?
    )
   )
 )

(equal? (longest "xyaabbbccccdefww" "xxxxyyyyabklmopq") "abcdefklmopqwxy")
(equal? (longest "abcdefghijklmnopqrstuvwxyz" "abcdefghijklmnopqrstuvwxyz") "abcdefghijklmnopqrstuvwxyz")
(equal? (longest "kkkhhhabcdfgghh" "optfjhlkj") "abcdfghjklopt")
;my test