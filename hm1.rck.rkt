#lang pl
;; the function declaration
( : append5 : Char Char Char Char Char -> String)
;; the function declaration 
;;( : permute3 : Char Char Char -> (Listof String))
;; the function.
(define (append5 c1 c2 c3 c4 c5)
  (string c1 c2 c3 c4 c5)
  )
( : permute3 : Char Char Char -> (Listof Any))
(define (permute3 c1 c2 c3)
  ;; convert all chars to string
 ;; (string c1)
  ;;(string c2)
  ;;(string c3)
  
  (list(list (string c1 c2 c3) (string c1 c3 c2)  (string c2 c1 c3)  (string c2 c3 c1)  (string c3 c1 c2)  (string c3 c2 c1)))
  )
  
( : count-3lists :(Listof Any)-> Natural)  
(define (count-3lists list)
 ( : helper-count-3lists :  Natural (Listof Any) -> Natural)
  (define (helper-count-3lists acc list) 
    (if(null? list)
      acc
       (helper-count-3lists (+ 1 acc ) list)))
  (helper-count-3lists 0 list))

(test (append5 #\e #\d #\c #\b #\a )=> "edcba")
(test (permute3 #\a #\b #\c) => '(("abc" "acb" "bac" "bca" "cab" "cba")))
(test (count-3lists '((1 3 4) (() (1 2 3)) ("tt"
"Three" 7) (2 4 6 8) (1 2 3))) => 3)
