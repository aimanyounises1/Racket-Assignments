
#lang pl
#|
 the function declaration
|#
( : append5 : Char Char Char Char Char -> String)
#| the function declaration 
 the function. |#
(define (append5 c1 c2 c3 c4 c5)
  (string c1 c2 c3 c4 c5)
  )
#|
We append every 3 possible permutation characaters. 
|#
( : permute3 : Char Char Char -> (Listof Any))
(define (permute3 c1 c2 c3)
  (list(list (string c1 c2 c3) (string c1 c3 c2)  (string c2 c1 c3)  (string c2 c3 c1)  (string c3 c1 c2)  (string c3 c2 c1)))
  )
  
;; ########################  2.1 ####################################

#|We iterate throught the list in recursive way and check if every list is equal to 3 using a help function called list-length.|#
(: count-3lists : (Listof (Listof Any)) -> Number)
(define (count-3lists lst)
   (if(null? lst)
      0
      (if(eq? (list-length(first lst)) 3)
         (+ 1(count-3lists(rest lst)))
         (+ 0 (count-3lists(rest lst))))))

#|Here we use tail-recursion to iterate throught the list also we define the help-function in our function.
Which is considered to get 2 arrguments the counter and the list using the help-function length-list.|#
;; ################################ 2.2 ###############################
(: count-3lists-tail : (Listof (Listof Any)) -> Number)
(define (count-3lists-tail lst)
 (: helper-count-3lists-tail : Number (Listof (Listof Any)) -> Number)
 (define (helper-count-3lists-tail acc lst)
   (if(null? lst)
      acc
      (if(eq? (list-length(first lst)) 3)
         (helper-count-3lists-tail(+ 1 acc) (rest lst))
         (helper-count-3lists-tail(+ 0 acc) (rest lst)))))
  
(helper-count-3lists-tail 0 lst))


;;############# help function ################
#|Here is the help function that we used before in order to count the list with size 3.|#
(: list-length : (Listof Any) -> Number)
(define (list-length ls)
  (if(null? ls)
     0
     (+ 1(list-length(rest ls)))))
  
;;########################## 2.3 ###################################
#|We use the function count-3list as a help-function and using match pattern to check if the list is a list of list of list.|#
( : count-3listsRec : (Listof(Listof Any)) -> Number)
(define (count-3listsRec lst)
      (match lst
        [(list(list x ...) ...) (+ 1 (count-3lists lst))]
        ['()0]
        )
  )

#|Here is the tests .|#
(test (append5 #\e #\d #\c #\b #\a )=> "edcba")
(test (append5 #\H #\o #\m #\e #\! )=> "Home!")
(test (append5 #\W #\o #\r #\k #\! )=> "Work!")
(test (append5 #\T #\h #\i #\r #\d)=> "Third")
(test (append5 #\Y #\E #\A #\R #\!)=> "YEAR!")

(test (permute3 #\a #\b #\c) => '(("abc" "acb" "bac" "bca" "cab" "cba")))

(test (count-3lists '((1 3 4) (() (1 2 3)) ("tt"
                                            "Three" 7) (2 4 6 8) (1 2 3))) => 3)
(test (count-3lists-tail '((1 3 4) (() (1 2 3)) ("tt"
                                                 "Three" 7) (2 4 6 8) (1 2 3))) => 3)
(test (count-3listsRec '((1 3 4) (() (1 2 3)) ("tt"
"Three" 7) (2 4 6 8) (1 2 3))) => 4)
(test (count-3listsRec '((1 3 4) ((1 2) (1 2 3) ()) ("tt"
"Three" 7) (2 4 6 8) (1 2 3))) => 5)

(test (count-3listsRec '((1 3 4 2) (() (1 2 3)) ("tt"
"Three" 7) (2 4 6 8) (1 2 3))) => 3)
(test (count-3listsRec '((1 3 4) ((1 2 2) (1 2 3) (1 2 3)) ("tt"
"Three" 7) (2 4 6 8) (1 2 3))) => 5)

;; ###################### 3.1 ######################################

(define-type KeyStack
  [EmptyKS]
  [Push Symbol String KeyStack]
  )
(: search-stack : Symbol KeyStack -> (U String Boolean))
(define (search-stack s ks)
  (cases ks
    [(EmptyKS) #f]
    [(Push symb str st)
     (cond [(equal? s symb) str]
           [else (search-stack s st)])]))


(: pop-stack : KeyStack -> (U Boolean KeyStack))
(define (pop-stack ks)
  (cases ks
    [(EmptyKS) #f]
    [(Push symb str ks) ks]))

(test (EmptyKS) => (EmptyKS))

(test (Push 'b "B" (Push 'a "A" (EmptyKS))) =>
      (Push 'b "B" (Push 'a "A" (EmptyKS))))

(test (Push 'a "AIMAN" (Push 'y "YOUNIS" (EmptyKS)))
      =>(Push 'a "AIMAN" (Push 'y "YOUNIS"(EmptyKS))))

(test (EmptyKS) => (EmptyKS))
(test (Push 'h "H" (Push 'i "i" (EmptyKS))) => (Push 'h "H" (Push 'i "i" (EmptyKS))))
(test (Push 'y "Y" (Push 'o "O" (Push 'u "U" (EmptyKS)))) => (Push 'y "Y" (Push 'o "O" (Push 'u "U" (EmptyKS)))))
#|Test search stack|#
(test (search-stack 'a (Push 'a "AIMAN" (Push 'y "YOUNIS" (Push 'e ";)" (EmptyKS))))) => "AIMAN")
(test (search-stack 'a (Push 'a "A" (Push 'b "B" (Push 'a "AAA" (EmptyKS))))) => "A")
(test (search-stack 'd (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (search-stack 'd (Push 'n "Never" (Push 'b "back" (Push 'd "down" (EmptyKS))))) => "down")
#|test pop stack|#
(test (pop-stack (EmptyKS)) => #f)
(test (pop-stack (Push 'a "AIMAN" (Push 'y "YOUNIS" (EmptyKS)))) =>  (Push 'y "YOUNIS" (EmptyKS)))
(test (pop-stack (Push 'y "yes" (Push 'w "we" (Push 'c "can" (EmptyKS))))) => (Push 'w "we" (Push 'c "can" (EmptyKS))))
#|
   Qusetion 4
               |#

#|
 This two function works inn recursively way suppose we have 3 as input so the 3-1 is equal to 2 , and this number is even so we keep
 sending and the receiving the number back until we break at one of these two conditions. in our case we will return true because
The condition of is-even function will become true when the value is 0.
|#
(: is-odd? : Natural -> Boolean)
;; this functiom get as an input netural number and returns a boolean
;; The function's porpuse is to check if the given number is odd number by sending to is-even minus 1 
(define (is-odd? x)
(if (zero? x)
false
(is-even? (- x 1))))

(: is-even? : Natural -> Boolean)
#|
As we explained before this function is used to check if the number is even or odd
|#
(define (is-even? x)
(if (zero? x)
true
(is-odd? (- x 1))))

;; tests --- is-odd?/is-even?
(test (not (is-odd? 22)))
(test (is-even? 22))
(test (not (is-odd? 14)))
(test (is-even? 20))
(test (is-odd? 13))
(test (not (is-even? 3)))

(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
#|
In this function we get a function to use It's like lambda expression in CPP (just for explaination). 
The most improtant thing here that we get a function which returns boolean values and a list.
Which will be tested if it pass this pred/lambda/function or call it whatever you want.
The reurun statment is Boolean as descriped.
|#
(define (every? pred lst)
(or (null? lst)
(and (pred (first lst))
(every? pred (rest lst)))))

;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
#|
Checks all elements of the list if they pass the condition / function.
In other words if all elements in the list is even the function returns true.
Gets a list of numbers returns boolean value
|#
(define (all-even? lst)
(every? is-even? lst))

;; tests
(test (all-even? null))
(test (all-even? (list 256)))
(test (all-even? (list 2 4 8 16 32)))
(test (not (all-even? (list 1 2 3 5))));; the first 4 elements of fibonnaci :P
(test (not (all-even? (list 1 31 123))))
(test (not (all-even? (list 88 22 233 11))))


(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) -> Boolean))
#|The function recieves: function with A as input and boolean as output and function with B as input and boolean as output,
 And list of A's, list of B's. Returns boolean.
 And for all of this  A's B's need to be of the same type.
 The two functions returns boolean for some condition ((A -> Boolean)(B -> Boolean) )
|#
(define (every2? pred1 pred2 lst1 lst2)
(or (null? lst1) ;; both lists assumed to be of same length
(and (pred1 (first lst1))
(pred2 (first lst2))
(every2? pred1 pred2 (rest lst1) (rest lst2)))))