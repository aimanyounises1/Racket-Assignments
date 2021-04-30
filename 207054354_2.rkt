#lang pl 02

#|The grammar of the language
# Question 1.A
  # we will check if there is ambguity => and yes there is ambiguity 
ambiguity means we have different AST for each parse of SE


################ Alot of fixes to prevent the ambiguity #####################

<SE> ::<Str>           (1)
      |<D>                     (2)
      |<Char>                  (3)
      |{string <Char>}      (4)
      |{string-append <Str*> <Str>} (5)
      |{string-insert <Str> <Str> <Char> <D>}  (6)
      |{number->string <NUM>}  (7)
      |{string-length <Str>}   (8)


# infinite digits for representing the a number in a string
<D>::= <Digit> (9)
       |<Digit> <D>  (10)


# Represent the empty string
<Empty> ::= λ (11)
          

# Represent a String form digits only

<Str> ::= <"<D>"> (12)            # this will return the number in a String calling Digit every time. 
          |<Empty> <Str> (13)
          |<Empty>       (14)
          |{string-insert <Str> <Str> <Char> <D>} (15)
          |{number->string <NUM>}               (16)
          |{string <Char>}    (17)



<NUM> ::= |<D> (18) 
          |{string-length <Str>} (19) 


# Represent a Char of one Digit

<Char> ::= <CHR>    (20)
          |<CHR> <Char> (21)        (15)# This grammar is to apply the String #\2 #\3 etc..


#Characters from #\0 #\1 etc..

<CHR> ::= #\0 | #\1 | #\2 | #\3 | #\4 | #\5 | #\6 | #\7 | #\8 | #\9  (22)



# Digits from 0 - 9
<Digit>::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9    (23)



# this terminal to avoid ambiguity in append so instead of using <Str> <Str> in append I used <Str*> <Str> to just make sure
# that there is one deriviative


<Str*> ::= {string Char} (24)

           

 
# Example to derivative Question 1.B , I've used the number of each grammar to help you check.

1) "12344" => (12) (Str "D(1)
                        D(2)      
                            D(3)   
                                 D(4)
                                     D(4)") => "12344"

# Another example in parse
 
2) (parse ({string #\1 #\2 #\4}) (4) => (String (21)^2 (20)) # here call the (4) grammar then call Char grammar first one is number (21)
and the second one is (20)



#
3)(parse { string-append { string #\1 #\2 #\4 }  "12" }) (5) => { string-append (24)  "12" }  => { string-append (24)  "12" }  =>
 
{ string-append (24)  (12) } => { string-append (24)  (10) (9)} 




4)(parse( number->string ( string-length "0033344" ) )) => { number->string {(18)}} =>  { number->string {(12)}} => { number->string {(10)}}
{ number->string {(10)}} # and we got the result






############# Examples of invalid syntax in this grammar ########################

1)"a2b" => we can't get this result because there is no A and B characters in our alphabet


2)  12 13 4 67 => we can't this word from our grammar

3)( string 124 ) => {(4)} => we can't get numbers without #\v beacuase we will go to CHR grammar so we don't have this kind of words.

4)( string-append ( string-length "44" ) "12" ) =>  we don't have ( string-length "44" ) in our Str grammar

5) ( string-insert "1357" 4 66 )  => we can't apply that there is a terminal is missing.

|#

#|Question 2 |#
(: mult : Number -> Number)
; Multiply each element this function will be applied on each element on the list
(define (mult n)
  (* n n))

;Defining the function using lambda expression and and calculating the result with map using foldl every time map will multiply the number
(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares len)
  (let ([result (map mult len)]) ; Map will do => result = 1*1 => result =1 (using foldl)  result += 2*2 etc..   
    (foldl + 0 result))) 


;Tests to the function of sum-of-squares
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(1 2 4)) => 21)
(test (sum-of-squares '(1 3 3)) => 19)
(test (sum-of-squares '(1 2 3 5)) => 39)
(test (sum-of-squares '(2 2 2)) => 12)
(test (sum-of-squares '(1 2 3 5 6 7)) => 124)
(test (sum-of-squares '(1 2 3 5 6 7 8)) => (+ 124 (* 8 8)))
(test (sum-of-squares '(1 2 3 5 6 7 9)) => (+ 124 (* 9 9)))
; this function recieves a function and returns a function
(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs) 
  (: poly : (Listof Number) Number Integer Number ->
     Number)
  (define (poly argsL x power accum)
    (if (null? argsL) ;; return the accum of the list is null / empty 
        accum 
    (poly (rest argsL) x (+ power 1) (+ accum (* (first argsL) (expt x power)))))) ;create the polynom using tail-recursion with evaluation
  ;; and computing x value added to accum
  (: polyX : Number -> Number) ;; poly recieve an x and 
  (define (polyX x)
    (poly coeffs x 0 0)) ;; call poly with this parameters to start the tail recursion 
  polyX) ;; return the function

;; ######################### Test from our assigment ########################

(define p2345 (createPolynomial '(2 3 4 5)))
(test (p2345 0) =>
 (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5
(expt 0 3))))
(test (p2345 4) =>
 (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5
(expt 4 3))))
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4
(expt 11 2)) (* 5 (expt 11 3))))
(define p536 (createPolynomial '(5 3 6)))
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6
(expt 11 2))))
(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)


#|
The grammar was easy , but the first one Oh man , there was alot of terminals , LOL.
The grammar:
 <PLANG> ::= {{poly <AEs> }{<AEs> }} # The plang terminal .
 <AEs> ::= <AE> | <AE> <AEs>  # It's like <AE*>

 <AE> ::= <num>
 | <num> + <AE>
 | <num> - <AE>
 |#
(define-type PLANG
 [Poly (Listof AE) (Listof AE)])

;;this code was provided for us,
;;therefore i did not test it.
 (define-type AE
 [Num Number]
 [Add AE AE]
 [Sub AE AE]
 [Mul AE AE]
 [Div AE AE])

 (: parse-sexpr : Sexpr -> AE)
 ;; to convert s-expressions into AEs almost the same as we saw in the lecture. the parser to get the AST of AEs.
 (define (parse-sexpr sexpr)
 (match sexpr
 [(number: n) (Num n)]
 [(list '+ lhs rhs) (Add (parse-sexpr lhs)
 (parse-sexpr rhs))]
 [(list '- lhs rhs) (Sub (parse-sexpr lhs)
 (parse-sexpr rhs))]
 [(list '* lhs rhs) (Mul (parse-sexpr lhs)
 (parse-sexpr rhs))]
 [(list '/ lhs rhs) (Div (parse-sexpr lhs)
 (parse-sexpr rhs))]
[else (error 'parse-sexpr "bad syntax in ~s"
 sexpr)]))


 (: parse : String -> PLANG)
 ;; parse the Plang string to Plang AST.
 (define (parse str)
   (let ([code (string->sexpr str)])
 (match code
   ;; throw error if the first part is empty list.
   [(list (cons 'poly '()) (list t ...)) (error 'parse "at least one coefficient is
 required in ~s" code)]
      ;; throw error if the first part is empty list.
   [(list (cons 'poly h) '()) (error 'parse "at least one point is
 required in ~s" code)]
   ;;otherwise we assume a correct syntax and use 2 HOLY maps to parse each of list them (The AEs's gang).
   [(list (cons 'poly h) (list t ...)) (Poly (map parse-sexpr h) (map parse-sexpr t))]
   [else (error 'parse "bad syntax in ~s"
                code)])))


(test (parse "{{poly 1 2 3} {1 2 3}}")
 => (Poly (list (Num 1) (Num 2) (Num 3))
 (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly } {1 2} }") 
 =error> "parse: at least one coefficient is
 required in ((poly) (1 2))")
(test (parse "{{poly 1 2} {} }")
 =error> "parse: at least one point is
 required in ((poly 1 2) ())")
(test (parse "{{pct 1 2} {} }")
 =error> "parse: bad syntax in ((pct 1 2) ())")

#|Question 4b|#
;; evaluation of AE expressions to numbers
(: eval : AE ->  Number )
(define (eval expr)
(cases expr
[(Num n) n]
[(Add l r) (+ (eval l) (eval r))]
[(Sub l r) (- (eval l) (eval r))]
[(Mul l r) (* (eval l) (eval r))]
[(Div l r) (/ (eval l) (eval r))]))

(: eval-poly : PLANG -> (Listof Number) )
(define (eval-poly p-expr)
 (cases  p-expr ;; 1) check if the type is poly , then do the next.
   ;;2) evaluate all AEs and creat polynom
   ;; 3) use the returned function to caculate value at points with 3 maps (which are parsed from AEs's eval)
   
   [(Poly coeffs points) (map (createPolynomial (map eval coeffs)) (map eval points))]))

(: run : String -> (Listof Number))
;; evaluate a PLANG  in the string
(define (run str)
(eval-poly (parse str)))


(test (run "{{poly 1 2 3} {1 2 3}}")
=> '(6 17 34))
(test (run "{{poly 4 2 7} {1 4 9}}")
=> '(13 124 589))
(test (run "{{poly 1 2 3} {1 2 3}}")
=> '(6 17 34))
(test (run "{{poly 4/5 } {1/2 2/3 3}}")
=> '(4/5 4/5 4/5))
(test (run "{{poly 2 3} {4}}")
=> '(14))
(test (run "{{poly 1 1 0} {-1 3 3}}")
=> '(0 4 4))