#lang pl 02

#|The grammar of the language
# Question 1.A

<SE> ::<Str>
      |<Empty>
      |<D>
      |<Char>
      |{string <SE> <SE>}
      |{string-append <SE> <SE>}
      |{string-insert <SE> <SE>}
      |{number->string <SE> <SE>}
      |{string-length <SE> <SE>}


# infinite digits for representing the a number in a string
<D>::= <Digit>
       |<Digit> <D>


# Represent the empty string
<Empty> ::= λ
          

# Represent a String form digits only

<Str> ::= <"<D>"> # this will return the number in a String calling Digit every time. 
     
# Represent a Char of one Digit

<Char> ::= <CHR>
          |<CHR> <Char> # This grammar is to apply the String #\2 #\3 etc..

#Characters from #\0 #\1 etc..
<CHR> ::= #\0 | #\1 | #\2 | #\3 | #\4 | #\5 | #\6 | #\7 | #\8 | #\9

# Digits from 0 - 9
<Digit>::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
           

 
# Example to derivative Question 1.B

{"12344"} => (Str "D(1)
                        D(2)      
                            D(3)   
                                 D(4)
                                     D(4)") => "12344"

(parse ({string #\1 #\2 #\4}) => (String
                                        CHR(1) Char #\2 #\4 
                                              CHR(2)  Char #\2
                                                    CHR(4)) => "124"
 
(parse { string-append { string #\1 #\2 #\4 }  "12" }) => (string-append(string CHR(1)
                                                                                       CHR(2)
                                                                                               CHR(4))
                                
  #(1979 3)                                                                     ("D(1)
                                                                                                              D(2)")) => "12412"
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



(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(1 2 4)) => 21)
(test (sum-of-squares '(1 3 3)) => 19)
(test (sum-of-squares '(1 2 3 5)) => 39)
(test (sum-of-squares '(2 2 2)) => 12)
(test (sum-of-squares '(1 2 3 5 6 7)) => 124)
(test (sum-of-squares '(1 2 3 5 6 7 8)) => (+ 124 (* 8 8)))
(test (sum-of-squares '(1 2 3 5 6 7 9)) => (+ 124 (* 9 9)))

(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
  (: poly : (Listof Number) Number Integer Number ->
     Number)
  (define (poly argsL x power accum)
    (if (null? argsL)
        accum 
    (poly (rest argsL) x (+ power 1) (+ accum (* (first argsL) (expt x power))))))
  (: polyX : Number -> Number)
  (define (polyX x)
    (poly coeffs x 0 0))
  polyX)



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
The grammar:
 <PLANG> ::= {{poly <AEs> }{<AEs> }}
 <AEs> ::= <AE> | <AE> <AEs>
 <AE> ::= same as described in class
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
 ;; to convert s-expressions into AEs
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
 ;; parses a string containing a PLANG expressionto a PLANG AST
 (define (parse str)
   (let ([code (string->sexpr str)])
 (match code
   ;; if pattern is poly followed by empty list we throw error
   [(list (cons 'poly '()) (list tai ...)) (error 'parse "at least one coefficient is
 required in ~s" code)]
      ;; if pattern is ((poly $non empty list$) $empty list$) we throw error
   [(list (cons 'poly hea) '()) (error 'parse "at least one point is
 required in ~s" code)]
   ;;otherwise we assume current syntax and use map to parse each of the list "AEs"
   [(list (cons 'poly hea) (list tai ...)) (Poly (map parse-sexpr hea) (map parse-sexpr tai))]
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
;; evaluates AE expressions to numbers
;; this question was actually pretty easy 20 minitues
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
 (cases  p-expr
   ;; if we have Poly type of variant
   ;; we create polynomial from evaluating all "AES" in the first list
   ;; then we use the returned function to caculate value at point (which are parsed from AEs with map and eval)
   
   [(Poly coeffs points) (map (createPolynomial (map eval coeffs)) (map eval points))]))

(: run : String -> (Listof Number))
;; evaluate a FLANG program contained in a string
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