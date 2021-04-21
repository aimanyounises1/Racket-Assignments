#lang pl 02

#|

<SE> :: <string>
        | {string-append String String}
        | {string-insert String String}
        | {num->string Number}
        

 |#
(define-type SE
  [st String]
  [st-append SE SE]
  [string-insert SE SE]
  [num->string Number])

