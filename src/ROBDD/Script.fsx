#load "Library.fs"

open ROBDD.BDD
open ROBDD.Types

let ex01 = Or(Iff(X 1, X 2), X 3)

let ex02 = expand ex01 1 true

let (ex03,ex04) = shannonExpand ex01 3

let ex05 = expand ex02 2 true
//ex06 is fully expanded, 
let ex06 = expand ex05 3 false

//therefore we can call now bve2be 
let be01 = bve2be ex06

//and therefore we can evaluate it
let result01 = eval be01 

