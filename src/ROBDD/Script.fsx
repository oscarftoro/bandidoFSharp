#load "Library.fs"
open ROBDD


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


//lets create a bdd (fig7 from andersens paper)
let t : T = Map.ofArray [|(0, (INF struct (5,None,None)))
                    ; (1, (INF struct (5,None,None)))
                    ; (2, (INF struct (4,Some(1),Some(0)) ))
                    ; (3, (INF struct (4,Some(0),Some(1)) ))
                    ; (4, (INF struct (3,Some(2),Some(3)) ))
                    ; (5, (INF struct (2,Some(4),Some(0)) ))
                    ; (6, (INF struct (2,Some(0),Some(4)) )) 
                    ; (7, (INF struct (1,Some(5),Some(6)) ))|]

//probably we couls add 0 and 1 at the end ? not sure yet
let h : H = Map.ofArray [|((INF struct (50,None,None))      ,0)
                        ; ((INF struct (51,None,None))      ,1)
                        ; ((INF struct (4,Some(1),Some(0)) ),2)
                        ; ((INF struct (4,Some(0),Some(1)) ),3)
                        ; ((INF struct (3,Some(2),Some(3)) ),4)
                        ; ((INF struct (2,Some(4),Some(0)) ),5)
                        ; ((INF struct (2,Some(0),Some(4)) ),6) 
                        ; ((INF struct (1,Some(5),Some(6)) ),7)|]
   
let Some (t0) = H.lookup (INF struct (50,None,None)) h 

//let us test the build function
let (h0,t0) : H * T = (Map.ofList [], Map.ofList [])

let u0, h1, t1 = 
//there is an error in build 
BDD.build (Or(Iff(X 1,X 2), X 3)) 3

(* it visits only the left part of the tree and die
see whether you can find something:
https://powerusers.microsoft.com/t5/General-Flow-Discussion/Input-Array-Was-Empty-but-it-is-not/td-p/66258

> BDD.build (Or(Iff(X 1,X 2), X 3)) 3;;
variable i : 4
variable bve : Or (Iff (B false,B false),B false)
variable b: true
variable i : 4
variable bve : Or (Iff (B false,B false),B true)
variable b: true
variable i : 4
variable bve : Or (Iff (B false,B true),B false)
variable b: false
variable i : 4
variable bve : Or (Iff (B false,B true),B true)
variable b: true
System.ArgumentException: The input array was empty.
Parameter name: array
*)