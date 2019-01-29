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
let t : T = Map.ofArray [|(U 0, (INF0 struct (5,Zero,Zero)))
                        ; (U 1, (INF0 struct (5,One,One)))
                        ; (U 2, (INF struct (4,U 1, U 0) ))
                        ; (U 3, (INF struct (4,U 0, U 1) ))
                        ; (U 4, (INF struct (3,U 2, U 3) ))
                        ; (U 5, (INF struct (2,U 4, U 0) ))
                        ; (U 6, (INF struct (2,U 0, U 4) )) 
                        ; (U 7, (INF struct (1,U 5, U 6) ))|]

//probably we couls add 0 and 1 at the end ? not sure yet
let h : H = Map.ofArray [|((INF0 struct (5,Zero,Zero)) ,U 0)
                        ; ((INF0 struct (5,One,One))   ,U 1)
                        ; ((INF  struct (4,U 1,U 0)),U 2)
                        ; ((INF  struct (4,U 0,U 1)),U 3)
                        ; ((INF  struct (3,U 2,U 3)),U 4)
                        ; ((INF  struct (2,U 4,U 0)),U 5)
                        ; ((INF  struct (2,U 0,U 4)),U 6) 
                        ; ((INF  struct (1,U 5,U 6)),U 7)|]
   
let Some (t0) = H.lookup (INF0 struct (5,Zero,Zero)) h 

//let us test the build function
let (h0,t0) : H * T = (Map.ofList [], Map.ofList [])

let u0, t1, h1 = BDD.build (Or(Iff(X 1,X 2), X 3)) 3

Plot.t2dot t1;;


// let us excercise the super powers of apply

let t2 : T = Map.ofArray [|(U 0, (INF0 struct (6,Zero,Zero)))
                         ; (U 1, (INF0 struct (6,One,One)))
                         ; (U 2, (INF  struct (3,U 0,U 1) ))
                         ; (U 3, (INF  struct (2,U 1,U 0) ))
                         ; (U 4, (INF  struct (2,U 0,U 1) ))
                         ; (U 5, (INF  struct (1,U 0,U 2) ))
                         ; (U 6, (INF  struct (1,U 3,U 4) )) |]
                  //  ; (7, (INF struct (1,Some(5),Some(6)) ))|]
let h2 : H = Map.ofArray [|((INF0 struct (6,Zero,Zero))), U 0
                         ; ((INF0 struct (6,One,One)))  , U 1
                         ; ((INF struct (3,U 0, U 1)    , U 2 ))
                         ; ((INF struct (2,U 1, U 0)    , U 3 ))
                         ; ((INF struct (2,U 0, U 1)    , U 4 ))
                         ; ((INF struct (1,U 0, U 2)    , U 5 ))
                         ; ((INF struct (1,U 3, U 4)    , U 6 )) |]

Plot.t2dot t2;; //a unique table with two bdds test4
let (i,t3,h3)  = BDD.apply (fun x y -> Conj(Bl x,Bl y)) (U 5) (U 6) t2 h2
Plot.t2dot t3;; //test05

let t4 : T = Map.ofArray [|(U 0, (INF0 struct (8,Zero,Zero)))
                         ; (U 1, (INF0 struct (8,One,One)  ))
                         ; (U 2, (INF  struct (5,U 1, U 0) ))
                         ; (U 3, (INF  struct (4,U 2, U 0) ))
                         ; (U 4, (INF  struct (4,U 0, U 2) ))
                         ; (U 5, (INF  struct (3,U 3, U 4) ))
                         ; (U 6, (INF  struct (2,U 5, U 0) )) 
                         ; (U 7, (INF  struct (2,U 0, U 5) ))
                         ; (U 8, (INF  struct (1,U 6, U 7) ))
                         ; (U 6, (INF  struct (2,U 3, U 4) ))|]

// let us test restrict 
let t5 : T = Map.ofArray [|(U 0, (INF0 struct (5,Zero,Zero) ))
                         ; (U 1, (INF0 struct (5,One, One)  ))
                         ; (U 2, (INF struct (3,U 0,U 1)    ))
                         ; (U 3, (INF struct (2,U 1,U 2)    ))
                         ; (U 4, (INF struct (2,U 2,U 1)    ))
                         ; (U 5, (INF struct (1,U 3,U 4)    ))|]
// and ensure we are doing what we are supposed to do...
Plot.t2dot t5
//and yes, in test06.svg we show a cool result of the boolean expression:
// (x1  <=> x2) \/ x3

let (u6,t6,h6) = BDD.restrict3 (U 5) 2 0 t5
 // BDD.restrict (U 0) 2 0 t5 //yea, this requires an H table and it seems to me that this 0 at the begining is unnecessary


Plot.t2dot t6;;

// from symbolic boolean manipulation with Ordered Binary Decision Diagrams
// page 14
let t7 : T = Map.ofArray [|(U 0, (INF0 struct (6,Zero,Zero) ))
                         ; (U 1, (INF0 struct (6,One, One)  ))
                         ; (U 2, (INF struct (3,U 0,U 1)    ))
                         ; (U 3, (INF struct (3,U 1,U 0)    ))
                         ; (U 4, (INF struct (2,U 0,U 2)    ))
                         ; (U 5, (INF struct (2,U 3,U 2)    ))
                         ; (U 6, (INF struct (1,U 4,U 5)    ))|]


Plot.t2dot t7;;


let (u8,t8,h8) = BDD.restrict3 (U 6) 2 1 t7

Plot.t2dot t8;;

//testing the error
// u0 = U 4
let tm = 
  Map.ofList [(U 0, INF0 struct (6, Zero, Zero))
            ; (U 1, INF0 struct (6, One, One))
            ; (U 2, INF  struct (3, U 0, U 1))
            ; (U 3, INF  struct (3, U 1, U 0))
            ; (U 4, INF  struct (2, U 0, U 2))
            ; (U 5, INF  struct (2, U 3, U 2))
            ; (U 6, INF  struct (1, U 4, U 5))]
 
