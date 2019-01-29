module Program = 
  open Tests  
  open ROBDD 
  open ROBDD.Types

  open Hedgehog
  let [<EntryPoint>] main _ = 
    let t : T = Map.ofArray [|(U 0, (INF0 struct (5,Zero,Zero)))
                    ; (U 1, (INF0 struct (5,One,One)))
                    ; (U 2, (INF struct (4,U(1),U 0) ))
                    ; (U 3, (INF struct (4,U(0),U 1) ))
                    ; (U 4, (INF struct (3,U(2),U 3) ))
                    ; (U 5, (INF struct (2,U(4),U 0) ))
                    ; (U 6, (INF struct (2,U(0),U 4) )) 
                    ; (U 7, (INF struct (1,U(5),U 6) ))|]
    
    let u0, h1, t1 = ROBDD.BDD.build (Or(Iff(X 1,X 2), X 3)) 3 

    let t2 : T = Map.ofArray [|(U 0, (INF0 struct (6,Zero,Zero)))
                    ; (U 1, (INF0 struct (6,One,One)  ))
                    ; (U 2, (INF struct  (3, U 0,U 1) ))
                    ; (U 3, (INF struct  (2, U 1,U 0) ))
                    ; (U 4, (INF struct  (2, U 0,U 1) ))
                    ; (U 5, (INF struct  (1, U 0,U 2) ))
                    ; (U 6, (INF struct  (1, U 3,U 4) )) |]
                  //  ; (7, (INF struct (1,Some(5),Some(6)) ))|]
    let h2 : H = Map.ofArray [|((INF0 struct (6,Zero, Zero)) , U 0 )
                             ; ((INF0 struct (6,One, One ))  , U 1 )
                             ; ((INF  struct (3,U 0,U 1))    , U 2 )
                             ; ((INF  struct (2,U 1,U 0))    , U 3 )
                             ; ((INF  struct (2,U 0,U 1))    , U 4 )
                             ; ((INF  struct (1,U 0,U 2))    , U 5 )
                             ; ((INF  struct (1,U 3,U 4))    , U 6 ) |]

    //Plot.t2dot t2;; //a unique table with two bdds
    let (i,t3,h3)  = BDD.apply (fun x y -> Conj(Bl x,Bl y)) (U 5) (U 6) t2 h2
      //We run our properties here...
      //Property.print Tests.propReverse

    let t5 : T = Map.ofArray [|(U 0, (INF0 struct (5,Zero,Zero) ))
                             ; (U 1, (INF0 struct (5,One, One)  ))
                             ; (U 2, (INF struct (3,U 0,U 1)    ))
                             ; (U 3, (INF struct (2,U 1,U 2)    ))
                             ; (U 4, (INF struct (2,U 2,U 1)    ))
                             ; (U 5, (INF struct (1,U 3,U 4)    ))|]
    
    let (u6,t6,h6) = BDD.restrict3 (U 5) 2 0 t5

    let t7 : T = Map.ofArray [|(U 0, (INF0 struct (6,Zero,Zero) ))
                         ; (U 1, (INF0 struct (6,One, One)  ))
                         ; (U 2, (INF struct (3,U 0,U 1)    ))
                         ; (U 3, (INF struct (3,U 1,U 0)    ))
                         ; (U 4, (INF struct (2,U 0,U 2)    ))
                         ; (U 5, (INF struct (2,U 3,U 2)    ))
                         ; (U 6, (INF struct (1,U 4,U 5)    ))|]

    let (u8,t8,h8) = BDD.restrict3 (U 6) 2 1 t7

    
    0
