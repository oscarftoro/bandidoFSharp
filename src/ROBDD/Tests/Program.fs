module Program = 
  open Tests  
  open ROBDD 
  open ROBDD.Types

  open Hedgehog
  let [<EntryPoint>] main _ = 
    let t : T = Map.ofArray [|(0, (INF struct (5,None,None)))
                    ; (1, (INF struct (5,None,None)))
                    ; (2, (INF struct (4,Some(1),Some(0)) ))
                    ; (3, (INF struct (4,Some(0),Some(1)) ))
                    ; (4, (INF struct (3,Some(2),Some(3)) ))
                    ; (5, (INF struct (2,Some(4),Some(0)) ))
                    ; (6, (INF struct (2,Some(0),Some(4)) )) 
                    ; (7, (INF struct (1,Some(5),Some(6)) ))|]
    
    let u0, h1, t1 = ROBDD.BDD.build (Or(Iff(X 1,X 2), X 3)) 3 

    let t2 : T = Map.ofArray [|(0, (INF struct (6,None,None)))
                    ; (1, (INF struct (6,None,None)))
                    ; (2, (INF struct (3,Some(0),Some(1)) ))
                    ; (3, (INF struct (2,Some(1),Some(0)) ))
                    ; (4, (INF struct (2,Some(0),Some(1)) ))
                    ; (5, (INF struct (1,Some(0),Some(2)) ))
                    ; (6, (INF struct (1,Some(3),Some(4)) )) |]
                  //  ; (7, (INF struct (1,Some(5),Some(6)) ))|]
    let h2 : H = Map.ofArray [|((INF struct (6,None,None)))    ,0
                           ; ((INF struct (6,None,None)))    ,1
                           ; ((INF struct (3,Some(0),Some(1)),2 ))
                           ; ((INF struct (2,Some(1),Some(0)),3 ))
                           ; ((INF struct (2,Some(0),Some(1)),4 ))
                           ; ((INF struct (1,Some(0),Some(2)),5 ))
                           ; ((INF struct (1,Some(3),Some(4)),6 )) |]

    //Plot.t2dot t2;; //a unique table with two bdds
    let (i,t3,h3)  = BDD.apply (fun x y -> Conj(Bl x,Bl y)) 5 6 t2 h2
      //We run our properties here...
      //Property.print Tests.propReverse
    0
