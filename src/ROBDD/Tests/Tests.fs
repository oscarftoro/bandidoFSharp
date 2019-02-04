module Tests

open ROBDD
open ROBDD.Types
open System
open Xunit
open Hedgehog


//module T Tests
let t : T = Map.ofArray [|(U 0, (INF0 struct (5,Zero,Zero)))
                        ; (U 1, (INF0 struct (5,One,One)))
                        ; (U 2, (INF struct (4,U 1, U 0) ))
                        ; (U 3, (INF struct (4,U 0, U 1) ))
                        ; (U 4, (INF struct (3,U 2, U 3) ))
                        ; (U 5, (INF struct (2,U 4, U 0) ))
                        ; (U 6, (INF struct (2,U 0, U 4) )) 
                        ; (U 7, (INF struct (1,U 5, U 6) ))|]

let h : H = Map.ofArray [|((INF0 struct (5,Zero,Zero)) ,U 0)
                      ; ((INF0 struct (5,One,One))   ,U 1)
                      ; ((INF  struct (4,U 1,U 0)),U 2)
                      ; ((INF  struct (4,U 0,U 1)),U 3)
                      ; ((INF  struct (3,U 2,U 3)),U 4)
                      ; ((INF  struct (2,U 4,U 0)),U 5)
                      ; ((INF  struct (2,U 0,U 4)),U 6) 
                      ; ((INF  struct (1,U 5,U 6)),U 7)|]

[<Fact>]
let ``T init test returns 2_keys`` () =
  let t = Map.ofList []
  let tInited = T.init 4 t;
  let size = tInited |> Map.toSeq |> Seq.map fst |> Seq.length

  Assert.Equal(2,size);

[<Fact>]
let ``T add add a node`` () =
  let t = Map.ofList []
  let tInited = T.init 5 t 
  let node = (INF struct (4,U (1),U (0)) )
  let (newU, newT) = T.add node tInited  
  let size = newT |> Map.toSeq |> Seq.map fst |> Seq.length

  Assert.Equal( 3, size)
  Assert.Equal(U 2,newU)

[<Fact>]
let ``T Add add a node and check get from t`` () =
  let t = Map.ofList []
  let tInited = T.init 5 t
  let node = (INF struct(4,U(1),U(0)) )
  let (newU, newT) = T.add node tInited 
  let expecting = Map.find (U 2) newT 

  Assert.Equal( (INF struct (4,U(1),U(0))), expecting  )
  
[<Fact>]
let ``T var check var `` () =
  let t = Map.ofList []
  let tInited = T.init 5 t
  let node = (INF struct (4,U(1),U(0)) )
  let (newT,newU) = T.add node tInited  
  let variable = T.v newT newU 

  Assert.Equal(4, variable)
 
[<Fact>]

let ``T low check low `` () =
  let t = Map.ofList []
  let tInited = T.init 5 t
  let node = (INF struct (4,U(1),U(0)) )
  let (newU, newT) = T.add node tInited 
  let low = T.low newU newT 

  Assert.Equal(U (1), low);

[<Fact>]
let ``T high check high `` () =
  let t = Map.ofList []
  let tInited = T.init 5 t
  let node = (INF struct (4,U(1),U(0)))
  let (newU,newT) = T.add node tInited
  let high = T.high newU newT

  Assert.Equal(U(0),high)
 
[<Fact>]
let ``H Init in h test returns 1key`` () =
  let h = Map.ofList []
  let hInited = H.init 4 h
  let size = hInited |>  Map.toArray |> Seq.map fst |> Seq.length 

  Assert.Equal(2, size)
  

[<Fact>]
let  ``H member test happy path`` () =
  let h = Map.ofList [] 
  let hInited = H.init 4 h
  let hNew = H.insert (INF struct (6,U(0),U(1))) (U 0) hInited
  let result = H.isMember (INF struct(6,U(0),U(1))) hNew

  Assert.Equal(true, result);
 
[<Fact>]
let ``H member_test_not_found`` () =
  let h = Map.ofList []
  let hInited = H.init 4 h
  let hNew = H.insert (INF struct (6,U(0),U(1))) (U 0) hInited
  let result = H.isMember (INF struct (5,U(0),U(1))) hNew

  Assert.Equal(false,result)
 
[<Fact>]
let ``H lookup test happy path`` () =
  let h = Map.ofList []
  let hInited = H.init 4 h
  let hNew = H.insert (INF struct (6,U(0),U(1))) (U 0) hInited
  let result = H.lookup (INF struct (6,U(0),U(1))) hNew

  Assert.Equal(Some (U(0)), result)

[<Fact>]
let ``lookup_test_reallity_check`` () =
  let h = Map.ofList []
  let hInited = H.init 4 h
  let hNew = H.insert (INF struct (6,U(0),U(1))) (U 0) hInited
  let result = H.lookup(INF struct (5,U(0),U(1))) hNew

  Assert.Equal(None, result)
  
[<Fact>]
let ``H insert test`` () =
  let h = Map.ofList [] 
  let hInited = H.init 4 h
  let hNew = H.insert (INF struct (3,U(0),U(1))) (U 2) hInited
  let result = H.isMember (INF struct (3,U(0),U(1))) hNew 

  Assert.Equal(true, result)



//mk

[<Fact>]
let ``Make six nodes `` () =
        let t = Map.ofList []
        let h = Map.ofList []
        let t0 = T.init 5 t
        let h0 = H.init 5 h
        let (_u1,t1,h1) = BDD.mk (INF struct (4,U(1),U(0)) )  t0 h0 
        let (_u2,t2,h2) = BDD.mk (INF struct (4,U(0),U(1)) )  t1 h1 
        let (_u3,t3,h3) = BDD.mk (INF struct (3,U(2),U(3)) )  t2 h2 
        let (_u4,t4,h4) = BDD.mk (INF struct (2,U(4),U(0)) )  t3 h3
        let (_u5,t5,h5) = BDD.mk (INF struct (2,U(0),U(4)) )  t4 h4 
        let (u6,t6,h6)  = BDD.mk (INF struct (1,U(5),U(6)) )  t5 h5 

        Assert.Equal(U 7,u6)
       

[<Fact>]
let ``produce an H from a T `` () =
  
  let hfromt = T.t2h t
  h = hfromt

[<Fact>]
let ``build should build a bdd based on a logic formula`` () =
  let (h0,t0) : H * T = (Map.ofList [], Map.ofList [])
  let u0, t1, h1 = BDD.build (Or(Iff(X 1,X 2), X 3)) 3
  let expectedT1 = Map.ofList [(U 0, INF0 struct (3, Zero, Zero))
                     ; (U 1, INF0 struct (3, One, One))
                     ; (U 2, INF struct (3, U 0, U 1)) 
                     ; (U 3, INF struct (2, U 1, U 2))
                     ; (U 4, INF struct (2, U 2, U 1))
                     ; (U 5, INF struct (1, U 3, U 4))]
  Assert.Equal<T>(expectedT1,t1)

[<Fact>]
let ``apply should apply a logic formula to a bdd`` () =
  let t2 : T = Map.ofArray [|(U 0, (INF0 struct (6,Zero,Zero)))
                           ; (U 1, (INF0 struct (6,One,One)))
                           ; (U 2, (INF  struct (3,U 0,U 1) ))
                           ; (U 3, (INF  struct (2,U 1,U 0) ))
                           ; (U 4, (INF  struct (2,U 0,U 1) ))
                           ; (U 5, (INF  struct (1,U 0,U 2) ))
                           ; (U 6, (INF  struct (1,U 3,U 4) )) |]

  let h2 : H = T.t2h t2
  let (i,t3,h3)  = BDD.apply (fun x y -> Conj(Bl x,Bl y)) (U 5) (U 6) t2 h2
  let expectedT2 = Map.ofArray [|(U 0, INF0 struct (6, Zero, Zero))
                         ; (U 1, INF0 struct (6, One, One))
                         ; (U 2, INF struct (3, U 0, U 1))
                         ; (U 3, INF struct (2, U 1, U 0))
                         ; (U 4, INF struct (2, U 0, U 1))
                         ; (U 5, INF struct (1, U 0, U 2))
                         ; (U 6, INF struct (1, U 3, U 4))
                         ; (U 7, INF struct (2, U 0, U 2))
                         ; (U 8, INF struct (1, U 0, U 7))
                        |]
  Assert.Equal<T>(expectedT2,t3)  

[<Fact>]
let ``restrict3 u i b t should restrict a bdd u assigning b to the variable i with table t `` () = 
  let t5 : T = Map.ofArray [|(U 0, (INF0 struct (5,Zero,Zero) ))
                         ; (U 1, (INF0 struct (5,One, One)  ))
                         ; (U 2, (INF struct (3,U 0,U 1)    ))
                         ; (U 3, (INF struct (2,U 1,U 2)    ))
                         ; (U 4, (INF struct (2,U 2,U 1)    ))
                         ; (U 5, (INF struct (1,U 3,U 4)    ))|]
  let (u6,t6,h6) = BDD.restrict3 (U 5) 2 0 t5
  let expectedT6 =  Map.ofList [ (U 0, INF0 struct (5, Zero, Zero))
                               ; (U 1, INF0 struct (5, One, One))
                               ; (U 2, INF  struct (3, U 0, U 1))
                               ; (U 3, INF  struct (1, U 1, U 2))]
  
  Assert.Equal<T>(expectedT6,t6)  

[<Fact>]
let ``restrict3 u i b t should restrict a bdd u assigning b to the variable i with table t table2`` () = 
  let t7 : T = 
    Map.ofArray [|(U 0, (INF0 struct (6, Zero, Zero) ))
                ; (U 1, (INF0 struct (6, One, One)   ))
                ; (U 2, (INF struct (3, U 0, U 1)    ))
                ; (U 3, (INF struct (3, U 1, U 0)    ))
                ; (U 4, (INF struct (2, U 0, U 2)    ))
                ; (U 5, (INF struct (2, U 3, U 2)    ))
                ; (U 6, (INF struct (1, U 4, U 5)    ))|]
  let (u8,t8,h8) = BDD.restrict3 (U 6) 2 1 t7
  let expectedT8 =  Map.ofList [ (U 0, INF0 struct (6, Zero, Zero))
                               ; (U 1, INF0 struct (6, One, One))
                               ; (U 2, INF  struct (3, U 0, U 1))]
  
  Assert.Equal<T>(expectedT8,t8)  

[<Fact>]
let ``restrict4 u i b t should restrict a bdd u assigning b to the variable i with table t `` () = 
  let t5 : T = Map.ofArray [|(U 0, (INF0 struct (5,Zero,Zero) ))
                         ; (U 1, (INF0 struct (5,One, One)  ))
                         ; (U 2, (INF struct (3,U 0,U 1)    ))
                         ; (U 3, (INF struct (2,U 1,U 2)    ))
                         ; (U 4, (INF struct (2,U 2,U 1)    ))
                         ; (U 5, (INF struct (1,U 3,U 4)    ))|]
  let (u6,t6,h6) = BDD.restrict4 (U 5) 2 0 t5
  let expectedT6 =  Map.ofList [ (U 0, INF0 struct (5, Zero, Zero))
                               ; (U 1, INF0 struct (5, One, One))
                               ; (U 2, INF  struct (3, U 0, U 1))
                               ; (U 3, INF  struct (1, U 1, U 2))]
  
  Assert.Equal<T>(expectedT6,t6)  

[<Fact>]
let ``restrict4 u i b t should restrict a bdd u assigning b to the variable i with table t table2`` () = 
  let t7 : T = 
    Map.ofArray [|(U 0, (INF0 struct (6, Zero, Zero) ))
                ; (U 1, (INF0 struct (6, One, One)   ))
                ; (U 2, (INF struct (3, U 0, U 1)    ))
                ; (U 3, (INF struct (3, U 1, U 0)    ))
                ; (U 4, (INF struct (2, U 0, U 2)    ))
                ; (U 5, (INF struct (2, U 3, U 2)    ))
                ; (U 6, (INF struct (1, U 4, U 5)    ))|]
  let (u8,t8,h8) = BDD.restrict4 (U 6) 2 1 t7
  let expectedT8 =  Map.ofList [ (U 0, INF0 struct (6, Zero, Zero))
                               ; (U 1, INF0 struct (6, One, One))
                               ; (U 2, INF  struct (3, U 0, U 1))]
  
  Assert.Equal<T>(expectedT8,t8)  

let propReverse : Property<Unit> =
    property {
        let! xs = Gen.list (Range.linear 0 100) Gen.alpha
        return xs |> List.rev |> List.rev = xs
        }