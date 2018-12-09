module Tests

open ROBDD
open ROBDD.Types
open System
open Xunit
open Hedgehog


//module T Tests

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
    let node = (INF struct (4,Some(1),Some(0)) )
    let (newU, newT) = T.add node tInited  
    let size = newT |> Map.toSeq |> Seq.map fst |> Seq.length

    Assert.Equal(3, size)
    Assert.Equal(2,newU)

[<Fact>]
let ``T Add add a node and check get from t`` () =
    let t = Map.ofList []
    let tInited = T.init 5 t
    let node = (INF struct(4,Some(1),Some(0)) )
    let (newU, newT) = T.add node tInited 
    let expecting = Map.find 2 newT 
    
    Assert.Equal( (INF struct (4,Some(1),Some(0))), expecting  )
  
[<Fact>]
let ``T var check var `` () =
    let t = Map.ofList []
    let tInited = T.init 5 t
    let node = (INF struct (4,Some(1),Some(0)) )
    let (newT,newU) = T.add node tInited  
    let variable = T.v newT newU 

    Assert.Equal(4, variable)
 
[<Fact>]

let ``T low check low `` () =
    let t = Map.ofList []
    let tInited = T.init 5 t
    let node = (INF struct (4,Some(1),Some(0)) )
    let (newU, newT) = T.add node tInited 
    let low = T.low newU newT 

    Assert.Equal(Some (1), low);

[<Fact>]
let ``T high check high `` () =
    let t = Map.ofList []
    let tInited = T.init 5 t
    let node = (INF struct (4,Some(1),Some(0)))
    let (newU,newT) = T.add node tInited
    let high = T.high newU newT

    Assert.Equal(Some(0),high)
 
[<Fact>]
let ``H Init in h test returns 1key`` () =
   let h = Map.ofList []
   let hInited = H.init 4 h
   let size = hInited |>  Map.toArray |> Seq.map fst |> Seq.length 
   
   Assert.Equal(1, size)
  

[<Fact>]
let  ``H member test happy path`` () =
   let h = Map.ofList [] 
   let hInited = H.init 4 h
   let hNew = H.insert (INF struct (6,Some(0),Some(1))) 0 hInited
   let result = H.isMember (INF struct(6,Some(0),Some(1))) hNew

   Assert.Equal(true, result);
 
[<Fact>]
let ``H member_test_not_found`` () =
   let h = Map.ofList []
   let hInited = H.init 4 h
   let hNew = H.insert (INF struct (6,Some(0),Some(1))) 0 hInited
   let result = H.isMember (INF struct (5,Some(0),Some(1))) hNew

   Assert.Equal(false,result)
 
[<Fact>]
let ``H lookup test happy path`` () =
  let h = Map.ofList []
  let hInited = H.init 4 h
  let hNew = H.insert (INF struct (6,Some(0),Some(1))) 0 hInited
  let result = H.lookup (INF struct (6,Some(0),Some(1))) hNew

  Assert.Equal(Some(0), result)

[<Fact>]
let ``lookup_test_reallity_check`` () =
  let h = Map.ofList []
  let hInited = H.init 4 h
  let hNew = H.insert (INF struct (6,Some(0),Some(1))) 0 hInited
  let result = H.lookup(INF struct (5,Some(0),Some(1))) hNew

  Assert.Equal(None, result)
  
[<Fact>]
let ``H insert test`` () =
  let h = Map.ofList [] 
  let hInited = H.init 4 h
  let hNew = H.insert (INF struct (3,Some(0),Some(1))) 2 hInited
  let result = H.isMember (INF struct (3,Some(0),Some(1))) hNew 

  Assert.Equal(true, result)



//mk

[<Fact>]
let ``Make six nodes `` () =
        let t = Map.ofList []
        let h = Map.ofList []
        let t0 = T.init 5 t
        let h0 = H.init 5 h
        let (_u1,t1,h1) = BDD.mk (INF struct (4,Some(1),Some(0)) )  t0 h0 
        let (_u2,t2,h2) = BDD.mk (INF struct (4,Some(0),Some(1)) )  t1 h1 
        let (_u3,t3,h3) = BDD.mk (INF struct (3,Some(2),Some(3)) )  t2 h2 
        let (_u4,t4,h4) = BDD.mk (INF struct (2,Some(4),Some(0)) )  t3 h3
        let (_u5,t5,h5) = BDD.mk (INF struct (2,Some(0),Some(4)) )  t4 h4 
        let (u6,t6,h6)  = BDD.mk (INF struct (1,Some(5),Some(6)) )  t5 h5 

        Assert.Equal(7,u6)
       
let propReverse : Property<Unit> =
    property {
        let! xs = Gen.list (Range.linear 0 100) Gen.alpha
        return xs |> List.rev |> List.rev = xs
        }