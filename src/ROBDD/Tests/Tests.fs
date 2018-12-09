module Tests

open ROBDD
open ROBDD.Types
open System
open Xunit
open Hedgehog

[<Fact>]
let ``My test`` () =
    Assert.True(true)


 //T

[<Fact>]
let ``Init test returns 2_keys`` () =
      let t = Map.ofList []
      let tInited = T.init t 4;
      let size = tInited |> Map.toSeq |> Seq.map fst |> Seq.length

      Assert.Equal(2,size);
  

[<Fact>]
let ``Make six nodes `` () =
        let t = Map.ofList []
        let h = Map.ofList []
        let t0 = T.init t 5
        let h0 = H.init h 5
        let (t1,h1,u1) = BDD.mk t0 h0 (INF struct (4,Some(1),Some(0) ) )
        let (t2,h2,u2) = BDD.mk t1 h1 (INF struct (4,Some(0),Some(1) ) )
        let (t3,h3,u3) = BDD.mk t2 h2 (INF struct (3,Some(2),Some(3) ) )
        let (t4,h4,u4) = BDD.mk t3 h3 (INF struct (2,Some(4),Some(0) ) )
        let (t5,h5,u5) = BDD.mk t4 h4 (INF struct (2,Some(0),Some(4) ) )
        let (t6,h6,u6) = BDD.mk t5 h5 (INF struct (1,Some(5),Some(6) ) )

        Assert.Equal(7,u6)

let propReverse : Property<Unit> =
    property {
        let! xs = Gen.list (Range.linear 0 100) Gen.alpha
        return xs |> List.rev |> List.rev = xs
        }