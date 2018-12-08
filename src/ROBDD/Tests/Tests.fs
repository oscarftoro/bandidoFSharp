module Tests

open ROBDD
open Hedgehog
open System
open Xunit

[<Fact>]
let ``My test`` () =
    Assert.True(true)


[<Fact>]
let propReverse : Property<Unit> =
    property {
        let! xs = Gen.list (Range.linear 0 100) Gen.alpha
        return xs |> List.rev |> List.rev = xs
        }