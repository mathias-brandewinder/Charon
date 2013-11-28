namespace Charon.Tests

open System
open NUnit.Framework
open FsUnit

open Charon.Entropy
open Charon.MDL
open Charon.Tree

[<TestFixture>]
type ``Experimental`` () =

    [<Test>]
    member test.``try out continuous tree`` () =

        let size = 10000
        let classes = 3

        let rng = Random()

        let outcomes = [| for i in 1 .. size -> rng.Next(classes) |]

        let features = 
            [|  yield outcomes |> Array.map (fun x -> (if x = 0 then Some(rng.NextDouble()) else Some(rng.NextDouble() + 1.)), x);
                yield outcomes |> Array.map (fun x -> (if x = 2 then Some(rng.NextDouble()) else Some(rng.NextDouble() + 1.)), x);
                yield outcomes |> Array.map (fun x -> Some(rng.NextDouble()), x); |]

        let dataset = { Classes = classes; Outcomes = outcomes; Features = features }
        let filter = [| 0 .. (size - 1) |]
        let remaining = [0;1;2] |> Set.ofList
        let selector = id

        let tree = growTree dataset filter remaining selector 5

        42 |> should equal 42