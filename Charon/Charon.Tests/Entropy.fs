namespace Charon.Tests

module ``Entropy Tests`` =

    open NUnit.Framework
    open FsUnit

    open Charon.Entropy
    open Charon.MDL

    [<Test>]
    let ``add counts should sum elements pair-wise`` () =

        let count1 = [| 0; 1; 2; |]
        let count2 = [| 3; 4; 5; |]

        add count1 count2 |> should equal [| 3; 5; 7; |]

    [<Test>]
    let ``add mismatched counts should fail`` () =

        let count1 = [| 0; 1; 2; |]
        let count2 = [| 3; 4; |]
        (fun () -> add count1 count2 |> ignore) |> should throw typeof<System.ArgumentException>

    [<Test>]
    let ``sub counts should sum elements pair-wise`` () =

        let count1 = [| 0; 1; 2; |]
        let count2 = [| 2; 1; 0; |]

        sub count1 count2 |> should equal [| -2; 0; 2; |]

    [<Test>]
    let ``sub mismatched counts should fail`` () =

        let count1 = [| 0; 1; 2; |]
        let count2 = [| 3; 4; |]
        (fun () -> sub count1 count2 |> ignore) |> should throw typeof<System.ArgumentException>

    [<Test>]
    let ``validate entropy`` () =

        let count = [| 1; 1; |]
        h count |> should equal (- log 0.5)

    [<Test>]
    let ``gain of even split should be zero`` () =

        let count1, count2 = [| 1; 1; |], [| 1; 1; |]
        gain (count1,count2) |> should equal 0.

    [<Test>]
    let ``trivial check MDL split value`` () =
        
        let count1, count2 = [| 100; 0; |], [| 0; 100; |]
        splitValue count1 count2 |> Option.get |> should greaterThan 0.

    [<Test>]
    let ``cla should count non empty counts`` () =
        
        let count = [| 10; 0; 20 |]
        cla count |> should equal 2
