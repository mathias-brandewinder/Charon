namespace Charon.Tests

module ``Continuous Tests`` =

    open NUnit.Framework
    open FsUnit

    open Charon
    open Entropy

    [<Test>]
    let ``Validate splitValue`` () =
        let keys = 2
        let feature = [|
            (Some(0.),0);
            (Some(0.),0);
            (Some(0.),0);
            (Some(0.),0);
            (Some(0.),0);
            (Some(1.),1);
            (Some(1.),1);
            (Some(1.),1);
            (Some(1.),1);
            (Some(1.),1); |]
        let index = [| 0 .. 9 |]
        let initial = 
            feature 
            |> Seq.map snd           
            |> Seq.countBy id
            |> Seq.map snd
            |> Seq.toArray
            |> h

        let (splits,value) = Continuous.splitValue keys feature index 
        splits  |> should equal [ 1.]