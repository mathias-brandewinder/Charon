namespace Charon.Tests

module ``Continuous Tests`` =

    open NUnit.Framework
    open FsUnit

    open Charon
    open Charon.Continuous

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
        let (splits,value) = splitValue keys feature index |> Option.get
        splits  |> should equal [ 1.]