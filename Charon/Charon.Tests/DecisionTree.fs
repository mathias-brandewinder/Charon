namespace Charon.Tests

module UnitTests =

    open NUnit.Framework
    open FsUnit
    open Charon.DecisionTree

    [<Test>]
    let ``Validate split`` () =

        let feature = [| 0; 0; 0; 1; 1; 1; |] |> prepare
        let labels  = [| 0; 0; 2; 1; 2; 2; |] |> prepare
        let result = split feature labels

        result.[0].[0] |> should equal (Set.ofList [ 0; 1 ])
        result.[0].[1] |> should equal (Set.empty)
        result.[0].[2] |> should equal (Set.ofList [ 2 ])

        result.[1].[0] |> should equal (Set.empty)
        result.[1].[1] |> should equal (Set.ofList [ 3 ])
        result.[1].[2] |> should equal (Set.ofList [ 4; 5 ])
    
    [<Test>]
    let ``Validate filterBy`` () =

        let feature = [| 0; 0; 0; 1; 1; 1; |] |> prepare
        let filter = Set.ofList [ 1; 3; 5 ]

        let filtered = feature |> filterBy filter
        filtered.[0] |> should equal (Set.ofList [ 1 ])
        filtered.[1] |> should equal (Set.ofList [ 3; 5 ])

    [<Test>]
    let ``Validate indexesOf`` () =

        let feature = [| 0; 0; 0; 1; 1; 1; |] |> prepare
        let filter = Set.ofList [ 1; 3; 5 ]

        (feature |> filterBy filter) |> indexesOf |> should equal filter