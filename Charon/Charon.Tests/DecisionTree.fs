namespace Charon.Tests

module ``DecisionTree Tests`` =

    open NUnit.Framework
    open FsUnit

    open Charon
    open Charon.DecisionTree

    [<Test>]
    let ``Validate split`` () =

        let feature = [| 0; 0; 0; 1; 1; 1; |] |> prepare
        let labels  = [| 0; 0; 2; 1; 2; 2; |] |> prepare
        let result = split feature labels

        result.[0].[0] |> should equal [ 0; 1 ]
        result.[0].[1] |> should equal Index.empty
        result.[0].[2] |> should equal [ 2 ]

        result.[1].[0] |> should equal Index.empty
        result.[1].[1] |> should equal [ 3 ]
        result.[1].[2] |> should equal [ 4; 5 ]
    
    [<Test>]
    let ``Validate filterBy`` () =

        let feature = [| 0; 0; 0; 1; 1; 1; |] |> prepare
        let filter = [ 1; 3; 5 ]

        let filtered = feature |> filterBy filter
        filtered.[0] |> should equal [ 1 ]
        filtered.[1] |> should equal [ 3; 5 ]

    [<Test>]
    let ``Validate indexesOf`` () =

        let feature = [| 0; 0; 0; 1; 1; 1; |] |> prepare
        let filter = [ 1; 3; 5 ]

        (feature |> filterBy filter) |> indexesOf |> should equal filter

    [<Test>]
    let ``Validate prepare`` () =

        let feature = [| 0; 1; 2; 0; 1; 3; |] |> prepare
        
        feature.[0] |> should equal [ 0; 3 ]
        feature.[1] |> should equal [ 1; 4 ]
        feature.[2] |> should equal [ 2 ]
        feature.[3] |> should equal [ 5 ]

    [<Test>]
    let ``No features should return most likely class`` () =

        let labels = [| 0; 0; 0; 1; 1; 2 |] |> prepare
        let dataset = [ (0, labels) ] |> Map.ofList

        let tree = build dataset [ 0 .. 5 ] Set.empty any 0
        let x =
            match tree with
            | Branch(_) -> failwith "unexpected!"
            | Leaf(x) -> x 
        
        x |> should equal 0

    [<Test>]
    let ``Single informative features should return split on feature`` () =

        let feature = [| 0; 0; 0; 1; 1; 1 |] |> prepare
        let labels =  [| 1; 1; 1; 0; 0; 0 |] |> prepare
        let dataset = [ (0, labels); (1, feature) ] |> Map.ofList

        let tree = build dataset [ 0 .. 5 ] (Set.ofList [ 1 ]) any 0
        let x =
            match tree with
            | Branch(feat, subTree) -> feat, subTree
            | Leaf(_) -> failwith "unexpected!" 
        
        fst x |> should equal 1
        let subTree = snd x
        subTree.[0] |> should equal (Leaf(1))
        subTree.[1] |> should equal (Leaf(0))
