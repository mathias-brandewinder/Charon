namespace Charon.Tests

module ``Index Tests`` =

    open NUnit.Framework
    open FsUnit

    open Charon

    [<Test>]
    let ``intersect of subset`` () =

        let index1 = [ 0 .. 10 ]
        let index2 = [ 1 .. 2 .. 9 ]

        Index.intersect index1 index2 |> should equal [ 1 .. 2 .. 9 ]
    
    [<Test>]
    let ``intersect of disjoint`` () =

        let index1 = [ 0; 1 ]
        let index2 = [ 3; 4 ]

        Index.intersect index1 index2 |> should equal Index.empty

    [<Test>]
    let ``intersect of overlapping`` () =

        let index1 = [ 0; 1; 2; 2; 4 ]
        let index2 = [ 2; 2; 3; 4; 4; 5 ]

        Index.intersect index1 index2 |> should equal [ 2; 2; 4 ]

    [<Test>]
    let ``Validate merge`` () =

        let index1 = [ 0 .. 5 ]
        let index2 = [ 1; 2; 2 ]

        Index.merge index1 index2 |> should equal [ 0; 1; 1; 2; 2; 2; 3; 4; 5 ]
        Index.merge index2 index1 |> should equal [ 0; 1; 1; 2; 2; 2; 3; 4; 5 ]

    [<Test>]
    let ``Validate out-of-bag`` () =

        Index.complement (0, 2) [ 0; 1; 2 ] |> should equal []
        
        Index.complement (0, 2) [ 0; 1; ] |> should equal [ 2 ]
        Index.complement (0, 2) [ 1; 2; ] |> should equal [ 0 ]
        Index.complement (0, 2) [ 0; 2; ] |> should equal [ 1 ]
        
        Index.complement (0, 2) [] |> should equal [ 0; 1; 2 ]
        
        Index.complement (0, 1) [ 0; 1; 2 ] |> should equal []
        Index.complement (1, 2) [ 0; 1; 2 ] |> should equal []

        Index.complement (0, 1) [ 2; 3; 4 ] |> should equal [ 0; 1; ]
        //Index.complement (5, 6) [ 2; 3; 4 ] 