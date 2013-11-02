#load "Continuous.fs"

open System
open Charon.Continuous

type Tree = 
    | Leaf of int // decision reached
    | CatBranch of int * Tree []
    | NumBranch of int * float list * Tree []

let rec decide tree ((cat:int[]),(num:float[])) =
    match tree with
    | Leaf(x) -> x
    | CatBranch(feat,trees) -> 
        let f = cat.[feat]
        decide (trees.[f]) (cat,num)
    | NumBranch(feat,cuts,trees) ->
        let f = num.[feat] // find value of feature
        let i = indexOf cuts f // index if value wrt cuts
        decide (trees.[i]) (cat,num)

let Test = 
    CatBranch(1,
        [|
            Leaf(2);
            CatBranch(0,
                [|
                    Leaf(0);
                    Leaf(1);
                |]);
            NumBranch(0,[1.;2.;],
                [|
                    Leaf(2);
                    Leaf(1);
                    Leaf(0);
                |])
        |])

type Feature<'a> =
    | Categorical of ('a -> string option)
    | Numerical   of ('a -> float option)

type Obs = { Float:float; Int:int; String:string }

let features = [
    (fun obs -> obs.Float |> Some) |> Numerical;
    (fun obs -> obs.Int |> float |> Some) |> Numerical;
    (fun obs -> obs.String |> Some) |> Categorical ]

let rng = Random()
let size = 100000
let keys = 5
let feature = [| for i in 1 .. size -> rng.NextDouble(), rng.Next(keys) |]

let filter = [| for i in 1 .. 10000 -> rng.Next(size) |]

let test = splitValue keys feature filter