type Tree = 
    | Leaf of int // decision reached
    | CatBranch of int * Tree []
    | NumBranch of int * float list * Tree []

let find cuts v =
    let rec walk cs i =
        match cs with
        | [] -> i
        | hd::tl -> 
            if v < hd then i else walk tl i+1
    walk cuts 0

let rec decide tree ((cat:int[]),(num:float[])) =
    match tree with
    | Leaf(x) -> x
    | CatBranch(feat,trees) -> 
        let f = cat.[feat]
        decide (trees.[f]) (cat,num)
    | NumBranch(feat,cuts,trees) ->
        let f = num.[feat] // find value of feature
        let i = find cuts f // index if value wrt cuts
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