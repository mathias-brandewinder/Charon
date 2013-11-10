type Feature<'a> =
    | Categorical of ('a -> string option)
    | Numerical   of ('a -> float option)

let preprocess dataset features =
    features
    |> List.map (fun feat ->
        match feat with
        | Numerical(f) -> Numerical(f), Map.empty
        | Categorical(f) ->
            Categorical(f),
            dataset 
            |> Seq.map f 
            |> Seq.distinct
            |> Seq.choose id
            |> Seq.mapi (fun i v -> v,i) 
            |> Map.ofSeq)

type Featurizer<'a> =
    | Categorize of ('a -> int option)
    | Numerize   of ('a -> float option)

type Value =
    | Int   of int option
    | Float of float option

let featurize x fs =
    fs 
    |> List.map (fun f ->
        match f with
        | Categorize(g) -> g(x) |> Int
        | Numerize(g)   -> g(x) |> Float)

let featurizers prepared = 
    prepared 
    |> List.map (fun (x,map) ->
        match x with 
        | Categorical(f) -> 
            fun a ->
                match (f a) with
                | None -> None 
                | Some(k) ->
                    if map |> Map.containsKey k 
                    then Some(map.[k]) 
                    else None
            |> Categorize
        | Numerical(f) -> Numerize(f))

let feat x f = 
    match f with
    | Categorize(g) -> g x |> Int
    | Numerize(g) -> g x |> Float

type BranchType =
    | Cat of int * int * Tree [] // feature, default, rest
    | Num of int * int * float list * Tree [] // feature, default, cuts, rest
and Tree =
    | Leaf of int
    | Branch of BranchType

let find cuts v =
    let rec walk cs i =
        match cs with
        | [] -> i
        | hd::tl -> 
            if v < hd then i else walk tl i+1
    walk cuts 0

let rec walk tree (obs:Value[]) =
    match tree with
    | Leaf(x) -> x
    | Branch(b) ->
        match b with
        | Cat(feat,def,tree) ->
            let value = obs.[feat]
            match value with
            | Int(x) -> 
                let v = 
                    match x with
                    | None -> def
                    | Some(v) -> v
                walk tree.[v] obs
            | _ -> failwith "Incorrect tree"
        | Num(feat,def,cuts,tree) -> 
            let value = obs.[feat]
            match value with
            | Float(x) ->
                let i =
                    match x with
                    | None -> def
                    | Some(v) -> find cuts v
                walk tree.[i] obs            
            | _ -> failwith "Incorrect tree"

// Demo

type Obs = { Float:float; Int:int; String:string }

let features = [
    (fun obs -> obs.Float |> Some) |> Numerical;
    (fun obs -> obs.Int |> string |> Some) |> Categorical;
    (fun obs -> obs.String |> Some) |> Categorical; ]

let data = [
    { Float = 1.; Int = 1; String = "Alpha" };
    { Float = 2.; Int = 2; String = "Alpha" };
    { Float = 3.; Int = 1; String = "Bravo" };
    { Float = 1.; Int = 1; String = "Charlie" };
    { Float = 2.; Int = 2; String = "Alpha" };
    { Float = 3.; Int = 2; String = "Charlie" }; ]

let obs = { Float = 1.; Int = 2; String = "Charlie" }

let converter x = 
    preprocess data features 
    |> featurizers
    |> List.map (fun f -> feat x f)
    |> List.toArray

converter obs

let tree = 
    Branch(Cat(2,1,
        [|  Leaf(0);
            Branch(Num(0,1,[1.5],
                [|  Leaf(1);
                    Leaf(0); |]));
            Branch(Cat(1,1,
                [|  Leaf(2);
                    Leaf(1); |]));
        |]))

let test = walk tree (obs |> converter)