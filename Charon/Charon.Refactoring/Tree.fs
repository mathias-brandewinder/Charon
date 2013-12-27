namespace Charon.Refactoring

module Tree =

    open Charon.Refactoring.Featurization

    type NumBranch = { FeatIndex: int; Default: int; Splits: float list; }
    type CatBranch = { FeatIndex: int; Default: int; }

    type BranchType =
        | Cat of CatBranch * Tree []
        | Num of NumBranch * Tree []
    and Tree =
        | Leaf of int
        | Branch of BranchType

    let indexOf splits value =
        let rec walk splits index =
            match splits with
            | [] -> index
            | hd::tl ->
                if value < hd
                then index
                else walk tl (index+1)
        walk (splits |> List.sort) 0

    let rec decide (tree: Tree) (observation: Value []) =
        match tree with
        | Leaf(v)   -> v
        | Branch(t) ->
            match t with
            | Cat(branch,next) ->
                let value = observation.[branch.FeatIndex]
                match value with
                | Int(i) ->
                    let index = 
                        match i with 
                        | None    -> branch.Default
                        | Some(x) -> x
                    decide next.[index] observation
                | _ -> failwith "Observation and tree do not match"
            | Num(branch,next) ->
                let value = observation.[branch.FeatIndex]
                match value with
                | Float(f) ->
                    let index = 
                        match f with 
                        | None    -> branch.Default
                        | Some(x) -> indexOf branch.Splits x
                    decide next.[index] observation
                | _ -> failwith "Observation and tree do not match"              

    let private pad (actives: int Set) (depth: int) =
        System.String.Join("", 
            [|  for i in 0 .. (depth - 1) do 
                    if (actives.Contains i) 
                    then yield "│   " 
                    else yield "   " |])

    // TEMPORARY, FIX THIS
    let predictor x = sprintf "Outcome %i" x

    let rec display (tree:Tree) (actives:int Set) (depth:int) =
        seq {
            match tree with
            | Leaf(v)   -> yield sprintf "%s -> %s" (pad actives depth) (predictor v)
            | Branch(t) ->
                match t with
                | Cat(branch,next) ->
                    let last = (Array.length next) - 1 // index of last branch
                    for i in 0 .. last do
                        let actives' = 
                            if (i = last) 
                            then Set.remove depth actives 
                            else actives
                        let pipe = 
                            if (i = last) 
                            then "└" else "├"
                        match next.[i] with
                        | Leaf(z) -> 
                            yield sprintf "%s%s %s = %s → %s" (pad actives depth) pipe "FEAT" "VALUE" (predictor z)
                        | Branch(_) -> 
                            yield sprintf "%s%s %s = %s" (pad actives' depth) pipe "FEAT" "VALUE"
                            yield! display (next.[i]) (Set.add (depth + 1) actives') (depth + 1)
                | Num(branch,next) ->
                    let last = (Array.length next) - 1 // index of last branch
                    for i in 0 .. last do
                        let actives' = 
                            if (i = last) 
                            then Set.remove depth actives 
                            else actives
                        let pipe = 
                            if (i = last) 
                            then "└" else "├"
                        match next.[i] with
                        | Leaf(z) -> 
                            yield sprintf "%s%s %s = %s → %s" (pad actives depth) pipe "FEAT" "VALUE" (predictor z)
                        | Branch(_) -> 
                            yield sprintf "%s%s %s = %s" (pad actives' depth) pipe "FEAT" "VALUE"
                            yield! display (next.[i]) (Set.add (depth + 1) actives') (depth + 1)
        }

    let pretty (tree:Tree) =
        display tree ([0] |> Set.ofList) 0
        |> Seq.iter (printfn "%s")
//    // renders a decision tree
//    let rec private plot (tree: Tree) 
//                         (actives: int Set) 
//                         (depth: int) 
//                         (predictor: int -> string)
//                         (reverseFeatures: (string * Map<int,string>)[]) =
//        seq {
//            match tree with
//            | Leaf(x) -> yield sprintf "%s -> %s" (pad actives depth) (predictor x)
//            | CatBranch(f,d,next) ->        
//                let last = next |> Map.toArray |> Array.length
//                let (fName, fMap) = reverseFeatures.[f]
//                let next' =
//                    next 
//                    |> Map.toArray
//                    |> Array.mapi (fun i (x, n) -> (i, x, n))
//                for (i, x, n) in next' do
//                    let actives' = 
//                        if (i = (last - 1)) 
//                        then Set.remove depth actives 
//                        else actives
//                    let pipe = 
//                        if (i = (last - 1)) 
//                        then "└" else "├"
//                    match n with
//                    | Leaf(z) -> 
//                        yield sprintf "%s%s %s = %s → %s" (pad actives depth) pipe fName (fMap.[x]) (predictor z)
//                    | CatBranch(_) -> 
//                        yield sprintf "%s%s %s = %s" (pad actives' depth) pipe fName (fMap.[x]) 
//                        yield! plot n (Set.add (depth + 1) actives') (depth + 1) predictor reverseFeatures
//        }