namespace Charon

[<AutoOpen>]
module Tree =

    open Charon.Featurization

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

    let feat (translator:(string*FeatureMap)*((string*FeatureMap)[])) (branch:BranchType) (i:int) =
        let _, fs = translator
        match branch with
        | Cat(branch,next) ->
            let f = branch.FeatIndex
            let (n, m) = fs.[f]
            n, m.InsideOut.[i]
        | Num(branch,next) ->
            let f = branch.FeatIndex
            let splits = branch.Splits |> List.toArray
            if (Array.length splits > 0)
            then
                if (i=Array.length splits)
                then
                    (fst fs.[f]), sprintf ">  %.3f" (splits.[max 0 i-1])
                else
                    (fst fs.[f]), sprintf "<= %.3f" (splits.[i])
            else
                (fst fs.[f]), "INVARIABLE"

    let rec display (tree:Tree) (actives:int Set) (depth:int) (translator:(string*FeatureMap)*((string*FeatureMap)[]))=
        let ls,fs = translator
        seq {
            match tree with
            | Leaf(v)   -> yield sprintf "%s -> %s" (pad actives depth) (fst ls + " " + (snd ls).InsideOut.[v])
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
                        let fname,fvalue = feat translator t i
                        match next.[i] with
                        | Leaf(z) -> 
                            yield sprintf "%s%s %s = %s → %s" (pad actives depth) pipe fname fvalue (fst ls + " " + (snd ls).InsideOut.[z])
                        | Branch(_) -> 
                            yield sprintf "%s%s %s = %s" (pad actives' depth) pipe fname fvalue
                            yield! display (next.[i]) (Set.add (depth + 1) actives') (depth + 1) translator
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
                        let fname,fvalue = feat translator t i
                        match next.[i] with
                        | Leaf(z) -> 
                            yield sprintf "%s%s %s = %s → %s" (pad actives depth) pipe fname fvalue (fst ls + " " + (snd ls).InsideOut.[z])
                        | Branch(_) -> 
                            yield sprintf "%s%s %s = %s" (pad actives' depth) pipe fname fvalue
                            yield! display (next.[i]) (Set.add (depth + 1) actives') (depth + 1) translator
        }

    let pretty (tree:Tree) (translator:(string*FeatureMap)*((string*FeatureMap)[])) =
        display tree ([0] |> Set.ofList) 0 translator
        |> Seq.map (sprintf "%s")
        |> Seq.toArray
        |> fun x -> System.String.Join("\n",x)