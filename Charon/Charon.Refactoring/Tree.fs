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