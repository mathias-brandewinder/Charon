namespace Charon

module Tree = 

    open Charon.Entropy
    open Charon.MDL
    open Charon.Continuous
    
    type Dataset = int * int [] * (float option * int) [][] // classes, outcomes, features: only continuous for now

    let selectFeature (dataset: Dataset) // full dataset
                      (filter: Filter) // indexes of observations in use
                      (remaining: int Set) = // indexes of features usable 
        
        let classes, outcomes, features = dataset
        
        let candidates = 
            seq { for f in remaining -> f, splitValue classes (features.[f]) filter }
            |> Seq.filter (fun (i,f) -> Option.isSome f)
            |> Seq.map (fun (i,f) -> i, Option.get f)
        if (Seq.isEmpty candidates)
        then None
        else 
            candidates 
            |> Seq.maxBy (fun (i, (splits,value)) -> value) 
            |> Some

    let mostLikely (outcomes: int []) = 
        outcomes 
        |> Seq.countBy id 
        |> Seq.maxBy snd 
        |> fst

    type BranchType =
//        | Cat of int * int * Tree [] // feature, default, rest
        | Num of int * int * float list * Tree [] // feature, default, cuts, rest
    and Tree =
        | Leaf of int
        | Branch of BranchType

    let rec growTree (dataset: Dataset) // full dataset
                     (filter: Filter) // indexes of observations in use
                     (remaining: int Set) // indexes of features usable
                     (featureSelector: int Set -> int Set)
                     (minLeaf: int) = // min elements in a leaf   
        
        let classes, outcomes, features = dataset
                   
        if (remaining = Set.empty) then              
            Leaf(filter |> filterBy outcomes |> mostLikely)
        elif (Array.length filter < minLeaf) then
            Leaf(filter |> filterBy outcomes |> mostLikely)
        else
            let candidates = featureSelector remaining
            let best = selectFeature dataset filter candidates

            match best with
            | None -> Leaf(filter |> filterBy outcomes |> mostLikely)
            | Some(index, (splits,_)) -> 
                let remaining = remaining |> Set.remove index
                let feature = features.[index]
                let indexes = subindex feature filter splits
                let likely = 0 // FIX THIS : what to pick when missing value?
                Branch(Num(index, likely, splits, [| for i in indexes -> growTree dataset (i.Value) remaining featureSelector minLeaf |]))