namespace Charon

module Tree = 

    open Charon
    open Charon.Entropy
    open Charon.MDL
    open Charon.Continuous
    open Charon.Discrete

    type Value =
        | Int   of int option
        | Float of float option

    type Feature =
        | Numeric of (float option * int) []
        | Categorical of Map<int, index> // TODO: possibly change representation

    type Dataset = { Classes:int; Outcomes:int []; Features: Feature [] } // classes, outcomes, features: only continuous for now

    type SplitType =
        | NumericSplit of float list * float // feat index, splits, conditional entropy
        | CategorySplit of float // feat index, conditional entropy
        
    let tempSplitValue classes outcomes feature filter =
        match feature with
        | Numeric(x) -> NumericSplit(splitValue classes x filter)
        | Categorical(x) -> CategorySplit(conditionalEntropy2 x filter outcomes) 

    let selectFeature (dataset: Dataset) // full dataset
                      (filter: Filter) // indexes of observations in use
                      (remaining: int Set) = // indexes of usable features 
        
        let classes, outcomes, features = dataset.Classes, dataset.Outcomes, dataset.Features

        let labels = outcomes |> Continuous.filterBy filter
        let initialEntropy =
            labels |> countFrom |> h

        let candidates = 
            seq { for f in remaining -> f, tempSplitValue classes outcomes (features.[f]) filter }
            |> Seq.filter (fun (i,splitType) -> 
                match splitType with
                | NumericSplit(splits, value) -> 
                    match splits with
                    | [] -> false
                    | _  -> initialEntropy - value > 0.
                | CategorySplit(value) -> initialEntropy - value > 0.)

        if (Seq.isEmpty candidates)
        then None
        else 
            candidates 
            |> Seq.maxBy (fun (i, splitType) ->
                match splitType with
                | NumericSplit(splits, value) -> initialEntropy - value > 0.
                | CategorySplit(value) -> initialEntropy - value > 0.)
            |> Some

    let mostLikely (outcomes: int []) = 
        outcomes 
        |> Seq.countBy id 
        |> Seq.maxBy snd 
        |> fst

    type NumBranch = { FeatIndex: int; Default: int; Splits:float list; }
    type CatBranch = { FeatIndex: int; Default: int; }

    type BranchType =
        | Cat of CatBranch * Tree [] // feature, default, rest
        | Num of NumBranch * Tree [] // feature, default, cuts, rest
    and Tree =
        | Leaf of int
        | Branch of BranchType

    let rec growTree (dataset: Dataset) // full dataset
                     (filter: Filter) // indexes of observations in use
                     (remaining: int Set) // indexes of features usable
                     (featureSelector: int Set -> int Set)
                     (minLeaf: int) = // min elements in a leaf   
        
        let classes, outcomes, features = dataset.Classes, dataset.Outcomes, dataset.Features
                   
        if (remaining = Set.empty) then              
            Leaf(filter |> Continuous.filterBy outcomes |> mostLikely)
        elif (Array.length filter < minLeaf) then
            Leaf(filter |> Continuous.filterBy outcomes |> mostLikely)
        else
            let candidates = featureSelector remaining
            let best = selectFeature dataset filter candidates

            match best with
            | None -> Leaf(filter |> Continuous.filterBy outcomes |> mostLikely)
            | Some(index,splitType) ->
                let remaining = remaining |> Set.remove index
                let feature = features.[index]
                match splitType with
                | NumericSplit(splits, value) -> 
                    match feature with
                    | Numeric(feature) ->
                        let indexes = subindex feature filter splits
                        let likely = filter |> Continuous.filterBy outcomes |> mostLikely
                        let branch = { FeatIndex = index; Default = likely; Splits = splits }
                        Branch(Num(branch,  [| 
                                                for i in indexes -> 
                                                    if Array.length (i.Value) < minLeaf then Leaf(likely)
                                                    else growTree dataset (i.Value) remaining featureSelector minLeaf 
                                            |]))
                    | Categorical(x) -> failwith "Feature mismatch"
                | CategorySplit(value) -> 
                    match feature with
                    | Categorical(x) -> 
                        let feature = x
                        let splits = filterBy filter feature
                        let likely = splits |> Discrete.mostLikely
                        let branch = { FeatIndex = index; Default = likely }
                        Branch(Cat(branch, [| 
                                              for i in splits -> 
                                                  if Array.length (i.Value) < minLeaf then Leaf(likely)
                                                  else growTree dataset (i.Value) remaining featureSelector minLeaf 
                                           |]))
                    | Numeric(feature) -> failwith "Feature mismatch"