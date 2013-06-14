namespace Charon

module DecisionTree =

    // A feature maps the outcomes, encoded as integers,
    // to observation indexes in the dataset.
    type Feature = Map<int, int Set>

    // A tree is either 
    // a Leaf (a final conclusion has been reached), or
    // a Branch (depending on the outcome on a feature,
    // more investigation is needed).
    type Tree = 
    | Leaf of int // decision
    | Branch of int * Map<int, Tree> // feature & sub-trees by outcome

    let private h (category: int) (total: int) = 
        if total = 0 then 0. // is this right? failwith "At least one observation is needed."
        elif category = 0 then 0.
        else
            let p = (float)category / (float)total
            - p * log p

    // Total elements in a feature.
    let private total (f: Feature) = 
        f 
        |> Map.toSeq 
        |> Seq.sumBy (fun (x,y) -> Set.count y)
    
    // Entropy of a feature.
    let entropy (f: Feature) =
        let size = total f
        f 
        |> Map.toSeq 
        |> Seq.sumBy (fun (x,y) -> h (Set.count y) size)

    // Apply a filter to a Feature, retaining
    // only the elements whose index are in the filter set.
    let filterBy (filter: int Set) (feature: Feature) =
        feature
        |> Map.map (fun value indexes -> Set.intersect indexes filter)

    // Retrieve all indexes covered by feature
    let indexesOf (feature: Feature) =
        feature 
        |> Map.fold (fun indexes k kIndexes -> Set.union indexes kIndexes) Set.empty

    // Split labels based on the values of a feature
    let split (feature: Feature) (labels: Feature) =
        feature 
        |> Map.map (fun v indexes ->
               labels 
               |> Map.map (fun l labelIndexes -> 
                      Set.intersect labelIndexes indexes))

    // Entropy gain of splitting labels by feature
    let gain (feature: Feature) (labels: Feature) =
        let initialEntropy = entropy labels
        let size = total feature
        split feature labels
        |> Map.map (fun v feature -> 
               (float)(total feature) * entropy (labels |> filterBy (indexesOf feature)) / (float)size)
        |> Map.toSeq
        |> Seq.sumBy snd
        |> (-) initialEntropy
            
    let selectFeature (dataset: Map<int,Feature>) // full dataset
                      (filter: int Set) // indexes of observations in use
                      (remaining: int Set) // indexes of features usable
                      (lbls: int) =
        let labels = dataset.[lbls] |> filterBy filter
        let best =
            remaining
            |> Seq.map (fun f -> f, dataset.[f] |> filterBy filter)
            |> Seq.map (fun (index, feat) -> gain feat labels, (index, feat))
            |> Seq.maxBy fst
        if (fst best > 0.) then Some(snd best) else None

    // Most likely outcome of a feature
    let private mostLikely (f: Feature) =
        f 
        |> Map.toSeq 
        |> Seq.maxBy (fun (i, s) -> Set.count s) 
        |> fst

    // Recursively build a decision tree,
    // selecting out of the remaining features
    // which ones yields the largest entropy gain
    let rec build (dataset: Map<int,Feature>) // full dataset
                  (filter: int Set) // indexes of observations in use
                  (remaining: int Set) // indexes of features usable
                  (lbls: int) =    
        if (remaining = Set.empty || Set.count filter < 10) then // replace by function
            Leaf(dataset.[lbls] |> filterBy filter |> mostLikely)
        else
            let best = selectFeature dataset filter remaining lbls

            match best with
            | None -> Leaf(dataset.[lbls] |> filterBy filter |> mostLikely)
            | Some(best) -> 
                let (index, feature) = best
                let remaining = remaining |> Set.remove index
                let splits = filterBy filter feature

                Branch(index,
                    splits
                    |> Map.map (fun v indices ->
                           let tree = build dataset indices remaining lbls
                           tree))

    // Recursively walk down the tree,
    // to figure out how an observation
    // should be classified         
    let rec decide (tree: Tree) (obs: int []) =
        match tree with
        | Leaf(outcome) -> outcome
        | Branch(feature, next) ->
              let value = obs.[feature]
              decide next.[value] obs

    // old version; prepare performs better.
    let private oldPrepare (obs: int seq) =
        obs 
        |> Seq.mapi (fun i x -> x, i) 
        |> Seq.groupBy fst 
        |> Seq.map (fun (x, indexes) -> 
               x, indexes |> Seq.map snd |> Set.ofSeq) 
        |> Map.ofSeq

    // Break a vector of observations
    // into a Map, with each possible value
    // mapping to a set of observation indexes
    let prepare (obs: int seq) =
        obs
        |> Seq.fold (fun (i, counts) value ->
               let updated =
                   if Map.containsKey value counts
                   then Map.add value (counts.[value] |> Set.add i) counts
                   else Map.add value ([i] |> Set.ofList) counts
               (i + 1), updated) (0, Map.empty)
        |> snd