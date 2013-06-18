namespace Charon

module DecisionTree =

    open Charon

    // A feature maps the outcomes, encoded as integers,
    // to sorted observation indexes in the dataset.
    type Feature = Map<int, index>

    // A tree is either 
    // a Leaf (a final conclusion has been reached), or
    // a Branch (depending on the outcome on a feature,
    // more investigation is needed).
    type Tree = 
    | Leaf of int // decision
    | Branch of int * int * Map<int, Tree> // feature index, default choice, & sub-trees by outcome

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
        |> Seq.sumBy (fun (x, y) -> Index.length y)
    
    // Entropy of a feature.
    let entropy (f: Feature) =
        let size = total f
        f 
        |> Map.toSeq 
        |> Seq.sumBy (fun (x,y) -> h (Index.length y) size)

    // Apply a filter to a Feature, retaining
    // only the elements whose index are in the filter set.
    let filterBy (filter: int list) (feature: Feature) =
        feature
        |> Map.map (fun value indexes -> Index.intersect indexes filter)

    // Retrieve all indexes covered by feature
    let indexesOf (feature: Feature) =
        feature 
        |> Map.fold (fun indexes k kIndexes -> Index.merge indexes kIndexes) []

    // Split labels based on the values of a feature
    let split (feature: Feature) (labels: Feature) =
        feature 
        |> Map.map (fun v indexes ->
               labels 
               |> Map.map (fun l labelIndexes -> 
                      Index.intersect labelIndexes indexes))

    // Conditional Entropy when splitting labels by feature
    let conditionalEntropy (feature: Feature) (labels: Feature) =
        let size = total feature
        split feature labels
        |> Map.map (fun v feature -> 
               (float)(total feature) * entropy (labels |> filterBy (indexesOf feature)) / (float)size)
        |> Map.toSeq
        |> Seq.sumBy snd
    
    // Given a filter on indexes and remaining features,
    // pick the feature that yields highest information gain
    // to split the tree on.             
    let selectFeature (dataset: Map<int,Feature>) // full dataset
                      (filter: index) // indexes of observations in use
                      (remaining: int Set) // indexes of features usable
                      (lbls: int) =
        let labels = dataset.[lbls] |> filterBy filter
        let initialEntropy = entropy labels
        
        let best =            
            remaining
            |> Seq.map (fun f -> f, dataset.[f] |> filterBy filter)
            |> Seq.map (fun (index, feat) -> 
                initialEntropy - conditionalEntropy feat labels, (index, feat))
            |> Seq.maxBy fst
        if (fst best > 0.) then Some(snd best) else None

    // Most likely outcome of a feature
    let private mostLikely (f: Feature) =
        f 
        |> Map.toSeq 
        |> Seq.maxBy (fun (i, s) -> Index.length s) 
        |> fst

    // Recursively build a decision tree,
    // selecting out of the remaining features
    // which ones yields the largest entropy gain
    let rec build (dataset: Map<int,Feature>) // full dataset
                  (filter: index) // indexes of observations in use
                  (remaining: int Set) // indexes of features usable
                  (minLeaf: int) // min elements in a leaf
                  (lbls: int) =    
        if (remaining = Set.empty) then 
            Leaf(dataset.[lbls] |> filterBy filter |> mostLikely)
        elif (Index.length filter < minLeaf) then
            Leaf(dataset.[lbls] |> filterBy filter |> mostLikely)
        else
            let best = selectFeature dataset filter remaining lbls

            match best with
            | None -> Leaf(dataset.[lbls] |> filterBy filter |> mostLikely)
            | Some(best) -> 
                let (index, feature) = best
                let remaining = remaining |> Set.remove index
                let splits = filterBy filter feature
                let likely = splits |> mostLikely // what to pick when missing value?
                Branch(index, likely,
                    splits
                    |> Map.map (fun v indices ->
                           let tree = build dataset indices remaining minLeaf lbls
                           tree))

    // Recursively walk down the tree,
    // to figure out how an observation
    // should be classified         
    let rec decide (tree: Tree) (obs: int []) =
        match tree with
        | Leaf(outcome) -> outcome
        | Branch(feature, mostLikely, next) ->
              let value = obs.[feature]
              if Map.containsKey value next
              then decide next.[value] obs
              else decide next.[mostLikely] obs

    // prepare an array into a Feature.
    let prepare (obs: int seq) =
        let dict = System.Collections.Generic.Dictionary<int, index>()
        obs
        |> Seq.fold (fun i value ->
                if dict.ContainsKey(value)
                then dict.[value] <- i::dict.[value]
                else dict.Add(value, [i])
                (i + 1)) 0
        |> ignore
        dict |> Seq.map (fun kv -> kv.Key, List.rev kv.Value) |> Map.ofSeq