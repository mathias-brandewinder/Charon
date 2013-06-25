namespace Charon

module DecisionTree =

    open Charon
    open System.Collections.Generic

    // A feature maps the outcomes, encoded as integers,
    // to sorted observation indexes in the dataset.
    type Feature = Map<int, index>
    // Training Set = Labels and Features
    type TrainingSet = Feature * Feature []

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
    let selectFeature (dataset: TrainingSet) // full dataset
                      (filter: index) // indexes of observations in use
                      (remaining: int Set) = // indexes of features usable 
        let labels = fst dataset |> filterBy filter
        let initialEntropy = entropy labels
        
        let best =            
            remaining
            |> Seq.map (fun f -> f, (snd dataset).[f] |> filterBy filter)
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
    let rec build (dataset: TrainingSet) // full dataset
                  (filter: index) // indexes of observations in use
                  (remaining: int Set) // indexes of features usable
                  (featureSelector: int Set -> int Set)
                  (minLeaf: int) = // min elements in a leaf   
                   
        if (remaining = Set.empty) then 
            Leaf(fst dataset |> filterBy filter |> mostLikely)
        elif (Index.length filter < minLeaf) then
            Leaf(fst dataset |> filterBy filter |> mostLikely)
        else
            let candidates = featureSelector remaining
            let best = selectFeature dataset filter candidates

            match best with
            | None -> Leaf(fst dataset |> filterBy filter |> mostLikely)
            | Some(best) -> 
                let (index, feature) = best
                let remaining = remaining |> Set.remove index
                let splits = filterBy filter feature
                let likely = splits |> mostLikely // what to pick when missing value?
                Branch(index, likely,
                    splits
                    |> Map.map (fun v indices ->
                           let tree = build dataset indices remaining featureSelector minLeaf
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
    
    let inline any x = id x
    
    let ID3Classifier (dataset: TrainingSet) // full dataset
                      (filter: index)
                      (minLeaf: int) = // min elements in a leaf  
        let fs = snd dataset |> Array.length
        let remaining = Set.ofList [ 0 .. (fs - 1) ]
        let tree = build dataset filter remaining any minLeaf
        decide tree

    // prepare an array into a Feature.
    let prepare (obs: int seq) =
        let dict = Dictionary<int, index>()
        obs
        |> Seq.fold (fun i value ->
                if dict.ContainsKey(value)
                then dict.[value] <- i::dict.[value]
                else dict.Add(value, [i])
                (i + 1)) 0
        |> ignore
        dict |> Seq.map (fun kv -> kv.Key, List.rev kv.Value) |> Map.ofSeq

    let extract (feature: 'a -> string) (data: 'a seq) =
        let map =
            data 
            |> Seq.map (fun item -> feature item) 
            |> Seq.distinct
            |> Seq.mapi (fun i value -> value, i + 1)
            |> Map.ofSeq
        let extractor x = 
            let value = feature x
            if map.ContainsKey(value) then map.[value] else 0
        map, extractor

    let trainingConverter (labelizer: 'a -> int) (featurizers: ('b -> int) []) =
        let features = Array.length featurizers
        let extractExample (l, ex) =
            labelizer l,
            featurizers |> Array.map (fun f -> f ex)
        features, extractExample

    let private append (dict: Dictionary<int, int list>) (value:int) (index:int) =
        if dict.ContainsKey(value)
        then dict.[value] <- index::dict.[value]
        else dict.Add(value, [index])

    let prepareTraining (obs: 'a seq) (converters: int * ('a -> int * int[])) =
        let fs, converters = converters
        let labels = Dictionary<int, index>()
        let features = [| for i in 1 .. fs -> (Dictionary<int, index>()) |]
        obs
        |> Seq.map (fun x -> converters x)
        |> Seq.iteri (fun i (label, feats) ->
            append labels label i
            features |> Array.iteri (fun j f -> append f feats.[j] i))

        labels |> Seq.map (fun kv -> kv.Key, List.rev kv.Value) |> Map.ofSeq,
        [| for feat in features -> feat |> Seq.map (fun kv -> kv.Key, List.rev kv.Value) |> Map.ofSeq |]
        
    // work in progress: Random Forest
    
    // incorrect but good enough for now
    let pickN n (from: int Set) =
        let array = Set.toArray from 
        let rng = System.Random()
        seq { for i in 1 .. n -> array.[rng.Next(0, Array.length array)] } |> Set.ofSeq
    
    // also incorrect but good enough
    let bag (p: float) (from: index) =
        let rng = System.Random()
        [ for x in from do if rng.NextDouble() <= p then yield x ]

    // grow a tree, picking a random subset of the features at each node
    let randomTree (dataset: TrainingSet) // full dataset
                   (filter: index) // indexes of observations in use
                   (remaining: int Set) // indexes of features usable
                   (minLeaf: int) = // min elements in a leaf    
        let n = sqrt (Set.count remaining |> (float)) |> ceil |> (int)
        build dataset filter remaining (pickN n) minLeaf

    // grow a forest of random trees
    let forest (dataset: TrainingSet) // full dataset
               (filter: index) // indexes of observations in use
               (minLeaf: int) // min elements in a leaf
               (bagging: float)
               (iters: int) =    
        let fs = snd dataset |> Array.length
        let remaining = Set.ofList [ 0 .. (fs - 1) ]
        let n = sqrt (Set.count remaining |> (float)) |> (int)
        let picker = pickN n
        let bagger = bag bagging
        [ for i in 1 .. iters -> build dataset (filter |> bagger) remaining picker minLeaf ]

    // decide based on forest majority decision
    let private forestDecision (trees: Tree list) (obs: int []) =
        trees 
        |> List.map (fun t -> decide t obs)
        |> Seq.countBy id
        |> Seq.maxBy snd
        |> fst

    let forestDecide (trees: Tree list) (obs: 'a) (f: 'a -> int []) (l: int -> string) =
        forestDecision trees (f obs) |> l