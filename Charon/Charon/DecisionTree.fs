namespace Charon

module DecisionTree =

    open Charon
    open System
    open System.Collections.Generic

    // A feature maps the outcomes, encoded as integers,
    // to sorted observation indexes in the dataset.
    type Feature = Map<int, index>
    // Training Set = Labels and Features
    type TrainingSet = Feature * Feature []
    // Output detail level
    type DetailLevel = Minimal | Verbose

    let Categorical<'a> (value: Option<'a>) =
        value |> Option.map (fun x -> x.ToString())

    let StringCategory text =
        if String.IsNullOrWhiteSpace(text)
        then None
        else Some(text)

    // A tree is either 
    // a Leaf (a final conclusion has been reached), or
    // a Branch (depending on the outcome on a feature,
    // more investigation is needed).
    type Tree = 
    | Leaf of int // decision
    | CatBranch of int * int * Map<int, Tree> // feature index, default choice, & sub-trees by outcome

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
    let rec growTree (dataset: TrainingSet) // full dataset
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
                CatBranch(index, likely,
                    splits
                    |> Map.map (fun v indices ->
                           let tree = growTree dataset indices remaining featureSelector minLeaf
                           tree))

    // Recursively walk down the tree,
    // to figure out how an observation
    // should be classified         
    let rec decide (tree: Tree) (obs: int []) =
        match tree with
        | Leaf(outcome) -> outcome
        | CatBranch(feature, mostLikely, next) ->
              let value = obs.[feature]
              if Map.containsKey value next
              then decide next.[value] obs
              else decide next.[mostLikely] obs
    
    let inline any x = id x
    
    let ID3Classifier (dataset: TrainingSet) // full dataset
                      (filter: index) // indexes of sample examples
                      (minLeaf: int) = // min elements in a leaf  
        let fs = snd dataset |> Array.length
        let remaining = Set.ofList [ 0 .. (fs - 1) ]
        let tree = growTree dataset filter remaining any minLeaf
        decide tree, tree

    // Data preparation functions ******************************

    // prepare an array of observed values into a Feature.
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

    // From a sequence of observations, for a feature,
    // construct a Map of existing values to integers (indexes),
    // an extractor function that converts an observation
    // to its mapped integer index.
    // Missing values (None) are implicitly mapped to 0
    let extract (feature: 'a -> string option) (data: 'a seq) =
        // map each existing value to 1, 2, ..
        let map =
            data 
            |> Seq.choose (fun item -> feature item)
            |> Seq.distinct
            // indexes start at i + 1, 0 is used for missing values
            |> Seq.mapi (fun i value -> value, i + 1)
            |> Map.ofSeq
        let extractor x = 
            let value = feature x
            match value with
            | None -> 0
            | Some(value) -> 
                if map.ContainsKey(value) 
                then map.[value] 
                else 0
        map, extractor

    let private append (dict: Dictionary<int, int list>) (value:int) (index:int) =
        if dict.ContainsKey(value)
        then dict.[value] <- index::dict.[value]
        else dict.Add(value, [index])

    let private prepareLabels (labels: string option []) =
        let labelsMap, labelizer = labels |> extract id
        let reverseLabels = 
            labelsMap 
            |> Map.toSeq 
            |> Seq.map (fun (k, v) -> (v, k)) 
            |> Map.ofSeq
        let predictor x = reverseLabels.[x]
        // currently not returning the map,
        // but might do so later for diagnosis
        labelizer, predictor

    let private prepareFeaturizer (observations: 'a seq) (fs: (string * ('a -> string option)) [])=
        let featuresMap, featurizers =
            fs 
            |> Array.map snd
            |> Array.map (fun f -> observations |> extract f)
            |> Array.unzip
        let names = fs |> Array.map fst
        let reverseFeatures =
            Array.zip names featuresMap
            |> Array.map (fun (name, map) ->
                name,
                map
                |> Map.toSeq
                |> Seq.map (fun (k, v) -> (v, k))
                |> Map.ofSeq)
        // currently not returning the map,
        // but might do so later for diagnosis
        let featurizer obs = featurizers |> Array.map (fun f -> f obs)
        (featurizer, reverseFeatures)

    // Create a function to extract labels and features
    // from a sequence of training examples (with a label and features)
    let private prepareTraining (obs: ('a * 'b) seq) 
                                (fs: int) // number of features
                                (labelizer: 'a -> int) 
                                (featurizers: ('b -> int [])) =
        let converters (l, ex) =
            labelizer l,
            featurizers ex
        
        let labels = Dictionary<int, index>()
        let features = [| for i in 1 .. fs -> (Dictionary<int, index>()) |]
        obs
        |> Seq.map (fun x -> converters x)
        |> Seq.iteri (fun i (label, feats) ->
            append labels label i
            features |> Array.iteri (fun j f -> append f feats.[j] i))

        labels |> Seq.map (fun kv -> kv.Key, List.rev kv.Value) |> Map.ofSeq,
        [| for feat in features -> feat |> Seq.map (fun kv -> kv.Key, List.rev kv.Value) |> Map.ofSeq |]

    // Tree visualization functions ******************************

    // Tree rendering utility: renders "pipes"
    // for active branches
    let private pad (actives: int Set) (depth: int) =
        String.Join("", 
            [|  for i in 0 .. (depth - 1) do 
                    if (actives.Contains i) 
                    then yield "│   " 
                    else yield "   " |])

    // renders a decision tree
    let rec private plot (tree: Tree) 
                         (actives: int Set) 
                         (depth: int) 
                         (predictor: int -> string)
                         (reverseFeatures: (string * Map<int,string>)[]) =
        seq {
            match tree with
            | Leaf(x) -> yield sprintf "%s -> %s" (pad actives depth) (predictor x)
            | CatBranch(f,d,next) ->        
                let last = next |> Map.toArray |> Array.length
                let (fName, fMap) = reverseFeatures.[f]
                let next' =
                    next 
                    |> Map.toArray
                    |> Array.mapi (fun i (x, n) -> (i, x, n))
                for (i, x, n) in next' do
                    let actives' = 
                        if (i = (last - 1)) 
                        then Set.remove depth actives 
                        else actives
                    let pipe = 
                        if (i = (last - 1)) 
                        then "└" else "├"
                    match n with
                    | Leaf(z) -> 
                        yield sprintf "%s%s %s = %s → %s" (pad actives depth) pipe fName (fMap.[x]) (predictor z)
                    | CatBranch(_) -> 
                        yield sprintf "%s%s %s = %s" (pad actives' depth) pipe fName (fMap.[x]) 
                        yield! plot n (Set.add (depth + 1) actives') (depth + 1) predictor reverseFeatures
        }
     
    let pretty tree predictor reverseFeatures =
        fun () ->
            plot tree (Set.ofList [0]) 0 predictor reverseFeatures
            |> Seq.iter (printfn "%s")

    // Draw the entire tree
    let render tree predictor reverseFeatures = 
        plot tree (Set.ofList [0]) 0 predictor reverseFeatures


    type ID3Config = {
        MinimumLeafSize: int;
        DetailLevel: DetailLevel }

    let DefaultID3Config = { 
        MinimumLeafSize = 5; 
        DetailLevel = Minimal }

    type ID3Report = {
        Tree: Tree;
        Pretty: unit -> unit }

    // Create a full ID3 Classification Tree
    let createID3Classifier (examples: (string option * 'a) []) 
                            (fs: (string * ('a -> string option))[]) 
                            (config: ID3Config) =
        // Unwrap labels and observations
        let labels, observations = Array.unzip examples
        // Convert label to integer, and integer to label
        let labelizer, predicted = prepareLabels labels
        // Convert observation to integer array
        let featurizer, reverseMap = prepareFeaturizer observations fs
        
        let trainingSet = prepareTraining examples (Array.length fs) labelizer featurizer

        let classifier, tree = ID3Classifier trainingSet [ 0.. (examples |> Array.length) - 1 ] (config.MinimumLeafSize)
        
        featurizer >> classifier >> predicted,
        match (config.DetailLevel) with
        | Minimal -> None
        | Verbose -> Some({ Tree = tree; Pretty = pretty tree predicted reverseMap })

    // work in progress: Random Forest
    
    // Pick n distinct random indexes at most from a set;
    // incorrect but good enough for now.
    let pickN n (rng: Random) (from: int Set) =
        let array = Set.toArray from 
        seq { for i in 1 .. n -> array.[rng.Next(0, Array.length array)] } |> Set.ofSeq
    
    // pick a proportion p from original sample, with replacement
    let bag (p: float) (rng: Random) (from: index) =
        let size = Index.length from
        let bagSize = ((float)size * p) |> (int)
        [ for i in 1 .. bagSize -> rng.Next(0, size) ] |> List.sort

    // grow a tree, picking a random subset of the features at each node
    let private randomTree (dataset: TrainingSet) // full dataset
                           (filter: index) // indexes of observations in use
                           (remaining: int Set) // indexes of features usable
                           (minLeaf: int) // min elements in a leaf
                           (rng: Random) = // random number generator to use    
        let n = Set.count remaining |> (float) |> sqrt |> ceil |> (int)
        growTree dataset filter remaining (pickN n rng) minLeaf

    // grow a forest of random trees
    let private growForest (dataset: TrainingSet) // full dataset
                           (filter: index) // indexes of observations in use
                           (minLeaf: int) // min elements in a leaf
                           (bagging: float)
                           (iters: int) 
                           (rng: Random) =    
        let fs = snd dataset |> Array.length
        let remaining = Set.ofList [ 0 .. (fs - 1) ]
        let n = sqrt (Set.count remaining |> (float)) |> (int)

        [| for i in 1 .. iters -> 
            let rng = Random(rng.Next())
            let picker = pickN n rng
            let bagger = bag bagging rng
            (picker, bagger) |] 
        |> Array.Parallel.map (fun (picker, bagger) -> growTree dataset (filter |> bagger) remaining picker minLeaf)

    // decide based on forest majority decision
    let private forestDecision (trees: Tree []) (obs: int []) =
        trees 
        |> Array.map (fun t -> decide t obs)
        |> Seq.countBy id
        |> Seq.maxBy snd
        |> fst

    let private forestDecide (trees: Tree []) (obs: 'a) (f: 'a -> int []) (l: int -> string) =
        forestDecision trees (f obs) |> l

    type RFConfig = {
        MinimumLeafSize: int;
        BaggingProportion: float;
        Iterations: int;
        RNG: Random option;
        DetailLevel: DetailLevel }

    let DefaultRFConfig = { 
        MinimumLeafSize = 5;
        BaggingProportion = 0.75;
        Iterations = 20;
        RNG = None;
        DetailLevel = Minimal }

    type RFReport = {
        Report: string; }

    // there is obvious duplication here with ID3, need to clean up
    let createForestClassifier (examples: (string option * 'a) []) 
                               (fs: (string * ('a -> string option))[])
                               (config: RFConfig) =
        // Unwrap labels and observations
        let labels, observations = Array.unzip examples
        // Convert label to integer, and integer to label
        let labelizer, predicted = prepareLabels labels
        // Convert observation to integer array
        let featurizer, reverseMap = prepareFeaturizer observations fs
        
        let trainingSet = prepareTraining examples (Array.length fs) labelizer featurizer

        let rng = 
            match config.RNG with
            | None -> Random()
            | Some(x) -> x

        let forest = growForest trainingSet [ 0.. (examples |> Array.length) - 1 ] config.MinimumLeafSize config.BaggingProportion config.Iterations rng
        
        let classifier obs = (forestDecide forest obs featurizer predicted)
        classifier,
        match (config.DetailLevel) with
        | Minimal -> None
        | Verbose -> Some({ Report = "Report" })