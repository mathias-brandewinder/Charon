type DiscreteFeature = {
    // how many distinct cases exist,
    // not including N/A.
    Cases: byte
    // for the sample, outcome/case for
    // each observation.
    // Convention: 0uy denotes a missing value.
    Observations: byte[]
    }


// Contingency Table: count for both features
// how many instances we have for each combination
// of outcomes.
let contingencyTable 
    (feature: DiscreteFeature) 
        (labels: DiscreteFeature)
            (filter: byte[]) =

    let featureCases = feature.Cases |> int
    let labelCases = labels.Cases |> int
    let feature = feature.Observations
    let labels = labels.Observations
    
    // For each outcome of the feature, create an array
    // that will count the instances for each label.
    // The extra slot is for N/A values.
    let counters = 
        Array.init 
            (featureCases + 1) 
            (fun _ -> Array.zeroCreate<int> (labelCases + 1))

    feature
    |> Array.iteri (fun i outcome ->
        if filter.[i] > 0uy
        then
            let outcome = int outcome
            let label = int labels.[i]
            let currentCount = counters.[outcome].[label]
            let count = int filter.[i]
            counters.[outcome].[label] <- currentCount + count)

    counters

// data is the count of instances for each possible outcome
let h (data:int[]) =
    let total = data |> Seq.sum |> float
    data 
    |> Seq.sumBy (fun x -> 
        let p = float x / total
        if p > 0. then - p * log p else 0.)

// How much better is my information on labels,
// if I split the dataset according to the feature?
let entropyGain
    (contingencyTable: int[][]) =

    let labelsCount = contingencyTable.[0].Length
    let original = Array.zeroCreate<int> (labelsCount |> int)

    contingencyTable 
    |> Array.iter (fun row -> 
        row 
        |> Array.iteri (fun i x -> original.[i] <- original.[i] + x))
    let H = h original

    let instances = original |> Seq.sum |> float
    let conditional =
        contingencyTable 
        |> Seq.sumBy (fun row -> 
            let p = row |> Seq.sum |> fun x -> float x / instances
            p * h row)
    H - conditional

// https://en.wikipedia.org/wiki/Decision_tree_learning#Metrics
// could inject other metrics, eg Gini
// TODO handle N/A

type Tree =
    | Leaf of byte // label 'index': later on can incorporate more data
    | Branch of int * Tree[] // feature index + rest

let createLeaf (labels: DiscreteFeature) (filter: byte[]) =
    // this block should be re-used
    let counters = Array.zeroCreate<int> (1 + int labels.Cases)
    filter
    |> Array.iteri (fun i f -> 
        if f > 0uy 
        then 
            let label = int (labels.Observations.[i])
            counters.[label] <- counters.[label] + int f)
    let total = counters |> Array.sum |> float
    let probas = counters |> Array.map (fun x -> float x / total)
    let prediction =
        counters
        |> Seq.mapi (fun i count -> i, count)
        |> Seq.maxBy snd
        |> fst
        |> byte 
    Leaf(prediction)

let splitFilters 
    (feature: DiscreteFeature) 
        (filter: byte[]) =
    
    let cases = int feature.Cases
    let size = filter.Length

    // one filter per case existing in feature
    let filters = 
        Array.init 
            (cases + 1) (fun _ ->
                Array.zeroCreate<byte> size)

    feature.Observations
    |> Array.iteri (fun i x ->
        if filter.[i] > 0uy
        then
            let case = int x
            filters.[case].[i] <- filter.[i])

    filters
    
let rec buildTree 
    (features: DiscreteFeature[]) 
        (labels: DiscreteFeature) 
            (filter: byte[]) 
                (candidates: int Set) =
    
    // TODO inject filters to stop recursing
    if candidates.IsEmpty
    then createLeaf labels filter
    else
        let bestIndex, bestTable = 
            candidates
            |> Seq.map (fun i -> i, contingencyTable features.[i] labels filter)
            |> Seq.maxBy (fun (i, table) -> entropyGain table)
        // need to properly handle N/A
        let remaining = candidates |> Set.remove bestIndex
        let bestFeature = features.[bestIndex]
        let filters = splitFilters bestFeature filter
        let branches = 
            filters 
            |> Array.map (fun f -> buildTree features labels f remaining)
        Branch(bestIndex, branches)
             
let tree     
    (features: DiscreteFeature[]) 
        (labels: DiscreteFeature) =

    // TODO: validate preconditions:
    // features > 0, size > 0, ...
    // TODO filter out observations with no label?
    let candidates = set [ 0 .. features.Length - 1 ]
    let size = labels.Observations.Length
    let filter = Array.init size (fun _ -> 1uy)

    buildTree features labels filter candidates

(*
Create a test sample
*)

open System
let rng = Random()

let generateSyntheticFeature (size:int) (cases:byte) =
    let maxCaseIndex = int cases
    let observations = Array.init size (fun _ -> rng.Next(maxCaseIndex + 1) |> byte)
    { Cases = cases; Observations = observations }

let size = 1000000

let labels = generateSyntheticFeature size 2uy

let feature1 = generateSyntheticFeature size 10uy

let feature2 = generateSyntheticFeature size 5uy

// filter denotes what observations are part of the sample.
// it could be bool, but byte allows also sample with repetition.
let filter = Array.init size (fun _ -> rng.Next(2) |> byte) 

#time "on"

tree [| feature1; feature2 |] labels 

// create fs fake features
let syntheticDataset size fs =
    generateSyntheticFeature size 2uy,
    Array.init fs (fun _ -> 
        generateSyntheticFeature size 2uy)

let ys,xs = syntheticDataset 100000 10

let test = tree xs ys |> ignore

// temporary conclusion:
// roughly linear in size
// non-linear in number of features (geometric? depends on outcomes, too)
// looks like a lot of GC, gen0 to gen2, is taking place. Why, where?
// need to profile.