type DiscreteFeature = {
    Cases: byte
    Observations: byte[]
    }

type DiscreteLabel = {
    Labels: byte
    Observations: byte[]
    }

open System
let rng = Random()

let size = 1000000

let labels = 
    let labelCases = 10
    let labels = Array.init size (fun _ -> rng.Next(labelCases) |> byte)
    {   Labels = labelCases |> byte
        Observations = labels }

let feature1 =
    let featureCases = 10
    let feature = Array.init size (fun _ -> rng.Next(featureCases) |> byte)
    {   Cases = featureCases |> byte
        Observations = feature }

let feature2 =
    let featureCases = 5
    let feature = Array.init size (fun _ -> rng.Next(featureCases) |> byte)
    {   Cases = featureCases |> byte
        Observations = feature }

let filter = Array.init size (fun _ -> 1uy)//rng.Next(2) |> byte) // could be bool

#time "on"

// this is really a Contingency Table
let contingencyTable 
    (feature: DiscreteFeature) 
        (labels: DiscreteLabel)
            (filter: byte[]) =

    let featureCases = feature.Cases |> int
    let labelCases = labels.Labels |> int
    let feature = feature.Observations
    let labels = labels.Observations

    let counters = 
        Array.init 
            featureCases 
            (fun _ -> Array.zeroCreate<int> labelCases)
    feature
    |> Array.iteri (fun i outcome ->
        if filter.[i] > 0uy
        then
            let outcome = int outcome
            let label = int labels.[i]
            let count = counters.[outcome].[label]
            counters.[outcome].[label] <- count + 1)
    counters

// data is the count of instances for each possible outcome
let h (data:int[]) =
    let total = data |> Seq.sum |> float
    data 
    |> Seq.sumBy (fun x -> 
        let p = float x / total
        if p > 0. then - p * log p else 0.)

let entropyGain
    (feature: DiscreteFeature) 
        (labels: DiscreteLabel)
            (filter: byte[]) =

    // compute totals
    let counts = contingencyTable feature labels filter

    let original = Array.zeroCreate<int> (labels.Labels |> int)

    counts 
    |> Array.iter (fun row -> 
        row 
        |> Array.iteri (fun i x -> original.[i] <- original.[i] + x))
    let H = h original
    let instances = original |> Seq.sum |> float
    let conditional =
        counts 
        |> Seq.sumBy (fun row -> 
            let p = row |> Seq.sum |> fun x -> float x / instances
            p * h row)
    H - conditional

// could inject other metrics, eg Gini
// TODO handle N/A

type Tree =
    | Leaf of byte // label
    | Branch of int * Tree[] // feature index + rest

let createLeaf (labels: DiscreteLabel) (filter: byte[]) =
    let counters = Array.zeroCreate<int> (int labels.Labels)
    filter
    |> Array.iteri (fun i f -> 
        if f > 0uy 
        then 
            let label = int (labels.Observations.[i])
            counters.[label] <- counters.[label] + int f)
    let total = counters |> Array.sum |> float
    counters |> Array.map (fun x -> float x / total) 

let rec buildTree 
    (features: DiscreteFeature[]) 
        (labels: DiscreteLabel) 
            (filter: byte[]) 
                (candidates: int Set) =
    

    candidates
    |> Seq.maxBy (fun featureIndex ->
        entropyGain features.[featureIndex] labels filter)


buildTree [| feature1; feature2; feature1; feature2; feature1; feature2;feature1; feature2;feature1; feature2 |] labels filter (set [0..9])