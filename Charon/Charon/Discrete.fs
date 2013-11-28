namespace Charon

module Discrete =

    open System.Collections.Generic
    open Charon

    // A feature maps the outcomes, encoded as integers,
    // to sorted observation indexes in the dataset.
    type Feature = Map<int, index>

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
    let filterBy (filter: int []) (feature: Feature) =
        feature
        |> Map.map (fun value indexes -> Index.intersect indexes filter)

    // Retrieve all indexes covered by feature
    let indexesOf (feature: Feature) =
        feature 
        |> Map.fold (fun indexes k kIndexes -> Index.merge indexes kIndexes) [||]
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

    let countFrom (data:int[]) =
        data |> Seq.countBy id |> Seq.map snd |> Seq.toArray

    // Conditional Entropy when splitting labels by feature
    let conditionalEntropy2 (feature: Feature) (filter: Filter) (outcomes: int[]) =
        let size = total feature
        feature 
        |> Map.map (fun k v -> Index.intersect filter v)
        |> Map.map (fun k v -> v |> Array.map (fun i -> outcomes.[i]))
        |> Map.map (fun k v -> countFrom v)
        |> Map.map (fun k v -> Entropy.h v)
        |> Map.map (fun k v -> 
               v * (float)(total feature) / (float)size)
        |> Map.toSeq
        |> Seq.sumBy snd

    // Most likely outcome of a feature
    let mostLikely (f: Feature) =
        f 
        |> Map.toSeq 
        |> Seq.maxBy (fun (i, s) -> Index.length s) 
        |> fst

    // prepare an array of observed values into a Feature.
    let prepare (obs: int seq) =
        let dict = Dictionary<int, index>()
        obs
        |> Seq.fold (fun i value ->
                if dict.ContainsKey(value)
                then dict.[value] <- Array.append dict.[value] [|i|]
                else dict.Add(value, [|i|])
                (i + 1)) 0
        |> ignore
        dict |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq