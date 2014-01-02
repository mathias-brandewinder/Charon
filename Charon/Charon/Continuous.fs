namespace Charon

[<RequireQualifiedAccess>]
module Continuous =

    open System
    open System.Collections.Generic
    open Charon
    open Charon.Entropy
    open Charon.MDL              

    let folder (data:(_*Count)[]) (lft,rgt,i) = 
        Some((lft,rgt,i), (add lft (snd data.[i]), sub rgt (snd data.[i]), i+1))

    // This function assumes data is sorted by first key
    let bestSplit c data =

        let empty = [| for i in 1 .. c -> 0 |]
        let total = Seq.fold (fun x item -> add x item) empty (data |> Seq.map snd)
        let initial = (empty, total, 0)
        let fold = folder data

        Seq.unfold fold initial
        |> Seq.map (fun (lft,rgt,i) -> splitValue lft rgt, i) 
        |> Seq.take (data.Length)
        |> Seq.filter (fun (x,y) -> Option.isSome x)
        |> Seq.map (fun (x,y) -> x.Value, y)
        |> fun x -> if Seq.isEmpty x then None else x |> Seq.maxBy fst |> Some

    let rec splitter c data (acc:float list) =
        match bestSplit c data with
        | None -> acc
        | Some(v,i) -> 
            let split = fst data.[i]
            let acc' = split :: splitter c data.[..i-1] acc
            splitter c data.[i..] acc'

    let split c data =
        splitter c data [] |> List.sort

    let prepare keys (data:(float * int)seq) =
        let dict = Dictionary<float,Count>()
        let empty () = [| for i in 1 .. keys -> 0 |]
        let update (count:Count) value = 
            count.[value] <- count.[value] + 1
            count
        for (x,y) in data do
                if dict.ContainsKey(x)
                then dict.[x] <- update dict.[x] y
                else dict.Add(x, update (empty ()) y)
        dict |> Seq.map (fun kv -> kv.Key,kv.Value) |> Seq.sortBy fst |> Seq.toArray

    let indexOf splits value =
        let rec walk splits index =
            match splits with
            | [] -> index
            | hd::tl ->
                if value < hd
                then index
                else walk tl (index+1)
        walk (splits |> List.sort) 0

    let subindex (data: (float option * _)[]) (filter:filter) splits =
        let keys = List.length splits
        let map = seq { for k in 0 .. keys -> k, [||] } |> Map.ofSeq // can probably make array
        seq { for i in filter do 
                  match (data.[i] |> fst) with
                  | None -> ignore ()
                  | Some(v) -> yield i, indexOf splits v }
        |> Seq.groupBy snd
        |> Seq.map (fun (x,grp) -> x, grp |> Seq.map fst |> Seq.toArray)
        |> Seq.fold (fun map (k,v) -> Map.add k v map) map

    let filterBy (feature: _ []) (filter:filter) =
        filter |> Array.map (fun i -> feature.[i])

    let removeMissing (data:(float option * _) seq) =
        seq {   
            for (x,y) in data do
                match x with
                | None -> ignore ()
                | Some(v) -> yield v,y } 
            
    let condent (data: int[][]) =
        let size = 
            data 
            |> Array.sumBy (fun y -> Array.sum y)
            |> float
        data
        |> Array.sumBy (fun y -> 
            let s = Array.sum y |> float
            (s/size) * h y)

    let binnize splits keys feature =
        let splitsCount = List.length splits + 1
        let bins = [| for s in 1 .. splitsCount -> [| for k in 1 .. keys -> 0 |] |]
        for (value,key) in feature do
            bins.[indexOf splits value].[key] <- bins.[indexOf splits value].[key] + 1
        bins

    let splitValue keys (feature:(float option*int)[]) filter =
        let filtered = 
            filterBy feature filter
            |> Array.filter (fun (x,y) -> (x |> Option.isSome))
            |> Array.map (fun (x,y) -> Option.get x, y)
            |> Array.sortBy fst
        let splits = split keys (prepare keys filtered)

        let featurized = binnize splits keys filtered
        splits, condent featurized

    let analyze (labels:int) (feature:(float option*int)[]) =
        let filtered = 
            feature 
            |> Array.filter (fun (x,y) -> (x |> Option.isSome))
            |> Array.map (fun (x,y) -> Option.get x, y)
            |> Array.sortBy fst

        if (filtered.Length <= 1) then 1., [] // TODO: check that
        else
            let splits = split labels (prepare labels filtered)
            match splits with
            | [] -> 1., []
            | _  ->
                let featurized = binnize splits labels filtered
                condent featurized, splits