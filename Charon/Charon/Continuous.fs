namespace Charon

module Continuous =

    open System
    open System.Collections.Generic
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

    let prepare keys data =
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

    type Filter = int []

    let filterBy (feature: _ []) (filter:Filter) =
        filter |> Array.map (fun i -> feature.[i])

    let condent (data: (int*int[])[]) =
        let size = 
            data 
            |> Array.sumBy (fun (x,y) -> Array.length y)
            |> float
        data
        |> Array.sumBy (fun (x,y) -> 
            let s = Array.length y |> float
            (s/size) * h y)

    let splitValue keys feature filter =
        let filtered = 
            filterBy feature filter
            |> Array.sortBy fst
        let splits = split keys (prepare keys filtered)
        let initial = 
            filtered 
            |> Seq.countBy (fun  (x,y) -> y)
            |> Seq.map snd
            |> Seq.toArray
            |> h
        let featurized =
            filtered
            |> Array.map (fun (v,c) -> indexOf splits v, c)
            |> Seq.groupBy fst
            |> Seq.map (fun (i,grp) ->
                i, [| for k in 0 .. (keys - 1) -> grp |> Seq.filter (fun (a,b) -> b = k) |> Seq.length |])
            |> Seq.toArray
        let gain = condent featurized - initial
        if (gain > 0.)
        then Some(splits, gain)
        else None