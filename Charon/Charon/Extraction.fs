namespace Charon

module Extraction =

    open System

    type Feature<'a> =
        | Categorical of ('a -> string option)
        | Numerical   of ('a -> float option)

    let preprocess dataset features =
        features
        |> List.map (fun feat ->
            match feat with
            | Numerical(f) -> Numerical(f), Map.empty
            | Categorical(f) ->
                Categorical(f),
                dataset 
                |> Seq.map f 
                |> Seq.distinct
                |> Seq.choose id
                |> Seq.mapi (fun i v -> v,i) 
                |> Map.ofSeq)
    
    let Categorical<'a> (value: Option<'a>) =
        value |> Option.map (fun x -> x.ToString())

    let Numerical<'a> (value: Option<'a>) =
        value |> Option.map (fun x -> Convert.ToDouble(x))