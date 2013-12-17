namespace Charon.Refactoring

module Learning =

    open Charon.Refactoring.Featurization

    type Variable =
        | Disc of int [][]
        | Cont of (float option*int option) []

    let discConv (x:Value) =
        match x with
        | Int(x) -> x
        | _      -> failwith "Not an int - Boom!"

    let contConv (x:Value) =
        match x with
        | Float(x) -> x
        | _        -> failwith "Not a float - Boom!"

    let continuous (data:'a seq) (feature:'a -> Value) (label:'a -> Value) =
        data
        |> Seq.map (fun obs -> feature obs |> contConv, label obs |> discConv)
        |> Seq.sortBy fst
        |> Seq.toArray

    let discrete (data:'a seq) (feature:'a -> Value) =
        data
        |> Seq.mapi (fun i obs -> i, feature obs |> discConv)
        |> Seq.filter (fun (i,v) -> Option.isSome v)
        |> Seq.map (fun (i,v) -> i, Option.get v)
        |> Seq.groupBy (fun (i,v) -> v)
        |> Seq.sortBy fst
        |> Seq.map (fun (v,is) -> is |> Seq.map fst |> Seq.toArray)
        |> Seq.toArray

    let prepare (data:'a seq) (labels:Converter<'a>) (features:Converter<'a> list) =
        
        let valueType,lblconverter = labels
        match valueType with
        | Continuous -> failwith "Regression not implemented yet."
        | Discrete   -> ignore ()

        features
        |> List.map (fun feat ->
            let valueType, converter = feat
            match valueType with
            | Discrete   -> discrete data converter |> Disc
            | Continuous -> continuous data converter lblconverter |> Cont)