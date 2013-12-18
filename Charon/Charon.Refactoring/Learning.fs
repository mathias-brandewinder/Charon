namespace Charon.Refactoring

module Learning =

    open Charon.Refactoring.Featurization

    type Variable =
        | Disc of int [][] // outcome, and corresponding observation indexes
        | Cont of (float option*int option) [] // values, and label value

    type Dataset = { Classes:int; Outcomes:int option []; Features: Variable [] }

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

    let countClasses (data:'a seq) (feature:'a -> Value) =
        data
        |> Seq.map (fun obs -> feature obs |> discConv)
        |> Seq.distinct
        |> Seq.choose id
        |> Seq.length

    let intlabels (data: 'a seq) (feature:'a -> Value) =
        data 
        |> Seq.map (fun obs -> feature obs |> discConv) 
        |> Seq.toArray
        
    let prepare (data:'a seq) (labels:Converter<'a>) (features:Converter<'a> list) =
        
        // Should I filter out the labels that are None / missing?
        // Useless for a tree, but interesting maybe as indication
        // of how reliable the tree is?
        let valueType,lblconverter = labels

        let classes,labels = 
            match valueType with
            | Continuous -> failwith "Regression not implemented yet."
            | Discrete   -> countClasses data lblconverter, intlabels data lblconverter

        let transforms = 
            features
            |> List.map (fun feat ->
                let valueType, converter = feat
                match valueType with
                | Discrete   -> discrete data converter |> Disc
                | Continuous -> continuous data converter lblconverter |> Cont)
            |> List.toArray

        { Classes = classes; Outcomes = labels; Features = transforms }