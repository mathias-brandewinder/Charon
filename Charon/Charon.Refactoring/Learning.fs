namespace Charon.Refactoring

module Learning =

    open Charon.Refactoring
    open Charon.Refactoring.Featurization
    open Charon.Refactoring.Tree

    type Variable =
        | Disc of int [][] // outcome, and corresponding observation indexes
        | Cont of (float option*int option) [] // values, and label value

    type Dataset = { Classes:int; Outcomes:int []; Features: Variable [] }

    let discConv (x:Value) =
        match x with
        | Int(x) -> x
        | _      -> failwith "Not an int - Boom!"

    let contConv (x:Value) =
        match x with
        | Float(x) -> x
        | _        -> failwith "Not a float - Boom!"

    let continuous (data:('l*'a) seq) (feature:'a -> Value) (label:'l -> Value) =
        data
        |> Seq.map (fun (lbl,obs) -> feature obs |> contConv, label lbl |> discConv)
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
        |> Seq.map (fun obs -> feature obs |> discConv |> Option.get)
        |> Seq.distinct
        |> Seq.length

    let intlabels (data: 'a seq) (feature:'a -> Value) =
        data 
        |> Seq.map (fun obs -> feature obs |> discConv |> Option.get) 
        |> Seq.toArray
        
    let translators (data:('l*'a) seq) (labels:(string*Feature<'l>), (features:(string*Feature<'a>) list)) = 
        
        let ls = data |> Seq.map fst
        let obs = data |> Seq.map snd

        let labelsMap = createFeatureMap ls (snd labels)
        let labelizer = converter (snd labels) (labelsMap.OutsideIn)
        
        let featurizers = 
            features
            |> List.map (fun (n,f) -> 
                let map = createFeatureMap obs f
                converter f (map.OutsideIn))

        labelizer, featurizers
            
    let prepare (data:('l*'a) seq) ((labels:Converter<'l>), (features:Converter<'a> list)) =
        
        let valueType,lblconverter = labels

        // Currently filtering out every observation that has no label.
        // Might want to revisit later, in case missing labels can
        // provide information on classifier reliability?
        let haslabel (x:Value) =
            match x with
            | Int(v) -> v |> Option.isSome
            | Float(v) -> v |> Option.isSome

        let data = 
            data 
            |> Seq.filter (fun (lbl,obs) -> lblconverter lbl |> haslabel)

        let classes,labels = 
            match valueType with
            | Continuous -> failwith "Regression not implemented yet."
            | Discrete   -> 
                let ls = data |> Seq.map fst
                countClasses ls lblconverter, intlabels ls lblconverter

        let transformed = 
            let observations = data |> Seq.map snd
            features
            |> List.map (fun feat ->
                let valueType, converter = feat
                match valueType with
                | Discrete   -> discrete observations converter |> Disc
                | Continuous -> continuous data converter lblconverter |> Cont)
            |> List.toArray

        { Classes = classes; Outcomes = labels; Features = transformed }

    type Settings = { MinLeaf:int; }

    let filterBy (feature: _ []) (filter:filter) =
        filter |> Array.map (fun i -> feature.[i])

    let mostLikely (outcomes: _ []) = 
        outcomes 
        |> Seq.countBy id 
        |> Seq.maxBy snd 
        |> fst

    let train (dataset:Dataset) (filter:filter) (remaining:int Set) (settings:Settings) =
        if (remaining = Set.empty) then
            Leaf(filter |> filterBy (dataset.Outcomes) |> mostLikely)
        elif (Array.length filter < settings.MinLeaf) then
            Leaf(filter |> filterBy (dataset.Outcomes) |> mostLikely)
        else        
            Leaf(filter |> filterBy (dataset.Outcomes) |> mostLikely)
