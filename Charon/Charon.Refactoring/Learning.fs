namespace Charon.Refactoring

module Learning =

    open Charon.Refactoring
    open Charon.Refactoring.Entropy
    open Charon.Refactoring.Featurization
    open Charon.Refactoring.Tree

    type Variable =
        | Disc of int [][] // outcome, and corresponding observation indexes
        | Cont of (float option*int option) [] // values, and label value

    type Dataset = { Classes:int; Outcomes:int []; Features: Variable [] }
    
    type Settings = { MinLeaf:int; }

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

    let applyFilter (filter:filter) (data: _ []) =
        filter |> Array.map (fun i -> data.[i])

    let countCases (data:_ []) =
        data |> Seq.countBy id |> Seq.map snd |> Seq.toArray

    let mostLikely (outcomes: _ []) = 
        outcomes 
        |> Seq.countBy id 
        |> Seq.maxBy snd 
        |> fst

    let arrayFilter (array:int[]) (filter:int[]) =
        filter |> Array.filter (fun x -> array |> Array.exists (fun z -> z = x))

    // TODO FIX THIS: NEED TO FILTER!!!
    let filteredBy (filter:filter) (feature:Variable) =
        match feature with
        | Disc(indexes) -> 
            [| for i in indexes -> arrayFilter i filter |] |> Disc
        | Cont(x) -> feature

    // TODO: IMPLEMENT THIS!
    // Compute entropy and branch data
    // Could be done cleaner, with different data struct
    // for Disc and Cont (which requires splits). Later.
    let conditional (data:int[][]) (labels:_[]) =
        let total = data |> Array.sumBy (fun x -> Array.length x |> float)
        data 
        |> Seq.map (fun x -> seq { for i in x -> labels.[i] } |> countFrom)
        |> Seq.map (fun x -> Array.sum x |> float, h x)
        |> Seq.sumBy (fun (count,h) -> h * count / total)
        
    let bestSplit (feature:Variable) (labels:int[]) =
        match feature with
        | Disc(x) -> 
            let c = conditional x labels
            c, [] // returning empty splits list
        | Cont(x) -> 0., [1.;2.;3.]
        


    let selectFeature (dataset: Dataset) // full dataset
                      (filter: filter) // indexes of observations in use
                      (remaining: int Set) = // indexes of usable features 
        
        let labels = dataset.Outcomes |> applyFilter filter
        let initialEntropy = labels |> countCases |> h

        let candidates = 
            seq { for i in remaining -> i, dataset.Features.[i] |> filteredBy filter }
            |> Seq.map (fun (i,f) -> i,f, bestSplit f (dataset.Outcomes))
            |> Seq.map (fun (i,f,(h,s)) -> (i,f,s,initialEntropy - h)) // replace h with gain
            |> Seq.filter (fun (_,_,_,g) -> g > 0.) // do I need to check splits non empty for continuous?

        if (Seq.isEmpty candidates) then None
        else candidates |> Seq.maxBy (fun (_,_,_,g) -> g) |> fun (i,f,s,_) -> (i,f,s) |> Some



    let rec train (dataset:Dataset) (filter:filter) (remaining:int Set) (settings:Settings) =

        let mostLikely () = dataset.Outcomes |> applyFilter filter |> mostLikely
        
        if (remaining = Set.empty) then
            Leaf(mostLikely ())
        elif (Array.length filter < settings.MinLeaf) then
            Leaf(mostLikely ())
        else
            let candidates = remaining
            let best = selectFeature dataset filter candidates
            
            printfn "BEST: %A" best

            match best with
            | None -> Leaf(mostLikely ())
            | Some(i,f,s) -> // index, feature, splits, gain
                let remaining = remaining |> Set.remove i
                match f with
                | Disc(indexes) ->
                    let branch = { FeatIndex = i; Default = mostLikely () }
                    Branch(Cat(branch, [| for filt in indexes -> train dataset filt remaining settings |]))
//                    Leaf(mostLikely ()) // TEMPORARY
                | Cont(_) ->
                    let branch = { NumBranch.FeatIndex = i; Default = mostLikely (); Splits = s }
                    Leaf(mostLikely ()) // TEMPORARY