namespace Charon

[<AutoOpen>]
module Learning =

    open System
    open Charon
    open Charon.Entropy
    open Charon.Featurization
    open Charon.Tree

    type Variable =
        | Disc of int [][] // outcome, and corresponding observation indexes
        | Cont of (float option*int) [] // values, and label value

    type Dataset = { Classes:int; Outcomes:int []; Features: Variable [] }
    
    type Settings = { MinLeaf:int; Holdout:float; RandomSeed:int; ForestSize:int }
    
    type FeaturesSelector = int Set -> int Set

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
        |> Seq.map (fun (lbl,obs) -> feature obs |> contConv, label lbl |> discConv |> Option.get)
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

    let filteredBy (filter:filter) (feature:Variable) =
        match feature with
        | Disc(indexes) -> 
             [| for i in indexes -> Index.intersect i filter |] |> Disc
        | Cont(x) -> 
            [| for i in filter -> x.[i] |] |> Cont

    let conditional (data:int[][]) (labels:_[]) =
        let total = data |> Array.sumBy (fun x -> Array.length x |> float)
        data 
        |> Seq.map (fun x -> seq { for i in x -> labels.[i] } |> countFrom)
        |> Seq.map (fun x -> Array.sum x |> float, h x)
        |> Seq.sumBy (fun (count,h) -> h * count / total)
        
    let bestSplit (feature:Variable) (labels:int[]) (classes:int) =
        match feature with
        | Disc(x) -> conditional x labels, []
        | Cont(x) -> Continuous.analyze classes x 
        
    let selectFeature (dataset: Dataset) // full dataset
                      (filter: filter) // indexes of observations in use
                      (remaining: int Set) = // indexes of usable features 
        
        let labels = dataset.Outcomes |> applyFilter filter
        let initialEntropy = labels |> countCases |> h

        let candidates = 
            seq { for i in remaining -> i, dataset.Features.[i] |> filteredBy filter }
            |> Seq.map (fun (i,f) -> i,f, bestSplit f (dataset.Outcomes) (dataset.Classes))
            |> Seq.map (fun (i,f,(h,s)) -> (i,f,s,initialEntropy - h)) // replace h with gain
            |> Seq.filter (fun (_,_,_,g) -> g > 0.) 

        if (Seq.isEmpty candidates) then None
        else candidates |> Seq.maxBy (fun (_,_,_,g) -> g) |> fun (i,f,s,_) -> (i,f,s) |> Some

    let rec train (dataset:Dataset) (filter:filter) (remaining:int Set) (fsselector:FeaturesSelector) (settings:Settings) =

        let mostLikely () = dataset.Outcomes |> applyFilter filter |> mostLikely
        
        if (remaining = Set.empty) then
            Leaf(mostLikely ())
        elif (Array.length filter <= settings.MinLeaf) then
            Leaf(mostLikely ())
        else
            let candidates = remaining |> fsselector
            let best = selectFeature dataset filter candidates
            
            match best with
            | None -> Leaf(mostLikely ())
            | Some(i,f,s) -> // index, feature, splits, gain
                let remaining = remaining |> Set.remove i
                match f with
                | Disc(indexes) ->
                    let branch = { FeatIndex = i; Default = mostLikely () }
                    Branch(Cat(branch, [| 
                                          for filt in indexes -> 
                                              if Array.length filt = 0 then Leaf(mostLikely ())
                                              else train dataset filt remaining fsselector settings 
                                       |]))
                | Cont(_) ->
                    let branch = { NumBranch.FeatIndex = i; Default = mostLikely (); Splits = s }
                    let feat = // this is ugly as hell
                        match dataset.Features.[i] with
                        | Cont(x) -> x
                        | _ -> failwith "kaboom"
                    let filters = Continuous.subindex feat filter s
                    Branch(Num(branch, [|
                                           for kv in filters ->
                                              let filt = kv.Value
                                              if Array.length filt = 0 then Leaf(mostLikely ())
                                              else train dataset filt remaining fsselector settings 
                                       |]))

    type Results<'a> = 
        {   Classifier:'a -> string;
            Tree: Tree;
            Settings: Settings;
            TrainingQuality: float option;
            HoldoutQuality: float option;
            Pretty: string }

    let DefaultSettings = { MinLeaf = 5; Holdout = 0.20; RandomSeed = 314159; ForestSize = 100 }

    let basicTree<'l,'a> (data:('l*'a) seq) ((labels:string*Feature<'l>), (features:(string*Feature<'a>) list)) (settings:Settings) =

        let fs = List.length features

        let labelsMap = createFeatureMap (data |> Seq.map fst) (snd labels)
        let predictionToLabel = labelsMap.InsideOut
        let maps = createTranslators data labels features

        let (labelizer,featurizers) = translators data (labels,features)
                        
        let dataset = prepare data (labelizer,featurizers)

        // TODO: improve with proper sampling
        let rng = Random(settings.RandomSeed)
        let xs = Array.length dataset.Outcomes
        let trainingsample,validationsample = [| 0 .. (xs - 1) |] |> Array.partition (fun x -> rng.NextDouble() > settings.Holdout)

        let tree = train dataset trainingsample ([0..(fs-1)] |> Set.ofList) id settings

        let converter = 
            let fs = featurizers |> List.unzip |> snd
            fun (obs:'a) -> List.map (fun f -> f obs) fs |> List.toArray
            
        let classifier = fun (obs:'a) -> labelsMap.InsideOut.[ decide tree (converter obs) ]

        let predictions =
            (dataset.Outcomes, data) 
            ||> Seq.zip
            |> Seq.map (fun (l,(_,v)) -> if l = decide tree (converter v) then 1. else 0.)
            |> Seq.toArray

        let trainingquality = 
            if (Array.length trainingsample = 0) then None
            else
                seq { for i in trainingsample -> predictions.[i] }
                |> Seq.average |> Some
        let holdoutquality = 
            if (Array.length validationsample = 0) then None
            else seq { for i in validationsample -> predictions.[i] } |> Seq.average |> Some

        let view = pretty tree maps

        { Classifier = classifier;
          Tree = tree;
          Settings = settings;
          TrainingQuality = trainingquality;
          HoldoutQuality = holdoutquality
          Pretty = view;
        }

    type ForestResults<'a> = 
        {   Classifier:'a -> string;
            Settings: Settings;
            OutOfBagQuality: float; }

    let pickN n (rng: Random) (from: int Set) =
        let array = Set.toArray from 
        seq { for i in 1 .. n -> array.[rng.Next(0, Array.length array)] } |> Set.ofSeq          
          
    let forest<'l,'a> (data:('l*'a) seq) ((labels:string*Feature<'l>), (features:(string*Feature<'a>) list)) (settings:Settings) =

        let fs = List.length features

        let labelsMap = createFeatureMap (data |> Seq.map fst) (snd labels)
        let predictionToLabel = labelsMap.InsideOut
        let maps = createTranslators data labels features

        let (labelizer,featurizers) = translators data (labels,features)
                        
        let dataset = prepare data (labelizer,featurizers)
        let rng = System.Random(settings.RandomSeed)
        let xs = Array.length dataset.Outcomes
        let samplesize = float xs * (1. - settings.Holdout) |> int
        let fsset = sqrt (float fs) |> ceil |> int
        let fsselector = pickN fsset rng
        let featuresSample = ([0..(fs-1)] |> Set.ofList)

        let trees = settings.ForestSize

        let models = 
            [| for t in 1 .. trees do
                let trainingsample = [| for i in 0 .. samplesize -> rng.Next(xs) |] |> Array.sort
                yield trainingsample, train dataset trainingsample featuresSample fsselector settings |]
        
        let forest = models |> Array.map snd 
         
        let converter = 
            let fs = featurizers |> List.unzip |> snd
            fun (obs:'a) -> List.map (fun f -> f obs) fs |> List.toArray
            
        // for each model, what observations are used
        let samples = 
            models 
            |> Seq.mapi (fun i (x,_) -> i, x |> Set.ofArray) |> Seq.toArray

        // observation index, and all models where it is OOB
        let oob = 
            seq { 0 .. (xs - 1) } 
            |> Seq.map (fun obs ->
                obs, samples |> Seq.filter (fun (j,xs) -> not (Set.contains obs xs)) |> Seq.map fst |> Set.ofSeq)
            |> Seq.filter (fun (obs,models) -> Set.count models > 0)

        let oobquality =             
            let predictiondata = Seq.zip dataset.Outcomes data |> Seq.toArray
            oob
            |> Seq.map (fun (obs,models) ->
                let (l,(_,v)) = predictiondata.[obs]
                seq { for model in models -> 
                          let tree = forest.[model]
                          decide tree (converter v) }
                |> Seq.countBy id
                |> Seq.maxBy snd
                |> fst
                |> fun x -> if x = l then 1. else 0.)
            |> Seq.average
                               
        let classifier = fun (obs:'a) -> 
            forest 
            |> Seq.map (fun tree -> labelsMap.InsideOut.[ decide tree (converter obs) ])
            |> Seq.countBy id
            |> Seq.maxBy snd
            |> fst

        {   Classifier = classifier;
            Settings = settings;
            OutOfBagQuality = oobquality; }
