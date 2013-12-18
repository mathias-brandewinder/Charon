namespace Charon.Refactoring

//open System
//
//type Feature<'a> =
//    | Cat of ('a -> string option)
//    | Num of ('a -> float option)
//
//type As () =
//
//    static member Category(value: 'a -> Option<'b>) =
//        Cat (value >> Option.map (fun x -> x.ToString()))
//
//    static member Category(value: 'a -> 'b) =
//        Cat (value >> Some >> Option.map (fun x -> x.ToString()))
//
//    static member Number(value: 'a -> Option<'b>) =
//        Num (value >> Option.map (fun x -> Convert.ToDouble(x)))
//
//    static member Number(value: 'a -> 'b) =
//        Num (value >> Some >> Option.map (fun x -> Convert.ToDouble(x)))

module Featurization =

    open System

    type ValueType = 
        | Discrete
        | Continuous

    type Value =
        | Int   of int option
        | Float of float option
    
    type Converter<'a> = ValueType * ('a -> Value)

    type Feature<'a> =
        | Cat of ('a -> string option)
        | Num of ('a -> float option)

    let Categorical<'a,'b> (value: 'a -> Option<'b>) =
        Cat (value >> Option.map (fun x -> x.ToString())) 

    let Numerical<'a,'b> (value: 'a -> Option<'b>) =
        Num (value >> Option.map (fun x -> Convert.ToDouble(x)))

    let convert (f:Feature<'a>) (map: Map<string,int>) (obs:'a) =
        match f with
        | Cat(g) -> 
            let key = g obs
            match key with
            | None -> Int(None)
            | Some(k) ->
                if map |> Map.containsKey k
                then Int(Some(map.[k]))
                else Int(None)
        | Num(g) -> g obs |> Float

    type FeatureMap = 
        { OutsideIn:Map<string,int>; 
          InsideOut:Map<int,string> }

    let createFeatureMap (data:'a seq) (f:Feature<'a>) = 
        match f with
        | Cat(g) -> 
            let oneWay = 
                data 
                |> Seq.map g
                |> Seq.distinct
                |> Seq.choose id 
                |> Seq.mapi (fun i k -> k,i) 
                |> Map.ofSeq
            let back = 
                oneWay 
                |> Map.toSeq 
                |> Seq.map (fun (x,y) -> (y,x)) 
                |> Map.ofSeq
            { OutsideIn = oneWay; InsideOut = back }
        | Num(_) -> { OutsideIn = Map.empty; InsideOut = Map.empty }

    // Create a function that fully converts an observation
    // into an array of Value (i.e. int or float)
    let extractor (fs:(string*Feature<'a>) list) (fsMap:Map<string,int> list) =        
        let combined = (fs,fsMap) ||> List.zip
        fun (obs:'a) -> combined |> List.map (fun ((_,f),map) -> convert f map obs)

    let createExtractor (data:'a seq) (fs:(string*Feature<'a>) list) =
        let map = fs |> List.map (fun (name,f) -> name, createFeatureMap data f)
        let ext = extractor fs (map |> List.map snd |> List.map (fun x -> x.OutsideIn))
        map,ext