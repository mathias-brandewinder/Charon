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

    let extractor (fs:(string*Feature<'a>) list) (fsMap:Map<string,int> list) =        
        let combined = (fs,fsMap) ||> List.zip
        fun (obs:'a) -> combined |> List.map (fun ((_,f),map) -> convert f map obs)