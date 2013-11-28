#load "Index.fs"
#load "Entropy.fs"
#load "Continuous.fs"
#load "Discrete.fs"
#load "Tree.fs"

open System
open Charon
open Charon.Tree

let size = 10000
let classes = 3

let rng = Random()

let outcomes = [| for i in 1 .. size -> rng.Next(classes) |]

let features = 
    [|  yield outcomes |> Array.map (fun x -> (if x = 0 then Some(rng.NextDouble()) else Some(rng.NextDouble() + 1.)), x) |> Numeric;
        yield outcomes |> Array.map (fun x -> (if x = 2 then Some(rng.NextDouble()) else Some(rng.NextDouble() + 1.)), x) |> Numeric;
        yield outcomes |> Array.map (fun x -> Some(rng.NextDouble()), x) |> Numeric;
        yield outcomes |> Array.map (fun x -> if x = 0 then 0 else 1) |> Charon.Discrete.prepare |> Categorical; |]

let dataset = { Classes = classes; Outcomes = outcomes; Features = features }
let filter = [| 0 .. (size - 1) |]
let remaining = [ 0; 1; 2; 3; ] |> Set.ofList
let selector = id

#time

let tree = growTree dataset filter remaining selector 5