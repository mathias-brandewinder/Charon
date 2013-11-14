#load "Entropy.fs"
#load "Continuous.fs"
#load "Tree.fs"

open System
open Charon.Tree

let size = 50000
let classes = 10
let fs = 20
let rng = Random()

let outcomes = [| for i in 1 .. size -> rng.Next(classes) |]

//let features = 
//    [|  yield outcomes |> Array.map (fun x -> (if x = 0 then rng.NextDouble() else rng.NextDouble() + 1.), x);
//        yield outcomes |> Array.map (fun x -> (if x = 2 then rng.NextDouble() else rng.NextDouble() + 1.), x);
//        yield outcomes |> Array.map (fun x -> rng.NextDouble(), x); |]

let features = 
    [|  for f in 1 .. fs -> outcomes |> Array.map (fun x -> rng.NextDouble(), x); |]

let dataset = classes, outcomes, features
let filter = [| 0 .. (size - 1) |]
let remaining = [0..(fs-1)] |> Set.ofList
let selector = id

#time

let tree = growTree dataset filter remaining selector 5