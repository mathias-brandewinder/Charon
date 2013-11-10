#load "Entropy.fs"
#load "Continuous.fs"

open Charon.Entropy
open Charon.MDL
open Charon.Continuous

open System

let rng = Random()

#time

// Generate k segments of different distributions

let sample = 10000
let classes = 10
let ks = classes

let raw = 
    [|
        for k in 0 .. (ks - 1) do
            for i in 1 .. sample -> (k |> float) + Math.Round(rng.NextDouble(), 2), k 
    |]

let clean = prepare classes raw

let splits = split classes (clean |> Array.sortBy fst)