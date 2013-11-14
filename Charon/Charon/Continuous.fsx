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

let dataset = 
    [|  (0.1,0);
        (0.1,1);
        (0.1,1);
        (1.0,0);
        (1.8,0);
        (1.5,0);
        (1.5,1);
        (2.0,1);
        (2.5,0);
        (2.5,1); |]

let filter = [| 1; 2; 3; 8; 9; |]
let sp = [ 0.5; 1.0 ]

let test = subindex dataset filter sp