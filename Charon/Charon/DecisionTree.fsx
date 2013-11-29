#load "Index.fs"
#load "Entropy.fs"
#load "Continuous.fs"
#load "Discrete.fs"
#load "Tree.fs"

open Charon
open Charon.Tree
open System
open System.IO
open System.Diagnostics

// Create a synthetic, random dataset and run a tree on it
let test (size: int) (feat: int) (outcomes: int) =

    let rng = System.Random()

    let labels = [| for i in 0 .. size -> rng.Next(0, outcomes + 1) |] 
    let data = [| for f in 1 .. feat -> [| for i in 0 .. size -> rng.Next(0, outcomes + 1) |] |> Discrete.prepare |> Categorical |]

    let dataset = { Classes = feat; Outcomes = labels; Features = data }

    let indexes = [| 0 .. size |]
    let features = [ 0 .. (feat - 1) ] |> Set.ofList
    let minLeaf = 5

    printfn "Initialized"

    let timer = Stopwatch()
    
    timer.Restart()
    let any = id
    let tree = growTree dataset indexes features any minLeaf

    timer.Stop()

    printfn "Tree building: %i ms" timer.ElapsedMilliseconds

    tree