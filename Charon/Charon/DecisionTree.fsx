#load "DecisionTree.fs"

open Charon.DecisionTree
open System
open System.IO

#time

// Create a synthetic, random dataset and run a tree on it
let test (size: int) (feat: int) (outcomes: int) =

    let rng = System.Random()

    let data = [ for f in 0 .. feat -> [| for i in 0 .. size -> rng.Next(0, outcomes + 1) |] |> prepare ]

    let dataset = data |> List.mapi (fun i x -> i, x) |> Map.ofList

    let initial = [ 0 .. size ] |> Set.ofList
    let features = [ 0 .. (feat - 1) ] |> Set.ofList

    printfn "Initialized"

    let tree = build dataset initial features feat
    
    printfn "Done!"

// Test on the Nursery dataset for UC Irvine:
// http://archive.ics.uci.edu/ml/machine-learning-databases/nursery/
let nursery () =

    // set the path to the data file
    let path = @"C:\users\mathias\desktop\nursery.txt"
       
    let data = 
        File.ReadAllLines(path)
        |> Array.map (fun line -> line.Split(','))

    let vars = 8
    let features = 
        [ for var in 0 .. 8 -> data |> Array.map (fun line -> line.[var]) ]
        |> List.mapi (fun i f -> i, f |> Seq.distinct |> Seq.mapi (fun i x -> (x, i)) |> Map.ofSeq)
        |> Map.ofList

    let dataset = 
        [ for var in 0 .. 8 -> data |> Array.map (fun line -> line.[var]) ]
        |> List.mapi (fun f data -> f, data |> Array.map (fun x -> features.[f].[x]) |> prepare)
        |> Map.ofList

    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    let t = build dataset (Set.ofList [0.. (data |> Array.length) - 1 ]) (Set.ofList [0..7]) 8
    timer.Stop()
    printfn "Tree building: %i ms" timer.ElapsedMilliseconds;