#load "Index.fs"
#load "DecisionTree.fs"

open Charon
open Charon.DecisionTree
open System
open System.IO

#time

// Create a synthetic, random dataset and run a tree on it
let test (size: int) (feat: int) (outcomes: int) =

    let rng = System.Random()

    let labels = [| for i in 0 .. size -> rng.Next(0, outcomes + 1) |] |> prepare
    let data = [| for f in 1 .. feat -> [| for i in 0 .. size -> rng.Next(0, outcomes + 1) |] |> prepare |]

    let dataset = labels, data

    let indexes = [ 0 .. size ]
    let features = [ 0 .. (feat - 1) ] |> Set.ofList
    let minLeaf = 5

    printfn "Initialized"

    let timer = System.Diagnostics.Stopwatch()
    
    timer.Restart()

    let tree = build dataset indexes features any minLeaf

    timer.Stop()

    printfn "Tree building: %i ms" timer.ElapsedMilliseconds

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
        [ for var in 0 .. vars -> data |> Array.map (fun line -> line.[var]) ]
        |> List.mapi (fun i f -> i, f |> Seq.distinct |> Seq.mapi (fun i x -> (x, i)) |> Map.ofSeq)
        |> Map.ofList

    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    let dataset = 
        [| for var in 0 .. vars -> data |> Array.map (fun line -> line.[var]) |]
        |> Array.mapi (fun f data -> data |> Array.map (fun x -> features.[f].[x]) |> prepare)
    let trainingSet = dataset.[8], dataset.[0..7]

    let minLeaf = 5
    let tree = build trainingSet [ 0.. (data |> Array.length) - 1 ] (Set.ofList [ 0 .. (vars - 1) ]) any minLeaf
    timer.Stop()

    printfn "Tree building: %i ms" timer.ElapsedMilliseconds