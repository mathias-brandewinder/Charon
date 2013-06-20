#load "Index.fs"
#load "DecisionTree.fs"

open Charon
open Charon.DecisionTree
open System
open System.IO
open System.Diagnostics

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

    let timer = Stopwatch()
    
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
    // create a mapping from each feature value to an int > 0
    let converters =
        [| for var in 0 .. vars -> data |> Array.map (fun line -> line.[var]) |]
        |> Array.map (fun feat -> 
            feat 
            |> Seq.distinct 
            |> Seq.mapi (fun i value -> (value, i + 1))
            |> Map.ofSeq)
    // convert a line to a label + features
    let transform (obs: string []) =
        converters.[8].[obs.[8]],
        [| for i in 0 .. 7 -> converters.[i].[obs.[i]] |]

    let timer = Stopwatch()
    timer.Restart()
    let dataset = 
        [| for var in 0 .. vars -> data |> Array.map (fun line -> converters.[var].[line.[var]]) |> prepare |]
    let trainingSet = dataset.[8], dataset.[0..7]
    timer.Stop()

    printfn "Data preparation: %i ms" timer.ElapsedMilliseconds

    timer.Restart()
    let minLeaf = 5
    let tree = build trainingSet [ 0.. (data |> Array.length) - 1 ] (Set.ofList [ 0 .. (vars - 1) ]) any minLeaf
    timer.Stop()

    printfn "Tree building: %i ms" timer.ElapsedMilliseconds

    printfn "Forecast evaluation"
    let correct = 
        data
        |> Array.map transform 
        |> Array.averageBy (fun x -> 
            let lbl, obs = x
            if lbl = (decide tree obs) then 1. else 0.)
    printfn "Correct: %.3f" correct

let nurseryForest () =

    // set the path to the data file
    let path = @"C:\users\mathias\desktop\nursery.txt"
       
    let data = 
        File.ReadAllLines(path)
        |> Array.map (fun line -> line.Split(','))

    let vars = 8
    // create a mapping from each feature value to an int > 0
    let converters =
        [| for var in 0 .. vars -> data |> Array.map (fun line -> line.[var]) |]
        |> Array.map (fun feat -> 
            feat 
            |> Seq.distinct 
            |> Seq.mapi (fun i value -> (value, i + 1))
            |> Map.ofSeq)
    // convert a line to a label + features
    let transform (obs: string []) =
        converters.[8].[obs.[8]],
        [| for i in 0 .. 7 -> converters.[i].[obs.[i]] |]

    let timer = Stopwatch()
    timer.Restart()
    let dataset = 
        [| for var in 0 .. vars -> data |> Array.map (fun line -> converters.[var].[line.[var]]) |> prepare |]
    let trainingSet = dataset.[8], dataset.[0..7]
    timer.Stop()

    printfn "Data preparation: %i ms" timer.ElapsedMilliseconds

    timer.Restart()
    let minLeaf = 5
    let bagging = 0.75
    let iters = 50

    let forest = forest trainingSet [ 0.. (data |> Array.length) - 1 ] (Set.ofList [ 0 .. (vars - 1) ]) minLeaf bagging iters
    timer.Stop()

    printfn "Forest building: %i ms" timer.ElapsedMilliseconds

    printfn "Forecast evaluation"
    let correct = 
        data
        |> Array.map transform 
        |> Array.averageBy (fun x -> 
            let lbl, obs = x
            if lbl = (forestDecide forest obs) then 1. else 0.)
    printfn "Correct: %.3f" correct