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

// Test on the Nursery dataset from UC Irvine ML Repository:
// http://archive.ics.uci.edu/ml/machine-learning-databases/nursery/

// representation of an observation;
// the 8 available features are simply
// retrieved as a string, which is treated
// as a categorical.
type observation = {    
    Var1: string;
    Var2: string;
    Var3: string;
    Var4: string;
    Var5: string;
    Var6: string;
    Var7: string;
    Var8: string; }

let readDataset () =
    // Path to the data file:
    let desktopPath = Environment.GetFolderPath(Environment.SpecialFolder.Desktop)
    let nurseryPath = desktopPath + @"\nursery.txt"

    let timer = Stopwatch()

    let data = 
        File.ReadAllLines(nurseryPath)
        |> Array.map (fun line -> line.Split(','))
        |> Array.filter (fun line -> Array.length line = 9)
        |> Array.map (fun line ->
            line.[8] |> StringCategory, // the label
            { Var1 = line.[0];
              Var2 = line.[1];
              Var3 = line.[2];
              Var4 = line.[3];
              Var5 = line.[4];
              Var6 = line.[5];
              Var7 = line.[6];
              Var8 = line.[7]; })

    timer.Stop()
    printfn "Data set read: %i ms" timer.ElapsedMilliseconds
    printfn "Training set size: %i" (Array.length data)
    data

// Test on the Nursery dataset (see comments on top of file)
let nursery () =

    let data = readDataset ()

    // define how the features should be extracted
    let features = 
        [| (fun x -> x.Var1 |> StringCategory);
           (fun x -> x.Var2 |> StringCategory);
           (fun x -> x.Var3 |> StringCategory);
           (fun x -> x.Var4 |> StringCategory);
           (fun x -> x.Var5 |> StringCategory);
           (fun x -> x.Var6 |> StringCategory);
           (fun x -> x.Var7 |> StringCategory);
           (fun x -> x.Var8 |> StringCategory); |]

    let minLeaf = 5
    let id3 = createID3Classifier data features minLeaf

    printfn "Forecast evaluation"
    let correct = 
        data
        |> Array.averageBy (fun (label, obs) -> 
            if label = Some(id3 obs) then 1. else 0.)
    printfn "Correct: %.4f" correct

    let minLeaf = 5 // min observations per leaf
    let bagging = 0.75 // proportion of sample used for estimation
    let iters = 50 // number of trees to grow
    let rng = Random(42)
    let forest = createForestClassifier data features minLeaf bagging iters rng
            
    let correct = 
        data
        |> Array.averageBy (fun (label, obs) -> 
            if label = Some(forest obs) then 1. else 0.)
    printfn "Correct: %.4f" correct