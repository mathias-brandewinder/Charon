#r @"..\Charon\bin\Debug\Charon.dll"

open Charon
open Charon.DecisionTree
open System
open System.IO
open System.Diagnostics

#time

// Test on the Nursery dataset from UC Irvine ML Repository:
// http://archive.ics.uci.edu/ml/machine-learning-databases/nursery/

// representation of an observation;
// the 8 available features are simply
// retrieved as a string, which is treated
// as a categorical.
type observation = {    
    parents: string;
    has_nurs: string;
    form: string;
    children: string;
    housing: string;
    finance: string;
    social: string;
    health: string; }

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
            { parents = line.[0];
              has_nurs = line.[1];
              form = line.[2];
              children = line.[3];
              housing = line.[4];
              finance = line.[5];
              social = line.[6];
              health = line.[7]; })

    timer.Stop()
    printfn "Data set read: %i ms" timer.ElapsedMilliseconds
    printfn "Training set size: %i" (Array.length data)
    data

// Test on the Nursery dataset (see comments on top of file)
let nursery () =

    let data = readDataset ()

    // define how the features should be extracted
    let features = 
        [| ("parents", fun x -> x.parents |> StringCategory);
           ("has_nurs", fun x -> x.has_nurs |> StringCategory);
           ("form", fun x -> x.form |> StringCategory);
           ("children", fun x -> x.children |> StringCategory);
           ("housing", fun x -> x.housing |> StringCategory);
           ("finance", fun x -> x.finance |> StringCategory);
           ("social", fun x -> x.social |> StringCategory);
           ("health", fun x -> x.health |> StringCategory); |]

    printfn "Generating tree"
    let treeClassifier, report = createID3Classifier data features { DefaultID3Config with DetailLevel = Verbose }
    report.Value.Pretty()

    printfn "Forecast evaluation: tree"
    let correct = 
        data
        |> Array.averageBy (fun (label, obs) -> 
            if label = Some(treeClassifier obs) then 1. else 0.)
    printfn "Correct: %.4f" correct

    printfn "Generating forest"
    let iters = 50 // number of trees to grow
    let rng = Random(42) // setting explicit RNG to replicate results
    let config = { DefaultRFConfig with RNG = Some(rng); Iterations = iters }
    let forestClassifier, report = createForestClassifier data features config

    printfn "Forecast evaluation: forest"            
    let correct = 
        data
        |> Array.averageBy (fun (label, obs) -> 
            if label = Some(forestClassifier obs) then 1. else 0.)
    printfn "Correct: %.4f" correct

