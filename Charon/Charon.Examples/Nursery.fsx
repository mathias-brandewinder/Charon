#r @"..\Charon\bin\Debug\Charon.dll"

open Charon
open Charon.Tree
open Charon.Featurization
open Charon.Learning
open System
open System.IO
open System.Diagnostics

#time

// Test on the Nursery dataset from UC Irvine ML Repository:
// http://archive.ics.uci.edu/ml/machine-learning-databases/nursery/
// First, download the file to the desktop, and rename it from .data to .txt

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
            line.[8], // the label
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
    let labels = "Survived", (fun (x:string) -> Some(x)) |> Categorical

    let features = 
        [  ("parents", (fun x -> x.parents |> Some) |> Categorical);
           ("has_nurs", (fun x -> x.has_nurs |> Some) |> Categorical);
           ("form", (fun x -> x.form |> Some) |> Categorical);
           ("children", (fun x -> x.children |> Some) |> Categorical);
           ("housing", (fun x -> x.housing |> Some) |> Categorical);
           ("finance", (fun x -> x.finance |> Some) |> Categorical);
           ("social", (fun x -> x.social |> Some) |> Categorical);
           ("health", (fun x -> x.health |> Some) |> Categorical); ]

    printfn "Generating tree"
    let results = basicTree data (labels,features)
    printfn "%s" (results.Pretty)

    printfn "Forecast evaluation: tree"
    printfn "Correct, training: %.4f" (results.TrainingQuality |> Option.get)
    printfn "Correct, validation: %.4f" (results.HoldoutQuality |> Option.get)

    let forestResults = forest data (labels,features)
    printfn "Forest quality: %.4f" forestResults.OutOfBagQuality