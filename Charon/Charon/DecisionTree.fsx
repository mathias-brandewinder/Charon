#load "Index.fs"
#load "DecisionTree.fs"
#r @"..\packages\FSharp.Data.1.1.9\lib\net40\FSharp.Data.dll"

open Charon
open Charon.DecisionTree
open System
open System.IO
open System.Diagnostics
open FSharp.Data

#time

type DataSet = CsvProvider<"titanic.csv", 
                           Schema ="PassengerId=int, Pclass->Class, Parch->ParentsOrChildren, SibSp->SiblingsOrSpouse", 
                           SafeMode=true, 
                           PreferOptionals=true>
type Passenger = DataSet.Row

let training =
    use data = new DataSet()
    [| for passenger in data.Data -> 
        passenger.Survived |> Categorical, // the label
        passenger |]

let features = [|
    "Sex", (fun (x:Passenger) -> x.Sex |> Categorical);
    "Class", (fun x -> x.Class |> Categorical); |]

let classifier, report = createID3Classifier training features { DefaultID3Config with DetailLevel = Verbose }
report.Value.Pretty()

// Correctly classified
[| for passenger in training -> 
    if classifier(snd passenger) = (fst passenger).Value then 1. else 0. |]
|> Array.average
    
let rfFeatures = [|
    "Sex", (fun (x:Passenger) -> x.Sex |> Categorical);
    "Class", (fun x -> x.Class |> Categorical);
    "Embark", (fun x -> x.Embarked |> Categorical);
    "Family", (fun x -> x.ParentsOrChildren |> Categorical);
    "Spouse", (fun x -> x.SiblingsOrSpouse |> Categorical);
    "Age", (fun x -> x.Age |> Option.map (fun age -> if age < 10m then "Kid" else "Adult") |> Categorical);  |]

let rfClassifier, rfReport = createForestClassifier training rfFeatures { DefaultRFConfig with DetailLevel = Verbose }



[| for passenger in training -> 
    if rfClassifier(snd passenger) = (fst passenger).Value then 1. else 0. |]
|> Array.average


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
        [| ("parents", fun x -> x.parents |> Some |> Categorical);
           ("has_nurs", fun x -> x.has_nurs |> StringCategory);
           ("form", fun x -> x.form |> StringCategory);
           ("children", fun x -> x.children |> StringCategory);
           ("housing", fun x -> x.housing |> StringCategory);
           ("finance", fun x -> x.finance |> StringCategory);
           ("social", fun x -> x.social |> StringCategory);
           ("health", fun x -> x.health |> StringCategory); |]

    let treeClassifier, report = createID3Classifier data features { DefaultID3Config with DetailLevel = Verbose }
    report.Value.Pretty()

    printfn "Forecast evaluation: tree"
    let correct = 
        data
        |> Array.averageBy (fun (label, obs) -> 
            if label = Some(treeClassifier obs) then 1. else 0.)
    printfn "Correct: %.4f" correct

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

    let tree = growTree dataset indexes features any minLeaf

    timer.Stop()

    printfn "Tree building: %i ms" timer.ElapsedMilliseconds

    tree