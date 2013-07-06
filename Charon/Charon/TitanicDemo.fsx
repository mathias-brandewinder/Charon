#r @"..\..\Charon\packages\FSharp.Data.1.1.8\lib\net40\FSharp.Data.dll"
#load "Index.fs"
#load "DecisionTree.fs"

open Charon
open Charon.DecisionTree
open System
open FSharp.Data.Csv

// let's define a type for our observations
type Passenger = {
    Id: string; 
    Class: string;
    Name: string;
    Sex: string;
    Age: string;
    SiblingsOrSpouse: string;
    ParentsOrChildren: string;
    Ticket: string;
    Fare: string;
    Cabin: string;
    Embarked: string }

// now let's retrieve examples from the training CSV file
// http://www.kaggle.com/c/titanic-gettingStarted/data
let path = @"C:\Users\Mathias\Documents\GitHub\Charon\Charon\Charon\train.csv"
let data = CsvFile.Load(path).Cache()

let trainingSet =
    [| for line in data.Data -> 
        line.GetColumn "Survived" |> Some, // the label
        {   Id = line.GetColumn "PassengerId"; 
            Class = line.GetColumn "Pclass";
            Name = line.GetColumn "Name";
            Sex = line.GetColumn "Sex";
            Age = line.GetColumn "Age";
            SiblingsOrSpouse = line.GetColumn "SibSp";
            ParentsOrChildren = line.GetColumn "Parch";
            Ticket = line.GetColumn "Ticket";
            Fare = line.GetColumn "Fare";
            Cabin = line.GetColumn "Cabin";
            Embarked = line.GetColumn "Embarked" } |]

// ID3 Decision Tree example
let treeExample =
    
    // let's define what features we want included
    let features = 
        [| (fun x -> x.Sex |> StringCategory);
           (fun x -> x.Class |> StringCategory); |]

    // train the classifier
    let minLeaf = 5
    let classifier = createID3Classifier trainingSet features minLeaf

    // let's see how good the model is on the training set
    printfn "Forecast evaluation"
    let correct = 
        trainingSet
        |> Array.averageBy (fun (label, obs) -> 
            if label = Some(classifier obs) then 1. else 0.)
    printfn "Correct: %.4f" correct

// Random Forest example
let forestExample = 

    // let's define what features we want included
    let binnedAge (age: string) =
        let result, value = Double.TryParse(age)
        if result = false then None
        else
            if value < 10. 
            then Some("Kid") 
            else Some("Adult")

    let features = 
        [| (fun x -> x.Sex |> StringCategory);
           (fun x -> x.Class |> StringCategory);
           (fun x -> x.Age |> binnedAge);
           (fun x -> x.SiblingsOrSpouse |> StringCategory);
           (fun x -> x.ParentsOrChildren |> StringCategory);
           (fun x -> x.Embarked |> StringCategory); |]

    let minLeaf = 5 // min observations per leaf
    let bagging = 0.75 // proportion of sample used for estimation
    let iters = 50 // number of trees to grow
    let rng = Random(42) // random number generator
    let forest = createForestClassifier trainingSet features minLeaf bagging iters rng
            
    let correct = 
        trainingSet
        |> Array.averageBy (fun (label, obs) -> 
            if label = Some(forest obs) then 1. else 0.)
    printfn "Correct: %.4f" correct