(*
Example using decision tree and the CSV type provider,
based on the Kaggle "Titanic: Machine Learning from Disaster" dataset:
http://www.kaggle.com/c/titanic-gettingStarted
*)

#r @"..\Charon\bin\Debug\Charon.dll"
#r @"..\packages\FSharp.Data.1.1.9\lib\net40\FSharp.Data.dll"

open Charon
open Charon.Tree
open Charon.Featurization
open Charon.Learning
open System
open FSharp.Data

(*
We use the CSV type provider from FSharp.Data 
to extract passenger information,
renaming properties for convenience:
- Pclass: Class, 
- Parch: ParentsOrChildren, 
- SibSp: SiblingsOrSpouse
We force inference to treat every feature as optional, 
assuming that any feature could have missing values.
*)

type DataSet = CsvProvider<"""C:\Users\Mathias\Documents\GitHub\Charon\Charon\Charon.Examples\titanic.csv""", 
                           Schema="PassengerId=int, Pclass->Class, Parch->ParentsOrChildren, SibSp->SiblingsOrSpouse", 
                           SafeMode=true, 
                           PreferOptionals=true>

type Passenger = DataSet.Row

let titanicDemo () =

    // We read the training set into an array,
    // defining the Label we want to classify on:
    let training = 
        use data = new DataSet()
        [| for passenger in data.Data -> 
            passenger, // label source
            passenger |] // features source

    // We define the label, and what features should be used:
    let labels = "Survived", (fun (obs:Passenger) -> obs.Survived) |> Categorical

    let features = 
        [ 
          "Sex", (fun (o:Passenger) -> o.Sex) |> Categorical;
          "Class", (fun (o:Passenger) -> o.Class) |> Categorical;
          "Age", (fun (o:Passenger) -> o.Age) |> Numerical;
          "Fare", (fun (o:Passenger) -> o.Fare) |> Numerical;
          "Embarked", (fun (o:Passenger) -> o.Embarked) |> Categorical;
          "Parents",  (fun (o:Passenger) -> o.ParentsOrChildren) |> Categorical;
          "Spouse",  (fun (o:Passenger) -> o.SiblingsOrSpouse) |> Categorical;
        ]
                
    // We run a basic tree classifier...
    let results = basicTree training (labels,features)

    // ... compute the quality on the training set and validation set...
    printfn "Quality, training: %.3f" (results.TrainingQuality |> Option.get)
    printfn "Quality, holdout: %.3f" (results.HoldoutQuality |> Option.get)
    
    // ... and display the resulting tree:    
    printfn "Tree:"
    printfn "%s" (results.Pretty)

    // We run now a random forest
    printfn "Forest"

    // ... and display the out-of-bag classification quality:
    let forestResults = forest training (labels,features)
    printfn "OOB quality: %f" forestResults.OutOfBagQuality

    // For comparison: quality over entire dataset.
    let quality = 
        training 
        |> Seq.averageBy (fun (l,x) -> 
            if (Option.get (l.Survived)).ToString() = (forestResults.Classifier x) then 1. else 0.)
    printfn "Forest quality: %f" quality