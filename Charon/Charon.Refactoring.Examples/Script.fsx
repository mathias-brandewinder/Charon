#r @"..\Charon.Refactoring\bin\Debug\Charon.Refactoring.dll"
#r @"..\packages\FSharp.Data.1.1.9\lib\net40\FSharp.Data.dll"

open Charon.Refactoring
open Charon.Refactoring.Tree
open Charon.Refactoring.Featurization
open Charon.Refactoring.Learning
open System
open FSharp.Data

type DataSet = CsvProvider<"""C:\Users\Mathias\Documents\GitHub\Charon\Charon\Charon.Examples\titanic.csv""", 
                           Schema="PassengerId=int, Pclass->Class, Parch->ParentsOrChildren, SibSp->SiblingsOrSpouse", 
                           SafeMode=true, 
                           PreferOptionals=true>

type Passenger = DataSet.Row

let titanicDemo () =

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

    let training = 
        use data = new DataSet()
        [| for passenger in data.Data -> 
            passenger, // label source
            passenger |] // features source
                
    let results = basicTree training (labels,features)

    printfn "Quality, training: %.3f" (results.TrainingQuality |> Option.get)
    printfn "Quality, holdout: %.3f" (results.HoldoutQuality |> Option.get)
    
    printfn "Tree:"
    printfn "%s" (results.Pretty)

    printfn "Forest"
    let forest = forest training (labels,features)

    // This is obviously weak-sauce, needs to be fixed.
    let quality = 
        training 
        |> Seq.averageBy (fun (l,x) -> 
            if (Option.get (l.Survived)).ToString() = (forest x) then 1. else 0.)
    printfn "Forest quality: %f" quality
