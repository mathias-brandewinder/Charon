(*
Example using decision tree and the CSV type provider,
based on the Kaggle "Titanic: Machine Learning from Disaster" dataset:
http://www.kaggle.com/c/titanic-gettingStarted
*)

#r @"..\Charon\bin\Debug\Charon.dll"
#r @"..\packages\FSharp.Data.1.1.9\lib\net40\FSharp.Data.dll"

open Charon
open Charon.DecisionTree
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
type DataSet = CsvProvider<"titanic.csv", 
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
            passenger.Survived |> Categorical, // the label
            passenger |]
    // We define what features should be used:
    let features = [|
        "Sex", (fun (x:Passenger) -> x.Sex |> Categorical);
        "Class", (fun x -> x.Class |> Categorical); |]
    // We run the classifier...
    let classifier, report = createID3Classifier training features { DefaultID3Config with DetailLevel = Verbose }
    // ... and display the resulting tree:
    report.Value.Pretty()