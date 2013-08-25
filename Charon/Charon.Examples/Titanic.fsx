#r @"..\Charon\bin\Debug\Charon.dll"
#r @"..\packages\FSharp.Data.1.1.9\lib\net40\FSharp.Data.dll"

open Charon
open Charon.DecisionTree
open System
open FSharp.Data

type DataSet = CsvProvider<"titanic.csv", 
                           Schema="PassengerId=int, Pclass->Class, Parch->ParentsOrChildren, SibSp->SiblingsOrSpouse", 
                           SafeMode=true, 
                           PreferOptionals=true>

type Passenger = DataSet.Row

let titanicDemo () =

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
