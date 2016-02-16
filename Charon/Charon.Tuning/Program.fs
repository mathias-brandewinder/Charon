open System
open Charon
open FSharp.Data

type Obs = { 
    Int: int option
    Float: float option
    String: string option
    RawFloat: float
    RawInt: int }

[<Literal>]
let samplePath = """../Charon.Examples/titanic.csv"""

type DataSet = CsvProvider<samplePath, 
                           Schema="PassengerId=int, Pclass->Class, Parch->ParentsOrChildren, SibSp->SiblingsOrSpouse", 
                           AssumeMissingValues=true, 
                           PreferOptionals=true>

type Passenger = DataSet.Row

let testingDemo () = 

    let data = [ 
        "A", { Int = Some(17); Float = Some(1.);  String = Some("One");   RawFloat = 42.0; RawInt = 1; };
        "B", { Int = Some(17); Float = Some(10.); String = None;          RawFloat = 32.0; RawInt = 0; };
        "A", { Int = Some(42); Float = Some(1.);  String = Some("Three"); RawFloat = 42.0; RawInt = 1; };
        "B", { Int = None;     Float = Some(10.); String = Some("One");   RawFloat = 22.0; RawInt = 0; };
        "",  { Int = Some(42); Float = Some(1.);  String = Some("One");   RawFloat = 22.0; RawInt = 0; };
        "B", { Int = Some(42); Float = Some(10.); String = Some("One");   RawFloat = 22.0; RawInt = 0; };
        "B", { Int = Some(42); Float = Some(10.); String = Some("One");   RawFloat = 22.0; RawInt = 0; };
        "A", { Int = Some(17); Float = Some(1.);  String = Some("Two");   RawFloat = 32.0; RawInt = 1; }; ]

    // Labels definition.
    let labels = "Label", (fun (txt:string) -> if txt = "" then None else Some(txt)) |> Categorical

    // Features definition, some Categorical (discrete states),
    // some Numerical (continuous values).
    let features = 
        [ "Integer", (fun o -> o.Int) |> Categorical; 
          "Float", (fun o -> o.Float) |> Numerical;
          "String", (fun o -> o.String) |> Categorical;
          "Raw Float", (fun o -> o.RawFloat |> Some) |> Numerical;
          "Raw Int", (fun o -> o.RawInt |> Some) |> Categorical; ]

    let tree = basicTree data (labels, features)
    tree

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
        let data = DataSet.GetSample ()
        [| for passenger in data.Rows -> 
            passenger, // label source
            passenger |] // features source
                
    let forest = forest training (labels,features) DefaultSettings

    printfn "FOREST"
    training |> Seq.take 25 |> Seq.iter (fun (l,x) -> printfn "%A -> %A" (l.Survived) (forest.Classifier x))
    let quality = training |> Seq.averageBy (fun (l,x) -> if (Option.get (l.Survived)).ToString() = (forest.Classifier x) then 1. else 0.)
    printfn "Forest quality: %f" quality


[<EntryPoint>]
let main argv = 
    printfn "Started"

    // TESTING DEBUG DEMO

//    let tree = testingDemo ()
    // TITANIC DEBUGGING / EXAMPLE

    let tree = titanicDemo ()

    0 // return an integer exit code