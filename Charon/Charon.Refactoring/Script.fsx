#load "Featurization.fs"
#load "Index.fs"
#load "Tree.fs"
#load "Learning.fs"

open Charon.Refactoring
open Charon.Refactoring.Featurization
open Charon.Refactoring.Learning

type Obs = 
    { Int: int option; 
      Float: float option; 
      String: string option;
      RawFloat: float;
      RawInt: int }

let fs = 
    [ "Integer", (fun o -> o.Int) |> Categorical; 
      "Float", (fun o -> o.Float) |> Numerical;
      "String", (fun o -> o.String) |> Categorical;
      "Raw Float", (fun o -> o.RawFloat |> Some) |> Numerical;
      "Raw Int", (fun o -> o.RawInt |> Some) |> Categorical; ]

let data = [ 
    "A", { Int = Some(17); Float = Some(1.); String = Some("One"); RawFloat = 42.0; RawInt = 1; };
    "B", { Int = Some(17); Float = Some(1.); String = Some("Two"); RawFloat = 32.0; RawInt = 0; };
    "A", { Int = Some(42); Float = Some(2.); String = Some("Three"); RawFloat = 42.0; RawInt = 1; };
    "B", { Int = Some(42); Float = Some(3.); String = Some("One"); RawFloat = 22.0; RawInt = 0; };
    "A", { Int = Some(17); Float = Some(2.); String = Some("Two"); RawFloat = 32.0; RawInt = 1; }; ]

let labels = Discrete, fun (x:Obs) -> (x.RawInt |> Some |> Int)

let featurizers = [ 
    Discrete, (fun (x:Obs) -> x.Int |> Int); 
    Discrete, (fun (x:Obs) -> x.RawInt |> Some |> Int); 
    Continuous, (fun (x:Obs) -> x.Float |> Float); ]

let labelizer =
    Discrete, (fun txt -> if txt = "A" then Int(Some(0)) else Int(Some(1)))

let test = prepare data labelizer featurizers

let map,extractor = createExtractor (data |> Seq.map snd) fs
data |> List.map snd |> List.map extractor