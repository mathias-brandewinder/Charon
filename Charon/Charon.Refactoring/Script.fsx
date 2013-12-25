// Experimental script to develop / play with the API.

#load "Featurization.fs"
#load "Index.fs"
#load "Tree.fs"
#load "Entropy.fs"
#load "Learning.fs"

open Charon.Refactoring
open Charon.Refactoring.Featurization
open Charon.Refactoring.Learning

// Our observations, with various types.
type Obs = 
    { Int: int option; 
      Float: float option; 
      String: string option;
      RawFloat: float;
      RawInt: int }

// The training set, a sequence of label, observation,
// with some missing values added for good measure.
let data = [ 
    "A", { Int = Some(17); Float = None;     String = Some("One");   RawFloat = 42.0; RawInt = 1; };
    "B", { Int = Some(17); Float = Some(1.); String = None;          RawFloat = 32.0; RawInt = 0; };
    "A", { Int = Some(42); Float = Some(2.); String = Some("Three"); RawFloat = 42.0; RawInt = 1; };
    "B", { Int = None;     Float = Some(3.); String = Some("One");   RawFloat = 22.0; RawInt = 0; };
    "",  { Int = Some(42); Float = Some(3.); String = Some("One");   RawFloat = 22.0; RawInt = 0; };
    "B", { Int = Some(42); Float = Some(3.); String = Some("One");   RawFloat = 22.0; RawInt = 0; };
    "B", { Int = Some(42); Float = Some(3.); String = Some("One");   RawFloat = 22.0; RawInt = 0; };
    "A", { Int = Some(17); Float = Some(2.); String = Some("Two");   RawFloat = 32.0; RawInt = 1; }; ]

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

// Create an "extractor", mapping original observations
// to the features used in the tree.
let map,extractor = createExtractor (data |> Seq.map snd) features
data |> List.map snd |> List.map extractor

// Prepare the Training Set into proper features.
let transformers = translators data (labels,features)
let trainingset = prepare data transformers

let tree = train trainingset [|0..6|] ([0..4] |> Set.ofList) { MinLeaf = 1 }