// Experimental script to develop / play with the API.

#load "Featurization.fs"
#load "Index.fs"
#load "Tree.fs"
#load "Entropy.fs"
#load "Continuous.fs"
#load "Learning.fs"

open Charon
open Charon.Featurization
open Charon.Learning
open Charon.Tree

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

// Create an "extractor", mapping original observations
// to the features used in the tree.
//let map,extractor = createExtractor (data |> Seq.map snd) features
//data |> List.map snd |> List.map extractor
//
//// Prepare the Training Set into proper features.
let transformers = translators data (labels,features)
let trainingset = prepare data transformers

let tree = train trainingset [|0..6|] ([0..4] |> Set.ofList) { MinLeaf = 1; Holdout = 0.1 }


let t = basicTree data (labels, features)

let a = [| 0; 5; 7; 10; |]
let b = [| 1; 3; 5; 7; 9 |]

let intersect (x:int[]) (y:int[]) =
    let (i,j) = (0,0)
    let lx,ly = x.Length, y.Length
    let rec search (a,b) =
        if a < lx && b < ly
        then 
            if x.[a] = y.[b] then Some(x.[a],(a+1,b+1))
            elif x.[a] < y.[b] then search (a+1,b)
            else search (a,b+1)
        else None
    Seq.unfold (fun (a,b) -> search (a,b)) (0,0) 
    |> Seq.toArray

let c = [| 0 .. 2 .. 100000 |]
let d = [| 0 .. 3 .. 100000 |]

