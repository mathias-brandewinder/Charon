#load "Featurization.fs"
open Charon.Refactoring
open Charon.Refactoring.Featurization

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

let fsMap =
    [ Map.ofSeq [ "42", 0; "17", 1; "-50", 2; ];
      Map.empty;
      Map.ofSeq [ "One", 0; "Two", 1; ];
      Map.empty;
      Map.ofSeq [ "42", 0; "12", 1; "34", 2; ] ]

let sample = 
    { Int = Some(17); 
      Float = Some(123.45); 
      String = Some("One");
      RawFloat = 42.0;
      RawInt = 42; }

let emptySample = 
    { Int = None; 
      Float = None; 
      String = None;
      RawFloat = 42.0;
      RawInt = 42; }

let f = extractor fs fsMap
f sample
f emptySample