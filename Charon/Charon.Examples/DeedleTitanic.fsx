(*
Example using decision tree and the CSV type provider,
based on the Kaggle "Titanic: Machine Learning from Disaster" dataset:
http://www.kaggle.com/c/titanic-gettingStarted
*)

#r @"..\Charon\bin\Debug\Charon.dll"
#r @"..\packages\FSharp.Data.2.0.9\lib\net40\FSharp.Data.dll"
#load @"..\packages\Deedle.1.0.1\Deedle.fsx"

open Charon
open System
open Deedle

// ----------------------------------------------------------------------------
// Deedle integration for charon
// ----------------------------------------------------------------------------

let extractFrameFeatures labelName (frame:Frame<_, _>) = 

  let makeFeature colTyp colKey = 
    let isNumerical = colTyp = typeof<float> || colTyp = typeof<decimal> 
    if isNumerical then
      Numerical(fun (row:ObjectSeries<string>) -> 
        row.TryGet(colKey) |> OptionalValue.asOption)
    else
      Categorical(fun (row:ObjectSeries<string>) -> 
        row.TryGet(colKey) |> OptionalValue.asOption)

  let input = [ for r in frame.Rows.Values -> r, r ]
  let labels = labelName, Categorical(fun (row:ObjectSeries<string>) -> 
    row.TryGet(labelName) |> OptionalValue.asOption)
  let frameData = frame.GetFrameData()
  let features = 
    [ for colKey, (colTyp, _) in Seq.zip frame.ColumnKeys frameData.Columns do
        if colKey <> labelName then
          yield colKey, makeFeature colTyp colKey  ]

  input, (labels, features)

let frameTree labelName frame = 
  let input, (labels, features) = extractFrameFeatures labelName frame
  basicTree input (labels, features) DefaultSettings

let frameForest labelName frame = 
  let input, (labels, features) = extractFrameFeatures labelName frame
  forest input (labels, features) DefaultSettings

// ----------------------------------------------------------------------------
// Load frame & drop columns we don't want
// ----------------------------------------------------------------------------

let titanic = 
  Frame.ReadCsv(__SOURCE_DIRECTORY__ + "/titanic.csv")
  |> Frame.sliceCols 
      [ "Survived"; "Pclass"; "Sex"; "Age"; 
        "SibSp"; "Parch"; "Fare"; "Embarked" ]

// ----------------------------------------------------------------------------
// Basic tree
// ----------------------------------------------------------------------------

let results = titanic |> frameTree "Survived"

// ... compute the quality on the training set and validation set...
printfn "Quality, training: %.3f" (results.TrainingQuality |> Option.get)
printfn "Quality, holdout: %.3f" (results.HoldoutQuality |> Option.get)
    
// ... and display the resulting tree:    
printfn "Tree:"
printfn "%s" (results.Pretty)

// ----------------------------------------------------------------------------
// Forest
// ----------------------------------------------------------------------------

// ... and display the out-of-bag classification quality:
let forestResults = titanic |> frameForest "Survived"
printfn "OOB quality: %f" forestResults.OutOfBagQuality

// For comparison: quality over entire dataset.
let quality = 
  titanic.Rows
  |> Series.mapValues (fun row ->
      if row.Get("Survived").ToString() = forestResults.Classifier row 
        then 1.0 else 0.0)
  |> Stats.mean

printfn "Forest quality: %f" quality
