open Charon.DecisionTree

[<EntryPoint>]
let main argv = 

    let runs = 10

    let rng = System.Random()
    let outcomes = 5
    let feat = 50
    let size = 100000

    let timer = System.Diagnostics.Stopwatch()

    [ 1 .. runs ]
    |> List.iter (fun run ->

            let initial = [ 0 .. size ]
            let features = [ 0 .. (feat - 1) ] |> Set.ofList

            timer.Restart()

            let data = [ for f in 0 .. feat -> [| for i in 0 .. size -> rng.Next(0, outcomes + 1) |] |> prepare ]
            let dataset = data |> List.mapi (fun i x -> i, x) |> Map.ofList
            
            timer.Stop()
            printfn "Data preparation: %i ms" timer.ElapsedMilliseconds  

            printfn "Initialized"

            timer.Restart()

            let tree = build dataset initial features feat

            timer.Stop()
            printfn "Tree building: %i ms" timer.ElapsedMilliseconds  

            printfn "Done!")

    printfn "%A" argv
    0 // return an integer exit code
