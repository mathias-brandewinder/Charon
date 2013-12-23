namespace Charon.Refactoring

type index = int []
type filter = int []

[<RequireQualifiedAccess>]
module Index =

    let length (a: index) = Array.length a

    let (empty: index) = [||]

    let intersect (ar1:int[]) (ar2:int[]) = 
        Seq.unfold (fun (i,j) -> 
            if (i >= Array.length ar1) then None 
            elif (j >= Array.length ar2) then None
            else
                if ar1.[i] = ar2.[j] then Some(Some(ar1.[i]), (i+1, j+1))
                elif ar1.[i] < ar2.[j] then Some(None, (i+1,j))
                else Some(None, (i,j+1))) (0,0)
        |> Seq.choose id
        |> Seq.toArray
          
    let merge (ar1:int[]) (ar2:int[]) =
        let l1 = Array.length ar1
        let l2 = Array.length ar2
        Seq.unfold (fun (i,j) -> 
            if (i >= l1) then
                if (j >= l2) then None 
                else Some(ar2.[j], (i, j + 1)) // can probably speed that part up
            elif (j >= l2) then Some(ar1.[i], (i+1, j))
            else
                if ar1.[i] < ar2.[j] then Some(ar1.[i], (i+1, j))
                else Some(ar2.[j], (i, j+1))) (0,0)
        |> Seq.toArray