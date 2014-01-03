namespace Charon

type index = int []
type filter = int []

[<RequireQualifiedAccess>]
module Index =

    let length (a: index) = Array.length a

    let (empty: index) = [||]

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