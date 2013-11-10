namespace Charon

module Entropy =

    type Count = int []

    let add (count1:Count) count2 = (count1, count2) ||> Array.map2 (fun x y -> x + y)
    let sub (count1:Count) count2 = (count1, count2) ||> Array.map2 (fun x y -> x - y)
    let sum (count:Count) = count |> Array.sum

    // Entropy of 
    let h (data:Count) =
        let total = sum data |> float
        data 
        |> Seq.sumBy (fun x -> 
            let p = float x / total
            if p > 0. then - p * log p else 0.)

    // Entropy gained by splitting into left/right
    let gain ((left:Count), (right:Count)) =
        let total = add left right
        let leftS = sum left |> float 
        let rightS = sum right |> float
        let totalS = leftS + rightS
        h total - h left * (leftS/totalS) - h right * (rightS/totalS)

/// Minimum Description Length partitioning
module MDL =
    
    open Entropy

    // Log in base b
    let private logb n b = log n / log b
    
    // Count classes containing elements
    let private cla (count:Count) = count |> Seq.reduce (fun acc x -> if x > 0 then acc + 1 else acc)

    // Minimum entropy gain required to accept split 
    let minG ((left:Count), (right:Count)) =

        let total = add left right
        let size = sum total |> float
        let classes = cla total

        let totalc = classes |> float
        let leftc = cla left |> float
        let rightc = cla right |> float
    
        let h = Entropy.h total
        let lefth = Entropy.h left
        let righth = Entropy.h right

        let delta =     
            logb (pown 3. classes - 2.) 2. - 
            (totalc * h - leftc * lefth - rightc * righth)
        ((logb (size - 1.) 2.) / size) + (delta / size)

    // Value of splitting by left / right
    let splitValue left right =
        let v = gain (left,right) - minG (left, right)
        if v > 0. then Some(v) else None