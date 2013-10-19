namespace Charon

type index = int list

[<RequireQualifiedAccess>]
module Index =

    let length (a: index) = List.length a

    let (empty: index) = []

    let rec private inter (curr: index) (a: index) (b: index) =
        match a with
        | [] -> curr
        | hda::tla ->
            match b with
            | [] -> curr
            | hdb::tlb ->
                if hda = hdb then inter (hda::curr) tla tlb
                elif hda < hdb then inter curr tla b
                else inter curr a tlb

    // Computes the intersection of 2 indices
    let intersect (a: index) (b: index) =
        inter [] a b |> List.rev

    let rec private mer (curr: index) (a: index) (b: index) =
        match a with
        | [] -> 
            match b with
            | [] -> curr
            | hd::tl -> mer (hd::curr) a tl
        | hda::tla ->
              match b with
              | [] -> mer (hda::curr) tla b
              | hdb::tlb ->
                    if hda < hdb then mer (hda::curr) tla b
                    else mer (hdb::curr) a tlb
    
    // Merges 2 indices 
    let merge (a: index) (b: index) =
        mer [] a b |> List.rev

    let rec private oob acc rest curr (lo, hi) =
        match rest with
        | [] ->
            if curr <= hi 
            then oob (curr::acc) rest (curr + 1) (lo, hi)
            else acc
        | hd::tl ->
            if curr < hd
            then oob (curr::acc) rest (curr + 1) (lo, hi)
            else oob acc tl (curr + 1) (lo, hi)

    let complement (lo, hi) (i: index) =
        oob [] i lo (lo, hi) |> List.rev