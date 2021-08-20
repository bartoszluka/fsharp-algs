module Sorts

let rec mergeWith compare listA listB : 'a list =
    match (listA, listB) with
    | ([], _) -> listB
    | (_, []) -> listA
    | (x :: xs, y :: ys) ->
        if compare x y then
            x :: (mergeWith compare xs listB)
        else
            y :: (mergeWith compare listA ys)

let merge = mergeWith (<)

let rec mergeSort (list: int list) =
    match List.length list with
    | len when len <= 1 -> list
    | len ->
        let lists = List.splitAt (len / 2) list
        let listA = fst lists
        let listB = snd lists
        merge (mergeSort listA) (mergeSort listB)

let rec quickSort (list: int list) =
    match list with
    | [] -> []
    | x :: xs ->
        let smaller = List.filter ((>) x) xs
        let bigger = List.filter ((<=) x) xs

        // quickSort smaller @ [ x ] @ quickSort bigger
        quickSort smaller @ (x :: quickSort bigger)
