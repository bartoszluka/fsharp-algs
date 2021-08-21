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

let partition predicate (list: int list) =
    let rec partHelper pred lst pair =
        match lst with
        | [] -> pair
        | x :: xs ->
            let (left, right) = pair

            if pred x then
                partHelper pred xs (x :: left, right)
            else
                partHelper pred xs (left, x :: right)

    partHelper predicate list ([], [])

let rec quickSort (list: int list) =
    match list with
    | [] -> []
    | x :: xs ->
        let (smaller, bigger) = partition ((>) x) xs

        quickSort smaller @ (x :: quickSort bigger)


// let bubbleSortMutable (l: Foo list) =
//     let mutable l' = l |> Array.ofList
//     let mutable keepGoing = true

//     while keepGoing do
//         keepGoing <- false

//         for i in [ 1 .. (Array.length l' - 1) ] do
//             if l'.[i - 1] > l'.[i] then
//                 let t = l'.[i - 1]
//                 l'.[i - 1] <- l'.[i]
//                 l'.[i] <- t
//                 keepGoing <- true

//     l' |> List.ofArray

// let bubbleSortRecursive (l: Foo list) =
//     let rec bubbleSort' a rev l =
//         match l, rev with
//         | [], true -> List.rev a
//         | [], false -> List.rev a |> bubbleSort' [] true
//         | h1 :: h2 :: t, _ ->
//             if h1 > h2 then
//                 bubbleSort' (h2 :: a) false (h1 :: t)
//             else
//                 bubbleSort' (h1 :: a) rev (h2 :: t)
//         | h :: t, _ -> bubbleSort' (h :: a) rev t

//     bubbleSort' [] true l
