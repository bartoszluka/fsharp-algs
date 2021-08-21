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

let mapPair f (x, y) = (f x, f y)

let splitWhen predicate (list: int list) =
    let rec splitHelper pred flag lst (left, right) =
        match lst with
        | [] -> (left, right)
        | x :: xs ->
            // match flag, pred x with
            // | true, _ -> splitHelper pred true xs (left, x :: right)
            // | false, true -> splitHelper pred true xs (left, x :: right)
            // | false, false -> splitHelper pred (pred x) xs (x :: left, right)
            if flag || pred x then
                splitHelper pred true xs (left, x :: right)
            else
                splitHelper pred (pred x) xs (x :: left, right)

    splitHelper predicate false list ([], [])
    |> mapPair List.rev


//possibly do binary insert
let insertIntoSorted item (list: int list) =
    // let (smaller, bigger) = splitWhen ((<) item) list
    let (smaller, bigger) = splitWhen (fun x -> x >= item) list
    smaller @ (item :: bigger)

let splitWhenPairwise (f: 'a -> 'a -> bool) (list: list<'a>) =
    if (List.isEmpty list) then
        ([], [])

    else
        let helper (a, b) = f a b

        let firstIndex =
            list |> List.pairwise |> List.tryFindIndex helper

        match (firstIndex) with
        | None -> (list, [])
        | Some n -> List.splitAt n list

let swap (x, y) = (y, x)

let insertionSort =
    let rec sortHelper (listIn, listOut) =
        match listIn with
        | [] -> ([], listOut)
        | x :: xs -> sortHelper (xs, insertIntoSorted x listOut)

    splitWhenPairwise (>=)
    >> swap
    >> sortHelper
    >> snd

let findMin comparison list = List.reduce comparison

let flip f x y = f y x

let removeMin list =
    match list with
    | [] -> None
    | notEmpty ->
        let indexedList = List.indexed notEmpty

        let min =
            indexedList
            |> List.reduce (fun (xi, x) (yi, y) -> if x < y then (xi, x) else (yi, y))

        List.filter (fun x -> min <> x) indexedList
        |> List.map snd
        |> (fun l -> ((snd min), l))
        |> Some


let selectionSort (list: int list) =
    let rec sortHelper (listIn, listOut) =
        match removeMin listIn with
        | None -> ([], listOut)
        | Some (min, rest) -> sortHelper (rest, min :: listOut)

    sortHelper (list, []) |> snd |> List.rev
// list
// |> splitWhenPairwise (>=)
// |> sortHelper
// |> snd

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
