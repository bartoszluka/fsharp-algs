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

let rec mergeSortWith comparison =
    function
    | [] -> []
    | [ x ] -> [ x ]
    | list ->
        let len = List.length list
        let (listA, listB) = List.splitAt (len / 2) list
        mergeWith comparison (mergeSortWith comparison listA) (mergeSortWith comparison listB)


let mergeSort = mergeSortWith (<=)

let mergeSortDescendig = mergeSortWith (>=)

let mergeSortInt (list: int list) = mergeSort list

let mergeSortDescendigInt (list: int list) = mergeSortDescendig list

let partition predicate list =
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

let rec quickSortWith comparison =
    function
    | [] -> []
    | x :: xs ->
        let (bigger, smaller) = partition (comparison x) xs

        quickSortWith comparison smaller
        @ (x :: quickSortWith comparison bigger)


let quickSort = quickSortWith (<=)

let quickSortDescendig = quickSortWith (>=)

let quickSortInt (list: int list) = quickSort list

let quickSortDescendigInt (list: int list) = quickSortDescendig list

let mapPair f (x, y) = (f x, f y)

let splitWhen predicate list =
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

let flip f x y = f y x

let insertIntoSorted comparison item list =
    // let (smaller, bigger) = splitWhen ((<) item) list
    let (smaller, bigger) = splitWhen (comparison item) list
    //this is faster than this smaller @ [ item ] @ bigger
    smaller @ (item :: bigger)

let splitWhenPairwise compare list =
    if (List.isEmpty list) then
        ([], [])

    else
        let helper (a, b) = compare a b

        let firstIndex =
            list |> List.pairwise |> List.tryFindIndex helper

        match (firstIndex) with
        | None -> (list, [])
        | Some n -> List.splitAt n list

let swap (x, y) = (y, x)

let insertionSortWith comparison =
    let rec sortHelper (listIn, listOut) =
        match listIn with
        | [] -> ([], listOut)
        | x :: xs -> sortHelper (xs, insertIntoSorted comparison x listOut)

    splitWhenPairwise (flip comparison)
    >> swap
    >> sortHelper
    >> snd

let insertionSort = insertionSortWith (<=)

let insertionSortDescendig = insertionSortWith (>=)

let insertionSortInt (list: int list) = insertionSort list

let insertionSortDescendigInt (list: int list) = insertionSortDescendig list

let removeMin comparison =
    function
    | [] -> None
    | notEmpty ->
        let indexedList = List.indexed notEmpty

        let min =
            indexedList
            |> List.reduce
                (fun (xInd, xVal) (yInd, yVal) ->
                    if comparison xVal yVal then
                        (xInd, xVal)
                    else
                        (yInd, yVal))

        List.filter (fun x -> min <> x) indexedList
        |> List.map snd
        |> (fun lst -> ((snd min), lst))
        |> Some

let selectionSortWith comparison =
    let rec sortHelper (listIn, listOut) =
        match removeMin comparison listIn with
        | None -> ([], listOut)
        | Some (min, rest) -> sortHelper (rest, min :: listOut)

    let pairUp list = (list, [])
    pairUp >> sortHelper >> snd >> List.rev

let selectionSort = selectionSortWith (<=)

let selectionSortDescendig = selectionSortWith (>=)

let selectionSortInt (list: int list) = selectionSort list

let selectionSortDescendigInt (list: int list) = selectionSortDescendig list
