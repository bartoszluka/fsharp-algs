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

let partition predicate =
    let rec partHelper pred (left, right) =
        function
        | [] -> (left, right)
        | x :: xs ->
            let concat =
                if pred x then
                    (x :: left, right)
                else
                    (left, x :: right)

            partHelper pred concat xs

    partHelper predicate ([], [])

let rec quickSortWith comparison =
    function
    | [] -> []
    | x :: xs ->
        let (bigger, smaller) = partition (comparison x) xs

        List.concat [ quickSortWith comparison smaller
                      [ x ]
                      quickSortWith comparison bigger ]


let quickSort = quickSortWith (<=)

let quickSortDescendig = quickSortWith (>=)

let quickSortInt (list: int list) = quickSort list

let quickSortDescendigInt (list: int list) = quickSortDescendig list

let mapPair f (x, y) = (f x, f y)

let unPair f (x, y) = f x y

let splitWhen predicate =
    let rec splitHelper pred flag (left, right) =
        function
        | [] -> (left, right)
        | x :: xs ->
            if flag || pred x then
                splitHelper pred true (left, x :: right) xs
            else
                splitHelper pred (pred x) (x :: left, right) xs

    splitHelper predicate false ([], [])
    >> mapPair List.rev

let flip f x y = f y x

let insertIntoSorted comparison item list =
    let (smaller, bigger) = splitWhen (comparison item) list

    List.concat [ smaller
                  [ item ]
                  bigger ]

let safePairwise =
    function
    | [] -> []
    | [ _ ] -> []
    | list -> List.pairwise list

let splitWhenPairwise compare list =
    let firstIndex =
        list
        |> safePairwise
        |> List.tryFindIndex (unPair compare)

    match firstIndex with
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
