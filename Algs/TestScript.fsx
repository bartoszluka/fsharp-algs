let negateIsReversed sorting list =
    let negate x = -x

    let sortLast = list |> List.map negate |> sorting

    let sortFirst =
        list |> sorting |> List.map negate |> List.rev

    printfn "%b" (sortLast = sortFirst)


let rec quickSort (list: int list) =
    match list with
    | [] -> []
    | x :: xs ->
        let smaller = List.filter ((>) x) xs
        let bigger = List.filter ((<=) x) xs

        quickSort smaller @ (x :: quickSort bigger)


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

let apply2 f (x, y) = f x y



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

let insertionSort (list: int list) =
    let rec sortHelper (listIn, listOut) =
        match listIn with
        | [] -> ([], listOut)
        | x :: xs -> sortHelper (xs, insertIntoSorted x listOut)


    sortHelper (list, []) |> snd

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

// [ 382; 100; 101; 21; 37; 1; -123 ]
[ 1; 2; 3; 4; 5; 9; 7; 8; 9; 10; 11 ]
|> splitWhenPairwise (>=)
// |> insertIntoSorted -1
// // |> splitWhen (fun x -> x >= 0)
|> mapPair (printfn "%A")
