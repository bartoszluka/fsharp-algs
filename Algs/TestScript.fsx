let mapPair f (x, y) = (f x, f y)

let splitWhen predicate list =
    let rec splitHelper pred flag lst (left, right) =
        match lst with
        | [] -> (left, right)
        | x :: xs ->
            if flag || pred x then
                splitHelper pred true xs (left, x :: right)
            else
                splitHelper pred (pred x) xs (x :: left, right)

    splitHelper predicate false list ([], [])
    |> mapPair List.rev

let flip f x y = f y x

let insertIntoSorted comparison item list =
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

let test =
    [ 382
      100
      101
      21
      37
      123
      530
      92
      -12
      12
      234 ]
// let test = [ 4; 1; 2; 1 ]
// let test = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 ]

#load "./ListZipper.fs"
open ListZipper

test |> shakerSortWith (<) |> printfn "%A"
