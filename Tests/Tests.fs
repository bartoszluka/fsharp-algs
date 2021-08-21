// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

module SortingTests

open Sorts
open FsCheck.Xunit

let flip f x y = f y x

let apply2 f (x, y) = f x y

let isSorted sorting list =
    let actualFunc =
        sorting
        >> List.pairwise
        >> List.map (apply2 (<=))
        >> (List.reduce (&&))

    match list with
    | [] -> true
    | [ _ ] -> true
    | notEmpty -> actualFunc notEmpty

let areTheSameLength sorting list =
    List.length list = List.length (sorting list)

let contentsAreTheSame sorting list =
    match sorting list with
    | [] -> true
    | notEmpty ->
        notEmpty
        |> List.map (flip List.contains list)
        |> List.reduce (&&)

let negateIsReversed sorting list =
    let negate x = -x

    let sortLast = List.map negate >> sorting

    let sortFirst = sorting >> List.map negate >> List.rev

    sortLast list = sortFirst list

// merge sort tests
[<Property>]
let ``merge sort contents are the same`` () = contentsAreTheSame mergeSort

[<Property>]
let ``merge sort lengths are the same`` () = areTheSameLength mergeSort

[<Property>]
let ``merge sort negate is the same`` () = negateIsReversed mergeSort

[<Property>]
let ``merge sort is sorted`` () = isSorted mergeSort


// quick sort tests
[<Property>]
let ``quick sort contents are the same`` () = contentsAreTheSame quickSort

[<Property>]
let ``quick sort lengths are the same`` () = areTheSameLength quickSort

[<Property>]
let ``quick sort negate is the same`` () = negateIsReversed quickSort

[<Property>]
let ``quick sort is sorted`` () = isSorted quickSort

// insertion sort tests
[<Property>]
let ``insertion sort contents are the same`` () = contentsAreTheSame insertionSort

[<Property>]
let ``insertion sort lengths are the same`` () = areTheSameLength insertionSort

[<Property>]
let ``insertion sort negate is the same`` () = negateIsReversed insertionSort

[<Property>]
let ``insertion sort is sorted`` () = isSorted insertionSort

// selection sort tests
[<Property>]
let ``selection sort contents are the same`` () = contentsAreTheSame selectionSort

[<Property>]
let ``selection sort lengths are the same`` () = areTheSameLength selectionSort

[<Property>]
let ``selection sort negate is the same`` () = negateIsReversed selectionSort

[<Property>]
let ``selection sort is sorted`` () = isSorted selectionSort
