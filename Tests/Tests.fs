// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

module SortingTests

open Sorts
open FsCheck.Xunit

let areTheSameLength sorting list =
    List.length list = List.length (sorting list)

let flip f x y = f y x

let contentsAreTheSame sorting list =
    match sorting list with
    | [] -> true
    | notEmpty ->
        notEmpty
        |> List.map (flip List.contains list)
        |> List.reduce (&&)

let negateIsReversed sorting list =
    let negate x = -x

    let sortLast = list |> List.map negate |> sorting

    let sortFirst =
        list |> sorting |> List.map negate |> List.rev

    sortLast = sortFirst

[<Property>]
let ``merge sort contents are the same`` () = contentsAreTheSame mergeSort


[<Property>]
let ``merge sort lengths are the same`` () = areTheSameLength mergeSort

[<Property>]
let ``merge sort negate is the same`` () = negateIsReversed mergeSort

[<Property>]
let ``quick sort contents are the same`` () = contentsAreTheSame quickSort

[<Property>]
let ``quick sort lengths are the same`` () = areTheSameLength quickSort

[<Property>]
let ``quick sort negate is the same`` () = negateIsReversed quickSort
