// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

module SortingTests

open Sorts
open Xunit
open FsCheck.Xunit

let flip f x y = f y x

let apply2 f (x, y) = f x y

[<AbstractClass>]
type SortTests(sorting, sortingReverse) =

    [<Property>]
    member _.``contents are the same`` list =
        match sorting list with
        | [] -> true
        | notEmpty ->
            notEmpty
            |> List.map (flip List.contains list)
            |> List.reduce (&&)

    [<Property>]
    member _.``lengths are the same`` list =
        List.length list = List.length (sorting list)

    [<Property>]
    member _.``sorting negated numbers is reversed normal sorting`` list =
        let negate x = -x

        let sortLast = List.map negate >> sorting

        let sortFirst = sorting >> List.map negate >> List.rev

        sortLast list = sortFirst list

    [<Property>]
    member _.``every element is greater or equal to the previous`` list =
        let isSorted =
            sorting
            >> List.pairwise
            >> List.map (apply2 (<=))
            >> (List.reduce (&&))

        match list with
        | [] -> true
        | [ _ ] -> true
        | notEmpty -> isSorted notEmpty


    [<Property>]
    member _.``sort then reversing is the same as sorting in the other direction`` list =
        sortingReverse list = (sorting >> List.rev) list

    [<Fact>]
    member _.``empty list sorted is still an empty list``() = [] = sorting [] |> Assert.True

    [<Fact>]
    member _.``one element list sorted is still one element list``() = [ 1 ] = sorting [ 1 ] |> Assert.True


    [<Fact>]
    member _.``sorting three element list``() =
        [ 1; 2; 3 ] = sorting [ 1; 3; 2 ] |> Assert.True

type MergeSortTests() =
    inherit SortTests(mergeSort, mergeSortDescendig)

type QuickSortTests() =
    inherit SortTests(quickSort, quickSortDescendig)

type InsertionSortTests() =
    inherit SortTests(insertionSort, insertionSortDescendig)

type SelectionSortTests() =
    inherit SortTests(selectionSort, selectionSortDescendig)
