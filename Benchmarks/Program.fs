module Benchmarks

open System
open Sorts
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running


let initRandomList n =
    let rand = Random()

    List.init n (fun _ -> rand.Next(10 * n))

let initSortedList n = initRandomList n |> List.sort

let initReversedList n = initRandomList n |> List.sortDescending


[<AbstractClass; MemoryDiagnoser; MarkdownExporter>]
type SortComparison(initializer) =

    [<Params(20, 100, 1000)>]
    member val ListSize: int = 0 with get, set

    member self.MainList = initializer self.ListSize

    [<Benchmark(Baseline = true)>]
    member self.ListSort() = List.sort self.MainList

    [<Benchmark>]
    member self.ListQuickSort() = quickSort self.MainList

    [<Benchmark>]
    member self.ListMergeSort() = mergeSort self.MainList

    [<Benchmark>]
    member self.ListInsertionSort() = insertionSort self.MainList

    [<Benchmark>]
    member self.ListSelectionSort() = selectionSort self.MainList

    [<Benchmark>]
    member self.ListShakerSort() = ListZipper.shakerSort self.MainList


type SortComparisonRandom() =
    inherit SortComparison(initRandomList)

type SortComparisonReversed() =
    inherit SortComparison(initReversedList)

type SortComparisonSorted() =
    inherit SortComparison(initSortedList)


[<EntryPoint>]
let main _ =
    BenchmarkRunner.Run typeof<SortComparisonRandom>
    |> ignore

    BenchmarkRunner.Run typeof<SortComparisonSorted>
    |> ignore

    BenchmarkRunner.Run typeof<SortComparisonReversed>
    |> ignore

    0
