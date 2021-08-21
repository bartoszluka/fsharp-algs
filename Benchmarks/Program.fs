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


[<AbstractClass>]
[<MemoryDiagnoser>]
type SortComparison() =

    [<Params(10, 100, 10000)>]
    member val ListSize: int = 0 with get, set

    abstract member Initializer : (int -> int list)

    member self.MainList = self.Initializer self.ListSize

    [<Benchmark(Baseline = true)>]
    member self.ListSort() = List.sort self.MainList

    [<Benchmark>]
    member self.ListQuickSort() = quickSort self.MainList

    [<Benchmark>]
    member self.ListMergeSort() = mergeSort self.MainList



type SortComparisonRandom() =
    inherit SortComparison()

    override _.Initializer = initRandomList


type SortComparisonReversed() =
    inherit SortComparison()

    override _.Initializer = initReversedList

type SortComparisonSorted() =
    inherit SortComparison()

    override _.Initializer = initSortedList


[<EntryPoint>]
let main _ =
    BenchmarkRunner.Run typeof<SortComparisonRandom>
    |> ignore

    BenchmarkRunner.Run typeof<SortComparisonSorted>
    |> ignore

    BenchmarkRunner.Run typeof<SortComparisonReversed>
    |> ignore

    0