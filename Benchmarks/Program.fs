module Benchmarks

open System
open Sorts
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

let listSort = List.sort

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

let initList n =
    let rand = Random()

    [ 1 .. n ]
    |> List.map (fun _ -> rand.Next(10 * n))

[<MemoryDiagnoser>]
type SortComparison() =

    [<Params(10, 100, 1000)>]
    member val ListSize: int = 0 with get, set

    member self.MainList = initList self.ListSize

    [<Benchmark(Baseline = true)>]
    member self.ListSort() = listSort self.MainList

    [<Benchmark>]
    member self.ListQuickSort() = quickSort self.MainList

    [<Benchmark>]
    member self.ListMergeSort() = mergeSort self.MainList



[<EntryPoint>]
let main _ =
    BenchmarkRunner.Run typeof<SortComparison>
    |> ignore

    0
