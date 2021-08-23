# F# algorithms

![build badge](https://github.com/bartoszluka/fsharp-algs/actions/workflows/build.yml/badge.svg)

## Introduction

This project is an implementation of some sorting algorithms in F#.
It came to be because I wanted to learn functional programming and F#.

Algorithms implemented:

- merge sort
- quick sort
- insertion sort
- selection sort

## Build

### Debug mode

To build the project in Debug mode run the following command:

`dotnet build`

after that you can test the project by runnning

`dotnet test Tests`

### Release mode

To build and test the project in Release mode run the following command:

`fake build`

## Benchmarks

The project was benchmarked using [BenchmarkDotNet](https://benchmarkdotnet.org/).
Results are stored in BenchmarkDotNet.Artifacts/results in the form of an markdown table.

To run the benchmarks yourself (this may take up to 10 minutes) use the following command:

`dotnet run --configuration Release --project Benchmarks/Benchmarks.fsproj`
