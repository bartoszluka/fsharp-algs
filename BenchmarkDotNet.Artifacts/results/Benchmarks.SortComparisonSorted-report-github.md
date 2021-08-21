```ini

BenchmarkDotNet=v0.13.1, OS=Windows 10.0.19042.1165 (20H2/October2020Update)
Intel Core i5-10400 CPU 2.90GHz, 1 CPU, 12 logical and 6 physical cores
.NET SDK=5.0.400
  [Host]     : .NET 5.0.9 (5.0.921.35908), X64 RyuJIT DEBUG
  DefaultJob : .NET 5.0.9 (5.0.921.35908), X64 RyuJIT


```

| Method        | ListSize |          Mean |         Error |        StdDev |    Ratio |  RatioSD |       Gen 0 |      Gen 1 |  Allocated |
| ------------- | -------- | ------------: | ------------: | ------------: | -------: | -------: | ----------: | ---------: | ---------: |
| **ListSort**  | **100**  |  **6.887 μs** | **0.0376 μs** | **0.0333 μs** | **1.00** | **0.00** |  **1.7090** | **0.0229** |  **11 KB** |
| ListQuickSort | 100      |     79.468 μs |     1.5498 μs |     1.7847 μs |    11.56 |     0.31 |     63.9648 |     1.2207 |     392 KB |
| ListMergeSort | 100      |     13.945 μs |     0.2389 μs |     0.2235 μs |     2.03 |     0.03 |      4.8676 |     0.1221 |      30 KB |
|               |          |               |               |               |          |          |             |            |            |
| **ListSort**  | **1000** | **67.513 μs** | **0.3793 μs** | **0.3167 μs** | **1.00** | **0.00** | **16.6016** | **2.5635** | **102 KB** |
| ListQuickSort | 1000     |  8,117.815 μs |    47.4915 μs |    39.6575 μs |   120.24 |     0.74 |   6234.3750 |   546.8750 |  38,210 KB |
| ListMergeSort | 1000     |    177.581 μs |     1.2519 μs |     1.1098 μs |     2.63 |     0.03 |     66.4063 |    12.6953 |     407 KB |