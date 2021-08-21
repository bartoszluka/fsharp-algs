```ini

BenchmarkDotNet=v0.13.1, OS=Windows 10.0.19042.1165 (20H2/October2020Update)
Intel Core i5-10400 CPU 2.90GHz, 1 CPU, 12 logical and 6 physical cores
.NET SDK=5.0.400
  [Host]     : .NET 5.0.9 (5.0.921.35908), X64 RyuJIT DEBUG
  DefaultJob : .NET 5.0.9 (5.0.921.35908), X64 RyuJIT


```

| Method        | ListSize |           Mean |         Error |        StdDev |    Ratio |  RatioSD |       Gen 0 |      Gen 1 |  Allocated |
| ------------- | -------- | -------------: | ------------: | ------------: | -------: | -------: | ----------: | ---------: | ---------: |
| **ListSort**  | **100**  |   **9.605 μs** | **0.1011 μs** | **0.0845 μs** | **1.00** | **0.00** |  **1.9073** | **0.0305** |  **12 KB** |
| ListQuickSort | 100      |      79.383 μs |     1.3062 μs |     1.0907 μs |     8.27 |     0.15 |     64.3311 |     1.2207 |     395 KB |
| ListMergeSort | 100      |      16.534 μs |     0.1346 μs |     0.1259 μs |     1.72 |     0.01 |      5.2490 |     0.0916 |      32 KB |
|               |          |                |               |               |          |          |             |            |            |
| **ListSort**  | **1000** | **102.260 μs** | **0.8312 μs** | **0.7775 μs** | **1.00** | **0.00** | **18.5547** | **2.8076** | **114 KB** |
| ListQuickSort | 1000     |   8,234.841 μs |   131.2342 μs |   156.2250 μs |    80.62 |     2.08 |   6234.3750 |   546.8750 |  38,237 KB |
| ListMergeSort | 1000     |     211.580 μs |     2.6869 μs |     2.5133 μs |     2.07 |     0.04 |     68.6035 |    13.4277 |     422 KB |