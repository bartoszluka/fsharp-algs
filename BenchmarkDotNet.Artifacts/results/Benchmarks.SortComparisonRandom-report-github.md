``` ini

BenchmarkDotNet=v0.13.1, OS=Windows 10.0.19042.1165 (20H2/October2020Update)
Intel Core i5-10400 CPU 2.90GHz, 1 CPU, 12 logical and 6 physical cores
.NET SDK=5.0.400
  [Host]     : .NET 5.0.9 (5.0.921.35908), X64 RyuJIT DEBUG
  DefaultJob : .NET 5.0.9 (5.0.921.35908), X64 RyuJIT


```
|        Method | ListSize |     Mean |     Error |    StdDev | Ratio |  Gen 0 | Allocated |
|-------------- |--------- |---------:|----------:|----------:|------:|-------:|----------:|
|      ListSort |       10 | 1.916 μs | 0.0037 μs | 0.0029 μs |  1.00 | 0.1602 |   1,016 B |
| ListQuickSort |       10 | 2.426 μs | 0.0101 μs | 0.0094 μs |  1.27 | 0.5493 |   3,461 B |
| ListMergeSort |       10 | 2.303 μs | 0.0177 μs | 0.0166 μs |  1.20 | 0.3357 |   2,126 B |
