module Heap

type ListInt =
    | Empty
    | NotEmpty of int * ListInt
