let negateIsReversed sorting list =
    let negate x = -x

    let sortLast = list |> List.map negate |> sorting

    let sortFirst =
        list |> sorting |> List.map negate |> List.rev

    printfn "%b" (sortLast = sortFirst)


let rec quickSort (list: int list) =
    match list with
    | [] -> []
    | x :: xs ->
        let smaller = List.filter ((>) x) xs
        let bigger = List.filter ((<=) x) xs

        quickSort smaller @ (x :: quickSort bigger)



[ 382
  -550
  -798
  -798
  -798
  706
  21
  37
  1
  -798
  706
  21
  37
  706
  21
  37
  1
  1
  1
  -798
  706
  21
  37
  1
  -798
  706
  706
  21
  37
  1
  -798
  706
  21
  37
  706
  21
  37
  1
  -798
  706
  21
  37
  -798
  706
  21
  37
  1
  -798
  706
  706
  21
  37
  1
  -798
  706
  21
  37
  706
  21
  37
  1
  -798
  706
  21
  37
  -798
  706
  21
  37
  1
  -798
  706
  706
  21
  37
  1
  -798
  706
  21
  37
  706
  21
  37
  1
  -798
  706
  21
  37
  1
  -798
  706
  21
  37
  1
  -123 ]
|> quickSort
|> printfn "%A"
