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


let partition predicate (list: int list) =
    let rec partHelper pred lst pair =
        match lst with
        | [] -> pair
        | x :: xs ->
            let (left, right) = pair

            if pred x then
                partHelper pred xs (x :: left, right)
            else
                partHelper pred xs (left, x :: right)

    partHelper predicate list ([], [])

let apply2 f (x, y) = f x y

[ 382
  706
  100
  101
  21
  37
  1706
  100
  101
  21
  37
  1706
  100
  101
  21
  37
  1706
  100
  101
  21
  37
  1
  706
  100
  101
  21
  37
  1
  -123 ]
|> List.sortDescending
// |> quickSort
|> printfn "%A"
