module ListZipper

type ListZipper<'a> = ListZipper of 'a list * 'a * 'a list

let focus (ListZipper (_, item, _)) = item

let moveRight (ListZipper (left, current, right)) =
    match right with
    | [] -> None
    | x :: xs -> ListZipper(current :: left, x, xs) |> Some

let moveLeft (ListZipper (left, current, right)) =
    match left with
    | [] -> None
    | x :: xs -> ListZipper(xs, x, current :: right) |> Some

let fromList =
    function
    | [] -> None
    | x :: xs -> ListZipper([], x, xs) |> Some

let toList (ListZipper (left, current, right)) =
    List.concat [ List.rev left
                  [ current ]
                  right ]

let isRight predicate =
    moveRight
    >> Option.map (focus >> predicate)
    >> Option.defaultValue false

let isLeft predicate =
    moveLeft
    >> Option.map (focus >> predicate)
    >> Option.defaultValue false

let isFocus predicate = focus >> predicate

let swapIfRight comparison (ListZipper (left, current, right)) =
    match right with
    | [] -> None
    | x :: xs ->
        if comparison current x then
            ListZipper(left, x, current :: xs)
        else
            ListZipper(left, current, right)
        |> Some

let swapWithRight (ListZipper (left, current, right)) =
    match right with
    | [] -> None
    | x :: xs -> ListZipper(left, x, current :: xs) |> Some

let swapWithLeft (ListZipper (left, current, right)) =
    match left with
    | [] -> None
    | x :: xs -> ListZipper(current :: xs, x, right) |> Some

let costam = [ 2; 1; 3; 7 ]

let zipper = fromList costam


let inline (>>=) fun1 fun2 = fun1 >> Option.bind fun2


let rec moveMaxToEnd =
    function
    | ListZipper (left, current, x :: xs) ->
        if current > x then
            ListZipper(x :: left, current, xs) |> moveMaxToEnd
        else
            ListZipper(current :: left, x, xs) |> moveMaxToEnd
    | zipper -> zipper

let rec moveMinToStart =
    function
    | ListZipper (x :: xs, current, right) ->
        if current < x then
            ListZipper(xs, current, x :: right)
            |> moveMinToStart
        else
            ListZipper(xs, x, current :: right)
            |> moveMinToStart
    | zipper -> zipper


let sort zipper =
    let current = focus zipper

    if isRight (fun right -> current <= right) zipper then
        swapWithRight zipper
    else
        Some zipper
