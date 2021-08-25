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

let inline (>>=) fun1 fun2 = fun1 >> Option.bind fun2

let rec moveMaxToEnd comparison (didChange, zipper) =
    match zipper with
    | ListZipper (left, current, x :: xs) ->
        if comparison x current then
            moveMaxToEnd comparison (true, ListZipper(x :: left, current, xs))
        else
            moveMaxToEnd comparison (didChange, ListZipper(current :: left, x, xs))
    | zipper -> (didChange, zipper)

let rec moveMinToStart comparison (didChange, zipper) =
    match zipper with
    | ListZipper (x :: xs, current, right) ->
        if comparison current x then
            moveMinToStart comparison (true, ListZipper(xs, current, x :: right))
        else
            moveMinToStart comparison (didChange, ListZipper(xs, x, current :: right))
    | zipper -> (didChange, zipper)

let shakerSortWith comparison =
    let rec sortHelper (didChange, zipper) =
        if didChange then
            match moveMinToStart comparison (false, zipper) with
            | true, notSorted ->
                (false, notSorted)
                |> moveMaxToEnd comparison
                |> sortHelper
            | false, sorted -> toList sorted
        else
            toList zipper

    function
    | [] -> []
    | x :: xs ->
        let zipper = ListZipper([], x, xs)

        (false, zipper)
        |> moveMaxToEnd comparison
        |> sortHelper

let shakerSort list = shakerSortWith (<) list
let shakerSortDescending list = shakerSortWith (>) list

let shakerSortInt (list: int list) = shakerSortWith (<) list
let shakerSortDescendingInt (list: int list) = shakerSortWith (>) list
